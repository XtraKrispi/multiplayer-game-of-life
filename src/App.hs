{-# LANGUAGE DeriveAnyClass #-}

module App where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM (TChan, TVar, atomically, dupTChan, modifyTVar, modifyTVar', newBroadcastTChanIO, newTChanIO, newTVarIO, readTChan, readTVarIO, writeTChan, writeTVar)
import Control.Monad (forever)
import Data.Aeson (FromJSON)
import Data.KnownPatterns (Patterns, loadPatterns, searchForPattern, toPattern)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import GHC.Generics (Generic)
import GameOfLife (Col (..), Coord (..), Grid, Row (..), add, mkGrid, step, translate)
import Html.Grid (renderGrid)
import Html.Home (autoButton)
import Html.Home qualified as Home
import Html.Layout (layout)
import Hypermedia.Datastar (patchElements, readSignals, sendPatchElements, sseResponse, stderrLogger)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (
  addBase,
  staticPolicy,
 )
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans (
  ScottyT,
  defaultOptions,
  get,
  html,
  liftIO,
  middleware,
  nested,
  post,
  request,
  scottyAppT,
 )

data Handles = Handles
  { gridStorage :: TVar Grid
  , autoThread :: TVar (Maybe ThreadId)
  , autoChannel :: TChan ()
  , broadcastChannel :: TChan FeedMessage
  }

runApp :: IO ()
runApp = do
  patterns <- loadPatterns "src/Data/input.dat"
  let grid = mkGrid 50 50 $ Coord <$> [(10, 10), (10, 11), (10, 12)]
  handles <-
    Handles
      <$> newTVarIO grid
      <*> newTVarIO Nothing
      <*> newTChanIO
      <*> newBroadcastTChanIO
  waiApp <- scottyAppT defaultOptions id (app patterns handles)

  -- Start automated by default
  initialThread <- createAutoThread handles.gridStorage handles.broadcastChannel
  atomically $ writeTVar handles.autoThread (Just initialThread)
  _ <- forkIO $ autoLoop handles

  run 8080 waiApp

autoLoop :: Handles -> IO b
autoLoop handles = do
  forever do
    _ <- atomically $ readTChan handles.autoChannel
    mCurrentThread <- readTVarIO handles.autoThread
    case mCurrentThread of
      Just threadId -> do
        killThread threadId
        atomically $ writeTVar handles.autoThread Nothing
      Nothing -> do
        newThread <- createAutoThread handles.gridStorage handles.broadcastChannel
        atomically $ writeTVar handles.autoThread (Just newThread)
    -- Tell the feed that we need to update our button
    atomically $ writeTChan handles.broadcastChannel UpdateAuto

createAutoThread :: TVar Grid -> TChan FeedMessage -> IO ThreadId
createAutoThread gridStorage broadcastChannel =
  forkIO do
    forever do
      atomically do
        modifyTVar gridStorage step
        writeTChan broadcastChannel UpdateGrid
      threadDelay 2000000

data SelectSignals = SelectSignals
  { selectedPattern :: Text
  , selectedCoord :: Coord
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

data FeedMessage = UpdateGrid | UpdateAuto

app :: Patterns -> Handles -> ScottyT IO ()
app patterns handles = do
  middleware $ staticPolicy (addBase "public")
  get "/" do
    g <- liftIO $ readTVarIO handles.gridStorage
    mThread <- liftIO $ readTVarIO handles.autoThread
    html $ renderHtml $ layout $ Home.page (isJust mThread) patterns g
  post "/reset" do
    liftIO $ atomically do
      writeTVar handles.gridStorage $ mkGrid 50 50 $ Coord <$> [(10, 10), (10, 11), (10, 12)]
      writeTChan handles.broadcastChannel UpdateGrid
  post "/step" do
    liftIO $ atomically do
      modifyTVar handles.gridStorage step
      writeTChan handles.broadcastChannel UpdateGrid
  post "/place" do
    req <- request
    eSignals :: Either String SelectSignals <- liftIO $ readSignals req
    case eSignals of
      Right (SelectSignals selectedPattern (Coord (Row r, Col c))) -> do
        let mPattern = searchForPattern patterns selectedPattern
        case mPattern of
          Just pattern -> do
            let actualPattern = translate c r $ toPattern pattern
            liftIO $ atomically do
              modifyTVar' handles.gridStorage (add actualPattern)
              writeTChan handles.broadcastChannel UpdateGrid
          _ -> pure ()
      _ -> pure ()
  post "/auto" do
    liftIO $ atomically $ writeTChan handles.autoChannel ()
  get "/feed" do
    chan <- liftIO $ atomically $ dupTChan handles.broadcastChannel
    nested \_ respond -> do
      respond $ sseResponse stderrLogger \gen ->
        forever do
          msg <- atomically $ readTChan chan
          case msg of
            UpdateGrid -> do
              g <- readTVarIO handles.gridStorage
              sendPatchElements gen (patchElements $ LT.toStrict $ renderHtml $ renderGrid g)
            UpdateAuto -> do
              mThread <- readTVarIO handles.autoThread
              sendPatchElements gen (patchElements $ LT.toStrict $ renderHtml $ autoButton (isJust mThread))
