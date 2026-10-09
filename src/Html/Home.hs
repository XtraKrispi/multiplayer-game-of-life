{-# LANGUAGE QuasiQuotes #-}

module Html.Home where

import Data.KnownPatterns
import GameOfLife (Grid)
import Html.Grid (renderGrid)
import Numeric (showFFloat)
import Text.Hamlet

autoButton :: Bool -> Html
autoButton isAuto =
  [shamlet|
        <button id="auto-button" class="px-2 py-1 border rounded-md border-gray-600" data-on:click="@post('/auto')">
            <span>Turn 
            <span>
              $if isAuto
                off
              $else 
                on
            <span> auto|]

delay :: Int -> Html
delay d =
  let s :: Double = fromIntegral d / 1000000
   in [shamlet|<span id="delay">#{showFFloat (Just 2) s ""}s|]

page :: Bool -> Int -> Patterns -> Grid -> Html
page isAuto d patterns grid =
  [shamlet|
    <div data-init="@get('/feed')" class="flex flex-col gap-2">
      <div class="border border-gray-400 p-2 rounded-md flex flex-col gap-2">
        <div class="flex gap-2">
          <button class="px-2 py-1 border rounded-md border-gray-600" data-on:click="@post('/reset')">Reset
          <button class="px-2 py-1 border rounded-md border-gray-600" data-on:click="@post('/step')">Step
          ^{autoButton isAuto}
        <div>
          <button class="px-2 py-1 border rounded-md border-gray-600" data-on:click="@post('/speed-up')"><<
          ^{delay d}
          <button class="px-2 py-1 border rounded-md border-gray-600" data-on:click="@post('/slow-down')">>>

      <div class="flex flex-col gap-2">
        $forall (sectionName, sectionPatterns) <- sections patterns
          <div class="border border-gray-400 p-2 rounded-md flex flex-col gap-2">
            <div>#{sectionName}
            <div class="flex gap-2">
                $forall patternName <- sectionPatterns
                  $with mPattern <- searchForPattern patterns patternName 
                    $maybe pattern <- mPattern
                      <div data-on:click="$selectedPattern = '#{patternName}'" data-class="{'bg-black': $selectedPattern == '#{patternName}'}" class="border border-gray-400 rounded-md p-2 inline-block cursor-pointer">
                        <div class="flex flex-col gap-2 items-center">
                          <div data-class="{'text-white': $selectedPattern == '#{patternName}'}">#{patternName}
                          <div class="bg-black gap-px border border-black inline-grid" style="grid-template-columns: repeat(#{pattern.width}, minmax(0, 10px));">
                            $forall (occupied, _) <- pattern.coords
                              <div class="aspect-square" :not occupied:class="bg-white" :occupied:class="bg-black">
      <div class="border border-gray-400 rounded-md p-2">
        ^{renderGrid grid}
  |]
