{-# LANGUAGE QuasiQuotes #-}

module Html.Grid where

import GameOfLife
import Text.Hamlet

renderGrid :: Grid -> Html
renderGrid grid =
  let w = fromWidth $ width grid
      h = fromHeight $ height grid
      allCoords = (\row col -> Coord (Row row, Col col)) <$> [0 .. h - 1] <*> [0 .. w - 1]
      coordId (Coord (Row r, Col c)) = show r <> "-" <> show c
   in [shamlet|
    <div id="grid" class="bg-black gap-px border border-black inline-grid" style="grid-template-columns: repeat(#{w}, minmax(0, 10px));">
      $forall coord@(Coord (Row row, Col col)) <- allCoords
        <div data-on:click="@post('/place')" data-on:mouseover="$selectedCoord = {row: #{row}, col: #{col}}" id="#{coordId coord}" class="aspect-square" :not (isOccupied grid coord):class="bg-white" :isOccupied grid coord:class="bg-black">
    |]