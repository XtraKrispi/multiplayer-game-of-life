{-# LANGUAGE QuasiQuotes #-}

module Html.Layout where

import Text.Hamlet (Html, shamlet)

layout :: Html -> Html
layout content =
  [shamlet|
    $doctype 5
    <html>
      <head>
        <link rel=stylesheet href=/app.css>
      <body>
        <div class="p-4">
          ^{content}
        <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.4/bundles/datastar.js">
  |]