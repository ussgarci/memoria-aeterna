{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified MyLib (someFunc)
import Web.Scotty
import Lucid


cdnImports :: Html ()
cdnImports = do 
  link_ [ rel_ "stylesheet"
        , href_ "https://cdn.jsdelivr.net/npm/daisyui@5" 
        , type_ "text/css"
        ]
  script_ [ src_ "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4" ] 
          ("" :: Html ()) -- script tag requires inner content or empty string in Lucid

homepage :: String -> Html ()
homepage name = 
  html_
    (do head_
          (do title_ "Introduction page."
              cdnImports)
        body_
          (do div_ [id_ "header", style_ "color:white"] "Syntax"
              p_ (span_ (strong_ "This is an example of Lucid syntax."))
              p_ (span_ (strong_ (toHtml name)))
              button_ [class_ "btn btn-error"] "Wow!"
              hr_ []
              ul_ (mapM_ (li_ . toHtml . show)
                         ([1, 2, 3] :: [Int])) -- Kept type annotation to prevent ambiguity
              table_ (tr_ (do td_ "Hello!"
                              td_ [class_ "alt"] "World!"
                              td_ "Sup?"))))


main :: IO ()
main = scotty 3000 $
  get "/:word" $ do
    beam <- pathParam "word"
    html $ renderText (homepage beam)
    -- html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]