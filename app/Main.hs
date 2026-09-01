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
  link_ [ rel_ "stylesheet"
        , href_ "https://cdn.jsdelivr.net/npm/daisyui@5/themes.css" 
        , type_ "text/css"
        ]
  script_ [ src_ "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4" ] 
          ("" :: Html ()) -- script tag requires inner content or empty string in Lucid

-- homepage :: String -> Html ()
-- homepage name = 
--   html_
--     (do head_
--           (do title_ "Introduction page."
--               cdnImports)
--         body_
--           (do div_ [id_ "header", style_ "color:white"] "Syntax"
--               p_ (span_ (strong_ "This is an example of Lucid syntax."))
--               p_ (span_ (strong_ (toHtml name)))
--               button_ [class_ "btn btn-error"] "Wow!"
--               hr_ []
--               ul_ (mapM_ (li_ . toHtml . show)
--                          ([1, 2, 3] :: [Int])) 
--               table_ (tr_ (do td_ "Hello!"
--                               td_ [class_ "alt"] "World!"
--                               td_ "Sup?"))))

navbar :: Html ()
navbar = nav_ [class_ "navbar justify-between bg-base-300"] $ do
    
    a_ [class_ "btn btn-ghost text-lg"] $ do
        "Memōria Aeterna"

-- app/Main.hs:44:3: error:
--     • Couldn't match expected type: HtmlT
--                                       Data.Functor.Identity.Identity a1
--                   with actual type: HtmlT m0 a0 -> HtmlT m0 a0
--     • Probable cause: ‘doctypehtml_’ is applied to too few arguments
--       In a stmt of a 'do' block: doctypehtml_
--       In the expression:
--         do doctypehtml_
--            html_ [data_ "theme" "retro"]
--              $ do head_ $ do ...
--                   body_ $ do ...
--       In an equation for ‘domus’:
--           domus
--             = do doctypehtml_
--                  html_ [data_ "theme" "retro"]
--                    $ do head_ $ ...
--                         ....
--    |
-- 44 |   doctypehtml_ 
-- domus :: Html ()
-- domus = do
--   doctypehtml_ 
--   html_ [data_ "theme" "retro"] $ do
--       head_ $ do
--           title_ "Memōria Aeterna"
--           cdnImports
--       body_  $ do
--           navbar

domus :: Html ()
domus = do
  doctype_ 
  html_ [data_ "theme" "retro"] $ do
      head_ $ do
          title_ "Memōria Aeterna"
          cdnImports
      body_  $ do
          navbar

main :: IO ()
main = scotty 3000 $
  get "/" $ 
    html $ renderText domus