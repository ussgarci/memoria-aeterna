{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lucid
import Lucid.Base (term)
import qualified MyLib (someFunc)
import Web.Scotty

cdnImports :: Html ()
cdnImports = do
    link_
        [ rel_ "stylesheet"
        , href_ "https://cdn.jsdelivr.net/npm/daisyui@5"
        , type_ "text/css"
        ]
    link_
        [ rel_ "stylesheet"
        , href_ "https://cdn.jsdelivr.net/npm/daisyui@5/themes.css"
        , type_ "text/css"
        ]
    script_
        [src_ "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"]
        ("" :: Html ())
    script_
        [src_ "https://cdn.jsdelivr.net/npm/htmx.org@2.0.10/dist/htmx.min.js"]
        ("" :: Html ())

navbar :: Html ()
navbar =
    nav_ [class_ "navbar justify-between bg-base-300"] $ do
        a_ [class_ "btn btn-ghost text-lg"] $ do
            "Memōria Aeterna"

hero :: Html ()
hero =
    div_ [class_ "flex justify-center"] $ do
        div_ [class_ "flex flex-col items-center text-center gap-6 max-w-xl"] $ do
            h1_ [class_ "text-5xl font-bold"] "Memōria Aeterna"
            span_ [class_ ""] "Memōria Aeterna: quia mala memoria mihi est"
            div_ [class_ "flex gap-4"] $ do
                a_ [class_ "btn btn-primary", term "hx-post" "/submit", term "hx-trigger" "click"] "Incipiamus"

-- i_ [class_ "fa-solid fa-arrow-right text-sm"] ("" :: Html())

domus :: Html ()
domus = do
    doctype_
    html_ [data_ "theme" "retro"] $ do
        head_ $ do
            title_ "Memōria Aeterna"
            cdnImports
        body_ $ do
            navbar
            hero

main :: IO ()
main =
    scotty 3000 $
        get "/" $
            html $
                renderText domus
