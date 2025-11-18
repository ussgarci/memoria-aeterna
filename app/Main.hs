{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Lucid
import Htmx.Lucid.Core
import Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev

    get "/" $ do
        html $ renderText home


home :: Html ()
home = do
    doctype_
    html_ [lang_ "en"] $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ "Memōria Aeterna"
            script_ [src_ "https://cdn.tailwindcss.com"] T.empty

        body_ [class_ "min-h-screen bg-gray-50 flex items-center justify-center"] $ do
            div_ [class_ "text-center p-10 bg-white shadow-xl rounded-lg"] $ do
                h1_ [class_ "text-5xl font-extrabold text-blue-600 mb-4"] "Salvē Mundus!"
                p_ [class_ "text-xl text-gray-700"] "Stāt Crūx dum volvitur orbis."