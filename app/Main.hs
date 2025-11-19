{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Web.Scotty
import Lucid
import Htmx.Lucid.Core
import Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Directory (listDirectory)
import Data.Aeson (FromJSON, decode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Control.Monad (forM)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

data Document = Document
    { titulus :: T.Text
    } deriving (Show, Generic, FromJSON)

generateListItems :: IO (Html ())
generateListItems = do
    let dir = "data"
    files <- listDirectory dir

    let jsonFiles = Prelude.filter (\f -> ".json" `T.isSuffixOf` (T.pack f)) files
    listItems <- forM jsonFiles $ \file -> do
        let filePath = dir <> "/" <> file
        content <- B.readFile filePath
        
        case decode content of
            Just doc -> do
                let textContent = titulus doc
                return $ li_ [class_ "hover:text-blue-500 hover:underline cursor-pointer"] (toHtml textContent)
            Nothing -> do
                putStrLn $ "Error decoding JSON from file: " ++ filePath
                return $ li_ [class_ "text-red-500"] (toHtml $ "Error loading " <> T.pack file)

    return $ mconcat listItems

lucid :: Html () -> ActionM ()
lucid = html . renderText

home :: IO (Html ())
home = do
    listItems <- generateListItems
    return $ do
        doctype_
        html_ [lang_ "en"] $ do
            head_ $ do
                meta_ [charset_ "utf-8"]
                title_ "Memōria Aeterna"
                script_ [src_ "https://cdn.tailwindcss.com"] T.empty
                script_ [src_ "https://unpkg.com/htmx.org@1.9.10"] T.empty

            body_ [class_ "min-h-screen bg-gray-50 flex items-center justify-center"] $ do
                div_ [class_ "text-center p-10 bg-white shadow-xl rounded-lg"] $ do
                    h1_ [class_ "text-5xl font-extrabold text-blue-600 mb-4"] "Salvē!"
                    p_ [class_ "text-sm text-gray-500 mb-6 italic"] "Ad incipiendum, ēlige capitulum ex indice infrā."
                    
                    -- The dynamic list
                    ul_ [class_ "list-none p-0 space-y-2 text-lg text-gray-800"] $
                        listItems

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    get "/" $ do
        page <- liftIO home
        lucid page
