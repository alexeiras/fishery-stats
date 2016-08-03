{-# LANGUAGE OverloadedStrings #-}
module FisheryStats where

import           Control.Monad
import           Data.Text       (Text)
import           Network.Crawler
import           Text.XML.Cursor (Axis, Cursor, attribute, attributeIs, child,
                                  content, element, fromDocument, ($//), (&|),
                                  (>=>))

type Species = (Text, Text)
type Name = Text

baseUrl :: URL
baseUrl = "http://pescadegalicia.com/estadisticas"

crawl :: IO ()
crawl = runCrawler baseUrl defaultManagerSettings $ do
    visit "/datos_estadisticas.asp?pub=1"
    visit "/agrupados/amm_menu_e.asp"
    species <- options $ selectField "SeleEspecies"
    forM species $ \s -> do
        submit "/agrupados/amm_menu_e.asp" ["SeleEspecies" := fst s]
        markets <- options $ selectField "SeleLonjas"
        return ()

    return ()

selectField :: Name -> Axis
selectField name = element "select" >=> attributeIs "name" name

options :: Axis -> Crawler [Species]
options select =
    withCursor $ \c ->
        let opts = select >=> child >=> element "option"
            ids = concat $ c $// opts &| attribute "value"
            names = concat $ c $// opts >=> child &| content
         in return $ zip ids names
