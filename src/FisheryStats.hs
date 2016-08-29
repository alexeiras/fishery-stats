{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FisheryStats where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as S
import           Data.Char
import           Data.List              (intersect)
import qualified Data.Text              as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Database.Redis
import           Network.Crawler
import           Text.XML.Cursor

baseUrl :: URL
baseUrl = "http://www.pescadegalicia.gal"

dbConn :: IO Connection
dbConn = connect defaultConnectInfo { connectHost = "192.168.99.100" }

data Params = Params
    { from    :: Day
    , to      :: Day
    , species :: [String]
    }

crawl :: Params -> IO ()
crawl Params{..} = withCrawler defaultManagerSettings baseUrl $ do
    visit "/estadisticas.html"
    visit "/estadisticas/datos_estadisticas.asp?pub=1"
    let dates = map renderFormValue [from .. to]
    allSpecies <- fetchSpecies
    let species' = case species of
            [] -> allSpecies
            _  -> allSpecies `intersect` map renderFormValue species
    forM_ species' $
        \s -> do
            markets <- fetchMarkets s
            let params = sequence [[s], markets, dates]
            mapM_ (fetchSales >=> saveData) params

fetchSpecies :: Crawler [S.ByteString]
fetchSpecies = do
    visit "/estadisticas/agrupados/amm_menu_e.asp"
    options $ selectField "SeleEspecies"

fetchMarkets :: (FormValue v) => v -> Crawler [S.ByteString]
fetchMarkets s = do
    submit "/estadisticas/agrupados/amm_menu_e.asp" ["SeleEspecies" := s]
    options $ selectField "SeleLonjas"

fetchSales :: [S.ByteString] -> Crawler [S.ByteString]
fetchSales ps@[s,m,d] = do
    submit "/estadisticas/agrupados/cadenasdatos.asp"
        [ "SeleEspecies" := s
        , "SeleLonjas"   := m
        , "Del"          := d
        , "Al"           := d
        ]
    liftM (ps ++) scrapeData

scrapeData :: Crawler [S.ByteString]
scrapeData =
    withCursor $
        \c -> do
            let scrape =
                    element "body"              &/
                    element "table"             >=>
                    attributeIs "class" "tabla" &//
                    element "tr"                &//
                    element "td"                >=>
                    hasAttribute "nowrap"       &//
                    content
                record = filter (not . T.null) $ map T.strip $ c $// scrape
             in case length record of
                28 -> return $ map renderFormValue (record !! 7 : drop 20 record)
                _  -> return []

saveData :: [S.ByteString] -> Crawler ()
saveData r
    -- ["POL","101","19/5/2012","A Coruña (Lonja Coruña, S.A.)","Abadexo","Pollachius pollachius","2012","132,50","480,86","1,00","7,20","3,63"]
    | length r == 12 = do
          conn <- liftIO dbConn
          date <- parseDate $ toString $ r !! 2
          liftIO $ runRedis conn $ do
              sadd "species" $ [S.intercalate ":" [r !! 0, r !! 4, r !! 5]]
              sadd "markets" $ [S.intercalate ":" [r !! 1, r !! 3]]
              zadd (S.intercalate ":" ["sales", r !! 0, r !! 1])
                   [( realToFrac $ utcTimeToPOSIXSeconds date
                    , S.intercalate ":"
                      [r !! 2, r !! 7, r !! 8, r !! 9, r !! 10, r !! 11])]
          return ()
    | otherwise = return ()

selectField :: T.Text -> Axis
selectField name = element "select" >=> attributeIs "name" name

options :: Axis -> Crawler [S.ByteString]
options select = withCursor $ \c ->
    return $ map renderFormValue $ concat $ c $// opts
        where
            opts = select >=> child >=> element "option" &| attribute "value"

parseDate :: (Monad m, ParseTime t) => String -> m t
parseDate = parseTimeM False defaultTimeLocale "%-d/%-m/%Y"

toString :: S.ByteString -> String
toString bs = map (chr . fromIntegral) (S.unpack bs)
