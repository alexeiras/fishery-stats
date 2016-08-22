{-# LANGUAGE RecordWildCards #-}
module Main where

import           Data.Time.Calendar  (Day (..))
import           FisheryStats
import           Options.Applicative

data DateBoundary = DateBoundary
    { from :: Day
    , to   :: Day
    }

dates :: Parser DateBoundary
dates = DateBoundary
    <$> option (str >>= parseDate)
        ( long "from"
        <> metavar "%d/%m/%Y"
        <> help "date from which to extract data" )
    <*> option (str >>= parseDate)
        ( long "to"
        <> metavar "%d/%m/%Y"
        <> help "date up to which extract data" )

main :: IO ()
main = execParser opts >>= run
    where
        opts = info (helper <*> dates)
            ( fullDesc
            <> progDesc "Extract fishery markets sales records from http://www.pescadegalicia.gal"
            <> header "fishery-stats - extract fishery markets data" )

run :: DateBoundary -> IO ()
run DateBoundary{..} = crawl from to
