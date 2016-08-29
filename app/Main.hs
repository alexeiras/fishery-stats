module Main where

import           Data.Time.Calendar  (Day (..))
import           FisheryStats
import           Options.Applicative

params :: Parser Params
params = Params
    <$> option (str >>= parseDate)
        ( long "from"
        <> short 'f'
        <> metavar "%d/%m/%Y"
        <> help "date from which to extract data" )
    <*> option (str >>= parseDate)
        ( long "to"
        <> short 't'
        <> metavar "%d/%m/%Y"
        <> help "date up to which extract data" )
    <*> many
        ( strOption
          ( long "species"
          <> short 's'
          <> help "species identifier(s)" ))

main :: IO ()
main = execParser opts >>= crawl
    where
        opts = info (helper <*> params)
            ( fullDesc
            <> progDesc "Extract fishery markets sales records from http://www.pescadegalicia.gal"
            <> header "fishery-stats - extract fishery markets data" )
