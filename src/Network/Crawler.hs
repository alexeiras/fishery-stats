module Network.Crawler
    ( Crawler
    , runCrawler
    , defaultManagerSettings
    , URL
    , FormParam(..)
    -- * Actions
    , visit
    , submit
    , withCursor
    ) where

import           Network.Crawler.Actions
import           Network.Crawler.Types
import           Network.HTTP.Client     (defaultManagerSettings)
