module Network.Crawler
    ( Crawler()
    , withCrawler
    , defaultManagerSettings
    , URL
    , FormParam(..)
    , FormValue(..)
    -- * Actions
    , visit
    , submit
    , withCursor
    ) where

import           Network.Crawler.Actions
import           Network.Crawler.Types
import           Network.HTTP.Client     (defaultManagerSettings)
