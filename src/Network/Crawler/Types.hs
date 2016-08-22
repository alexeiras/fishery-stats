{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Network.Crawler.Types
    ( withCrawler
    , Crawler()
    , CrawlEnv(..)
    , CrawlState(..)
    , FormParam(..)
    , FormValue(..)
    , Postable(..)
    , URL
    ) where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString           as S
import qualified Data.ByteString.Lazy      as LB
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.Time
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Text.XML.Cursor           (Cursor)

type URL = String

data CrawlEnv = CrawlEnv
    { manager :: Manager
    , baseUrl :: URL
    }

mkCrawlEnv :: Manager -> URL -> CrawlEnv
mkCrawlEnv = CrawlEnv

data CrawlState = CrawlState
    { url     :: URL
    , status  :: Status
    , body    :: LB.ByteString
    , cookies :: CookieJar
    , cursor  :: Maybe Cursor
    }

instance Show CrawlState where
    show (CrawlState{..}) =
        concat
            [ "CrawlState { "
            , "url = ", show url
            , ", status = ", show status
            , ", body = ", show body
            , ", cookies = ", show cookies
            , " }"
            ]

mkCrawlState :: CrawlState
mkCrawlState = CrawlState
    { url = ""
    , status = mkStatus 0 ""
    , body = ""
    , cookies = createCookieJar []
    , cursor = Nothing
    }

newtype Crawler a = Crawler
    { unCrawler :: ReaderT CrawlEnv (StateT CrawlState IO) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader CrawlEnv
               , MonadState CrawlState
               , MonadThrow
               )

withCrawler :: ManagerSettings -> URL -> Crawler a -> IO a
withCrawler ms url cr = do
    mgr <- newManager ms
    let env = mkCrawlEnv mgr url
    evalStateT (runReaderT (unCrawler cr) env) mkCrawlState

data FormParam where
    (:=) :: (FormValue v) => S.ByteString -> v -> FormParam

instance Show FormParam where
    show (a := b) = show a ++ " := " ++ show (renderFormValue b)

class FormValue v where
    renderFormValue :: v -> S.ByteString

instance FormValue S.ByteString where
    renderFormValue = id

instance FormValue T.Text where
    renderFormValue = T.encodeUtf8

instance FormValue String where
    renderFormValue = T.encodeUtf8 . T.pack

instance FormValue Day where
    renderFormValue = renderFormValue . formatTime defaultTimeLocale "%-d/%-m/%Y"

class Postable p where
    postPayload :: p -> Request -> Request

instance Postable [FormParam] where
    postPayload ps = postPayload (map f ps) where
        f (a := b) = (a, renderFormValue b)

instance Postable [(S.ByteString, S.ByteString)] where
    postPayload = urlEncodedBody
