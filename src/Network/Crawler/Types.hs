{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Network.Crawler.Types
    ( runCrawler
    , Crawler
    , CrawlEnv(..)
    , CrawlState(..)
    , FormParam(..)
    , FormValue
    , Postable(..)
    , URL
    ) where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Network.HTTP.Client
import           Text.XML.Cursor            (Cursor)

type URL = String

data CrawlEnv = CrawlEnv
    { baseUrl :: URL
    , manager :: Manager
    }

mkCrawlEnv :: URL -> Manager -> CrawlEnv
mkCrawlEnv = CrawlEnv

data CrawlState = CrawlState
    { url     :: URL
    , body    :: LB.ByteString
    , cookies :: CookieJar
    , cursor  :: Maybe Cursor
    }

instance Show CrawlState where
    show (CrawlState{..}) =
        concat
            [ "CrawlState { "
            , "url = ", show url
            , ", body = ", show $ LB.take 256 body
            , " }"
            ]

mkCrawlState :: CrawlState
mkCrawlState = CrawlState
    { url = ""
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

runCrawler :: URL -> ManagerSettings -> Crawler a -> IO a
runCrawler url ms crw = do
    mgr <- newManager ms
    let env = mkCrawlEnv url mgr
    evalStateT (runReaderT (unCrawler crw) env) mkCrawlState

data FormParam where
    (:=) :: (FormValue v) => S.ByteString -> v -> FormParam

class FormValue v where
    renderFormValue :: v -> S.ByteString

instance FormValue T.Text where
    renderFormValue = T.encodeUtf8

class Postable p where
    postPayload :: p -> Request -> Request

instance Postable [FormParam] where
    postPayload ps = postPayload (map f ps) where
        f (a := b) = (a, renderFormValue b)

instance Postable [(S.ByteString, S.ByteString)] where
    postPayload = urlEncodedBody
