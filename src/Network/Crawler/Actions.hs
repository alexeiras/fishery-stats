{-# LANGUAGE OverloadedStrings #-}
module Network.Crawler.Actions
    ( visit
    , submit
    , withCursor
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe
import           Network.Crawler.Types
import           Network.HTTP.Client
import           Text.HTML.DOM          (parseLBS)
import           Text.XML.Cursor        (Cursor, fromDocument)

visit :: URL -> Crawler ()
visit u = do
    env <- ask
    req <- parseUrl $ baseUrl env ++ u
    res <- liftIO $ httpLbs req (manager env)
    modify
        (\s ->
              s
              { url     = u
              , body    = responseBody res
              , cookies = responseCookieJar res
              , cursor  = Nothing
              })

submit :: (Postable a) => URL -> a -> Crawler ()
submit act ps = do
    env <- ask
    req <- parseUrl $ baseUrl env ++ act
    let req' = postPayload ps req
    res <- liftIO $ httpLbs req' (manager env)
    modify
        (\s ->
              s
              { url     = act
              , body    = responseBody res
              , cookies = responseCookieJar res
              , cursor  = Nothing
              })

withCursor :: (Cursor -> Crawler a) -> Crawler a
withCursor f = do
    st <- get
    let c = fromMaybe (fromDocument $ parseLBS $ body st) $ cursor st
      in do
        modify (\s -> s { cursor = Just c })
        f c
