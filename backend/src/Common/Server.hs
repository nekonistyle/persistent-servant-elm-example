-- general
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
-- servant
{-# LANGUAGE DataKinds                  #-}

module Common.Server
    ( runServer
    , runServerWithCors
    ) where

-- servant
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
-- CORS
import Network.Wai.Middleware.Cors


-- Server
runServer :: HasServer api '[] => Int -> Proxy api -> Server api -> IO ()
runServer port proxy server =
  run port $ serve proxy server

--CORS
runServerWithCors :: HasServer api '[] => Int -> Proxy api -> Server api -> IO ()
runServerWithCors port proxy server =
  run port $ cors (const $ Just policy) $ serve proxy server
  where
    policy :: CorsResourcePolicy
    policy = simpleCorsResourcePolicy
             { corsRequestHeaders = [ "content-type" ] }

