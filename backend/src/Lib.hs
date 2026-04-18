{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib
    ( startApp
    , generate
    ) where

import Data.Aeson
import Servant
import Servant.Elm
import Elm

import Common
import DB.Main


port :: Int
port = 8080


type API = "AllMessage" :> Post '[JSON] [Message]
           :<|> "PostMessage" :> ReqBody '[JSON] Message :> Post '[JSON] ()

server :: Server API
server = getAllMessage :<|> postMessage


-- RunServer

startApp :: IO ()
startApp =
  runServerWithCors port (Proxy :: Proxy API) $ server

-- generate API in Elm

generate :: IO ()
generate =
  generateElm port deList (Proxy :: Proxy API) "Api"
  where
    deList = [DefineElm (Proxy :: Proxy Message)]

