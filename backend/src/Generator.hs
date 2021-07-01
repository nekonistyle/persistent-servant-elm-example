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



module Generator
    ( DirOpts
    , defDirOpts
    , generateElm
    , generateElmWith
    ) where

import Data.Proxy
import Data.String.Conversions (cs)
import Data.Text (append)
import Servant.Elm
import Elm
import Shelly


data DirOpts = DirOpts
  { srcDir :: FilePath
  , generatedDir :: FilePath
  }

defDirOpts :: DirOpts
defDirOpts = DirOpts
             { srcDir = "../frontend"
             , generatedDir = "Generated"
             }


elmOpts :: Int -> ElmOptions
elmOpts port =
  defElmOptions
  { urlPrefix = Static (append "http://localhost:" (cs (show port))) }


--mkSpec :: ( omited ) => Text -> Int -> Proxy dat -> Proxy api -> Text -> Spec
--mkSpec gDir port proxyData proxyApi moduleName =
--  Spec [gDir, moduleName]
--  (defElmImports
--    : toElmTypeSource proxyData
--    : toElmDecoderSource proxyData
--    : toElmEncoderSource proxyData
--    : generateElmForAPIWith (elmOpts port) proxyApi
--  )

--mkSpec :: ( omited ) => Text -> Int -> Proxy dat -> Proxy api -> Text -> Spec
--mkSpec gDir port proxyData proxyApi moduleName =

--generateElmModuleWith :: ( HasForeign LangElm EType api, GenerateList Etype (Foreign EType api)) => ElmOptions -> Namespace -> Text -> FilePath -> [DefineElm] -> Proxy api -> IO ()
-- NameSpace = [String]



--generate :: ( omited ) => Int -> Proxy dat -> Proxy api -> Text -> IO ()
generateElm port proxyData proxyApi moduleName =
  generateElmWith defDirOpts port proxyData proxyApi moduleName

-- generateElmWith :: ( omited ) => DirOpts -> Int -> Proxy dat -> Proxy api -> Text -> IO ()
generateElmWith dirOpts port proxyData proxyApi moduleName = do
  shelly $ mkdir_p $ srcdir </> generateddir
  generateElmModuleWith opts [generateddir, moduleName] defElmImports srcdir
    [DefineElm proxyData] proxyApi
  where
    opts :: ElmOptions
    opts = elmOpts port

    srcdir :: FilePath
    srcdir = srcDir dirOpts

    generateddir :: FilePath
    generateddir = generatedDir dirOpts
--  specsToDir [mkSpec generateddir port proxyData proxyApi moduleName] elmdir
--  where


-- ( omited ) = ( HasForeign LangElm EType api, GenerateList EType (Foreign EType api ))

