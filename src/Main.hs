{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Happstack.Server hiding (port)
--import Happstack.Server.FastCGI

import Homepage

import Control.Exception
import Prelude hiding (catch)
import System.Environment

-- import           System.Log.Handler.Simple
-- import           System.Log.Logger



main :: IO ()
main = do
    args <- getArgs
    case args of
      [port] -> startServer $ read port
      _      -> putStrLn "pass me a port number, please"



startServer :: Int -> IO ()
startServer port = do
    -- this IO action initializes the homepage's state and returns a
    -- monad evaluator function 
    -- 
    --    eval :: HomepageMonad a -> IO a
    -- 
    -- we'll pass this into simpleHTTP'. (the HTTP server expects to
    -- be given IO and needs a function to produce an IO action given
    -- our custom state monad.)
    getBlaaarghDir >>= initHomepage >>= go

  where
    go eval = simpleHTTP' eval (Conf port Nothing) topLevelHandler


getBlaaarghDir :: IO FilePath
getBlaaarghDir = getEnv "BLAAARGH_DIR" `catch`
                   \(_ :: SomeException) -> return "data"



