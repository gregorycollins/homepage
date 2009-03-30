{-# LANGUAGE OverloadedStrings, UnboxedTuples, BangPatterns #-}

-- | This module contains types (and a couple of functions) pertaining
-- | to the website's global state, plus some synonyms
module Homepage.Types where

import Control.Concurrent.MVar
import Control.Monad.Reader

import Data.IORef

import qualified Network.Delicious as D
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Time

import Happstack.Server

import Text.StringTemplate
import Text.StringTemplate.Helpers


------------------------------------------------------------------------
-- * Type synonyms

-- | type synonyms for templates & template groups
type TemplateDirs  = STDirGroups B.ByteString
type TemplateGroup = STGroup B.ByteString
type Template      = StringTemplate B.ByteString


------------------------------------------------------------------------
-- * Homepage State

-- | In order to not spam delicious, we only pull my recent feeds once
-- | every four hours. So we need to keep the last posts and update
-- | time.
data DeliciousState = DeliciousState ![D.Post] !UTCTime


-- | We're going to keep the templates inside the homepage state. The
-- | templates are wrapped in an IORef because I'm planning on using
-- | inotify to handle template reloads
data HomepageState = HomepageState {
      homepageDeliciousMVar :: MVar  DeliciousState
    , homepageTemplateMVar  :: IORef TemplateDirs
}


-- | Create a homepage state object with new empty mvars
emptyHomepageState :: TemplateDirs -> IO HomepageState
emptyHomepageState td = do
  d <- newEmptyMVar
  t <- newIORef td
  return $! HomepageState d t


-- | We'll put the homepage state into a state monad so we don't have
-- | to pass it around everywhere
type HomepageMonad = ReaderT HomepageState IO

-- | Homepage handlers will have the following type
type HomepageHandler = ServerPartT HomepageMonad Response

-- | "standard" web handlers (like 'staticfiles') will have this type
type WebHandler      = ServerPartT IO Response

-- | ..so we'll need a function to lift a "WebHandler" into the
-- | "HomepageHandler"
liftH :: ServerPartT IO a -> ServerPartT HomepageMonad a
liftH = mapServerPartT liftIO


-- | this IO action initializes the homepage's state and returns a
-- | monad evaluator function 
-- |    runner :: HomepageMonad a -> IO a
-- | we'll pass this into simpleHTTP'.
initHomepage :: IO (HomepageMonad a -> IO a)
initHomepage = do
    
    s <- directoryGroups "templates" >>= emptyHomepageState

    return $! runHomepage s



runHomepage :: HomepageState -> HomepageMonad a -> IO a
runHomepage hps = flip runReaderT hps
    

------------------------------------------------------------------------
-- * odds and ends

-- | this instance should be in happstack already, it allows us to
-- | treat a bytestring as an HTML response
newtype BStoHTML = BStoHTML B.ByteString
instance ToMessage BStoHTML where
    toContentType _ = "text/html"
    toMessage (BStoHTML a) = a
