{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Homepage.Util.Delicious
(
  getRecent
, DiffPost(..)
) where

import qualified Control.Exception as Ex
import Control.Concurrent.MVar
import Control.Monad.State.Strict

import Data.Char (isSpace)
import Data.Maybe
import Data.Time

import System.Locale

import Text.StringTemplate()
import Text.StringTemplate.Classes

import qualified Data.Map as Map
import qualified Network.Delicious.JSON as D
import qualified Network.Delicious.Types as D

import Homepage.Types

myDeliciousUserName :: String
myDeliciousUserName = "how.gauche"

type Age = String

-- | a DiffPost is a delicious post plus an age string, e.g. '2 hours
-- | ago'; we need to interpret the post in the context of the current
-- | time in order to compute the age
data DiffPost = DiffPost !D.Post !Age


agePost :: TimeZone -> UTCTime -> D.Post -> DiffPost
agePost tz now post = DiffPost post s
  where
    dt = parseDeliciousTime $ D.postStamp post
    s  = humanReadableTimeDiff tz now dt


getRecentPosts :: MVar DeliciousState -> IO [D.Post]
getRecentPosts mvar = do
    now   <- getCurrentTime
    empty <- isEmptyMVar mvar
    if empty then do
        posts <- getRecentPosts'
        tryPutMVar mvar $! DeliciousState posts now
        return posts
      else do
        modifyMVar mvar $! \oldstate@(DeliciousState oldposts oldtime) -> do
                       if tooOld now oldtime then do
                           posts <- getRecentPosts'
                           let newstate = DeliciousState (posts `seq` posts) now
                           return $! (newstate `seq` newstate, posts `seq` posts)
                         else
                           return $! (oldstate, oldposts)
  where
    tooOld :: UTCTime -> UTCTime -> Bool
    tooOld now old = diffUTCTime now old > 60 * 60 * 4


getRecentPosts' :: IO [D.Post]
getRecentPosts' = do
  posts <- Ex.handle (\(_::Ex.SomeException) -> return [])
                     (D.runDelic D.nullUser
                           "http://feeds.delicious.com/v2/json" $
                           D.getUserBookmarks myDeliciousUserName)
  return $ take 5 posts


instance ToSElem DiffPost where
    toSElem (DiffPost (D.Post href _ desc notes tags _ _) age) =
        SM $! Map.fromList [ ("date",    toSElem age)
                           , ("title",   toSElem desc)
                           , ("summary", toSElem notes)
                           , ("href",    toSElem href)
                           , ("tags",    toSElem tags) ]



parseDeliciousTime :: String -> UTCTime
parseDeliciousTime = fromJust . parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"


humanReadableTimeDiff :: TimeZone    -- ^ our timezone
                      -> UTCTime     -- ^ current time
                      -> UTCTime     -- ^ old time
                      -> String
humanReadableTimeDiff tz curTime oldTime =
    helper diff
  where
    diff    = diffUTCTime curTime oldTime

    minutes :: NominalDiffTime -> Double
    minutes n = realToFrac $ n / 60

    hours :: NominalDiffTime -> Double
    hours   n = (minutes n) / 60

    days :: NominalDiffTime -> Double
    days    n = (hours n) / 24

    weeks :: NominalDiffTime -> Double
    weeks   n = (days n) / 7

    years :: NominalDiffTime -> Double
    years   n = (days n) / 365

    i2s :: RealFrac a => a -> String
    i2s !n = show m
      where
        m :: Int
        m = truncate n

    old = utcToLocalTime tz oldTime

    trim = f . f
      where f = reverse . dropWhile isSpace

    dow           = trim $! formatTime defaultTimeLocale "%l:%M %p on %A" old
    thisYear      = trim $! formatTime defaultTimeLocale "%b %e" old
    previousYears = trim $! formatTime defaultTimeLocale "%b %e, %Y" old

    helper !d | d < 1          = "one second ago"
              | d < 60         = i2s d ++ " seconds ago"
              | minutes d < 2  = "one minute ago"
              | minutes d < 60 = i2s (minutes d) ++ " minutes ago"
              | hours d < 2    = "one hour ago"
              | hours d < 24   = i2s (hours d) ++ " hours ago"
              | days d < 5     = dow
              | days d < 10    = i2s (days d)  ++ " days ago"
              | weeks d < 2    = i2s (weeks d) ++ " week ago"
              | weeks d < 5    = i2s (weeks d)  ++ " weeks ago"
              | years d < 1    = thisYear
              | otherwise      = previousYears


getRecent :: HomepageMonad [DiffPost]
getRecent = do
    delMVar <- get >>= return . homepageDeliciousMVar
    now     <- liftIO $ getCurrentTime
    tz      <- liftIO $ getCurrentTimeZone

    liftIO $ getRecentPosts delMVar >>=
           return . map (agePost tz now)
