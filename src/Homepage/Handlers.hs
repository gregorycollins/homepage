{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, FlexibleContexts #-}

-- | This module contains the top-level handler for the website.

module Homepage.Handlers (topLevelHandler) where

import Control.Monad.State.Strict

import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as B

import Happstack.Helpers
import Happstack.Server
--import Happstack.Server.Parts

import Homepage.Types
import Homepage.Util.Templates
import Homepage.Util.Delicious as Delicious

import Text.StringTemplate


topLevelHandler :: HomepageHandler
topLevelHandler =
-- gzip currently doesn't work. augh
--
--    do
--      compressedResponseFilter
      frontpage             `mappend`
        aboutpage           `mappend`
        contactpage         `mappend`
        (liftH staticfiles) `mappend`
        temporaryPosts      `mappend`
        fourohfour


frontpage :: HomepageHandler
frontpage =
    exactdir "/" $ do
      bookmarks <- lift Delicious.getRecent
      serveTemplate' "." "home" (setAttribute "recentBookmarks" bookmarks .
                                 setAttribute "whichCss" ("home" :: String))

aboutpage :: HomepageHandler
aboutpage =
    exactdir "/about" $ do
      serveTemplate' "." "about" (setAttribute "whichCss"
                                               ("posts" :: String))


contactpage :: HomepageHandler
contactpage =
    exactdir "/contact" $ do
      serveTemplate' "." "contact" (setAttribute "whichCss"
                                   ("posts" :: String))


temporaryPosts :: HomepageHandler
temporaryPosts = do
    postContent <- lift $ (getTemplate "." "temppost1") >>=
                            (return . B.unpack . render)

    let attrs :: [(String,String)]
        attrs = [ ("websiteTitleExtra",
                   ": Building a website with Haskell, part 1")
                , ("whichCss",    "posts")
                , ("postContent", postContent)
                , ("postTitle",   "Building a website with Haskell, part 1")
                , ("postSummary", "Using the <a href=\"\
                                  \http://www.happstack.com/\">happstack</a> \
                                  \web framework to power a simple personal \
                                  \website.")
                , ("postDate",    "march 28, 2009") ]

    exactdir "/posts/2009/03/28/building-a-website-part-1" $
      serveTemplate' "." "post" (setManyAttrib attrs)


fourohfour :: HomepageHandler
fourohfour = serveTemplate' "." "404" (setAttribute "whichCss"
                                      ("posts" :: String))


-- N.B. "fileServeStrict" here is like normal "fileServe" from
-- happstack 0.2.1, except modified to consume the file strictly
-- (avoiding handle leaks)
staticfiles :: WebHandler
staticfiles = staticserve "static"
  where staticserve d = dir d (fileServeStrict [] d)
