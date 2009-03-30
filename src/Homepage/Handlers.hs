{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, FlexibleContexts #-}

-- | This module contains the top-level handler for the website.

module Homepage.Handlers (topLevelHandler) where

import Control.Monad.State.Strict

import Data.List
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


tempPost1 :: HomepageHandler
tempPost1 = prefixdir "/posts/2009/03/28/building-a-website-part-1" $ do
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

    serveTemplate' "." "post" (setManyAttrib attrs)


tempPost2 :: HomepageHandler
tempPost2 = prefixdir "/posts/2009/03/30/building-a-website-part-2" $ do
    postContent <- lift $ (getTemplate "." "temppost2") >>=
                            (return . B.unpack . render)

    let attrs :: [(String,String)]
        attrs = [ ("websiteTitleExtra",
                   ": Building a website with Haskell, part 2")
                , ("whichCss",    "posts")
                , ("postContent", postContent)
                , ("postTitle",   "Building a website with Haskell, part 2")
                , ("postSummary", "In the second part of the series, we \
                                  \discuss the design of this <a href=\"\
                                  \http://www.happstack.com/\">happstack</a> \
                                  \website.")
                , ("postDate",    "march 30, 2009") ]

    serveTemplate' "." "post" (setManyAttrib attrs)



temporaryPosts :: HomepageHandler
temporaryPosts = tempPost1 `mappend` tempPost2


prefixdir :: (Monad m) => String -> ServerPartT m a -> ServerPartT m a
prefixdir staticPath sps = do
    rq <- askRq
    if staticPath `isPrefixOf` (rqURL rq) then sps else mzero


fourohfour :: HomepageHandler
fourohfour = serveTemplate' "." "404" (setAttribute "whichCss"
                                      ("posts" :: String))


-- N.B. "fileServeStrict" here is like normal "fileServe" from
-- happstack 0.2.1, except modified to consume the file strictly
-- (avoiding handle leaks). You'll need the darcs truck version of
-- happstack to run this.
staticfiles :: WebHandler
staticfiles = staticserve "static"
  where staticserve d = dir d (fileServeStrict [] d)
