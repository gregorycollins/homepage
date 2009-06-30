| title: Building a website with Haskell, part 2
| author: Gregory Collins <greg@gregorycollins.net>
| updated: 2009-03-30T16:32:00-0500
| summary: In the second part of the series, we discuss the design of
|          this <a href="http://www.happstack.com/">happstack</a>
|          website.

In the [last post](/posts/2009/03/28/building-a-website-part-1) in
this series, I discussed why I chose
[happstack](http://happstack.com/) to power this website. In this
post, I'll describe its design. If you'd like to follow along, you can
browse the source at [my github
page](http://github.com/gregorycollins/homepage/tree/v0.2).


### Types ###

Let's begin by looking at the
[`Homepage.Types`](http://github.com/gregorycollins/homepage/blob/v0.2/src/Homepage/Types.hs)
module, which contains types and functions pertaining to the global
website state; here "global" means "state that is shared by all
requests".

The datatype that holds the state for the site is called
`HomepageState`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
data HomepageState = HomepageState {
      homepageDeliciousMVar :: MVar  DeliciousState
    , homepageTemplateMVar  :: IORef TemplateDirs
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It holds the contents of my [delicious](http://delicious.com/) feed
(which we'll discuss later), and the
[HStringTemplate](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/HStringTemplate)
templates for the site. The delicious state is wrapped in an
[`MVar`](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html);
an `MVar` is a Haskell concurrency primitive that provides synchronized
access to its underlying type. We use this to enforce mutual
exclusion when accessing delicious, so that we don't contact it more
than once at a time.

The homepage state will, of course, be used by most of the URL
handlers for the website. In order to provide easy access to this
value so that we don't have to pass it around everywhere, we'll use a
standard Haskell trick and wrap it in a [reader
monad](http://www.haskell.org/ghc/docs/latest/html/libraries/mtl/Control-Monad-Reader.html):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
type HomepageMonad = ReaderT HomepageState IO
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Terminology like "reader monad" tends to frighten off newcomers but
really all this is doing is allowing us to write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
foo :: HomepageMonad
foo = do
    homepageState <- ask
    ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We inject a `HomepageState` value into the monad when we evaluate it,
and from then on we can chain monadic actions together without having
to explicitly pass the state around. The "`ReaderT`" type is a "monad
transformer"; that's another $2 term that just means that it wraps an
existing monad, producing a new monad that does everything the wrapped
monad does, plus carrying some state around. In this case the wrapped
monad is "`IO`."

(A parenthetical: v0.1 used `StateT` here, which is clearly wrong.)

The next type definition covers the type of our URL handlers, or
"server parts":

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
type HomepageHandler = ServerPartT HomepageMonad Response
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Happstack URL Handlers ###

Happstack URL handlers have the weird type [`ServerPartT m
a`](http://happstack.com/docs/0.2/happstack-server/0.2/Happstack-Server-SimpleHTTP.html#2)
-- this is just a wrapper around the monad transformer type:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
ReaderT Request (WebT m) a
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `ReaderT Request` part here just means that you can get at the
HTTP request with `ask`, and `WebT` is itself a monad transformer
which takes a monad you give it and wraps its result to be of the
form:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
(Maybe (Either Response a, FilterFun Response))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a little abstruse, and the happstack docs don't do a great job
of explaining it, which makes it tough to understand at first for
n00bs like me with Master's degrees in type theory. Some things you
can do with a `WebT`:

- short-circuit the computation with "`mzero`", which is represented
  by a return value of `Nothing`. This is what allows you to chain
  handlers together. Happstack will try your handlers in order, and
  the first handler that returns a non-`Nothing` value will be used to
  satisfy the request. The `mzero` value is `WebT`'s way of saying "I
  choose not to handle this."

- short-circuit the computation with "`finishWith response`", which is
  represented by a return value of `Just (Left response, ...)`. This
  allows you to bomb out early with a response, ignoring any
  subsequent chained monadic actions.

- add a filtering function to the handler with `setFilter` or
  `composeFilter`, which corresponds to a return type of `Just (...,
  f)`, where `f` is (roughly) a function of type `Response ->
  Response`. Ignore the baffling `SetAppend (Dual (Endo a))` type;
  that's just there to dictate how filter functions get bolted
  together by their `Monoid` instance.

- return a value of arbitrary type "`a`", just like any other monad --
  that corresponds to a return type of `Just (Right v, ...)`.


Happstack handlers belong to the `Monoid` typeclass. In case you
didn't do a math degree, a `Monoid` is a set with an associative
binary operator, usually called "⊕", and a zero element. In order for
a set to be a monoid when it grows up, it needs to satisfy the "monoid
laws":

<div class="indented">
<table>
<tr><td style="padding-right:2em">1\. `a` ⊕ (`b` ⊕ `c`) = (`a` ⊕ `b`) ⊕ `c`</td><td>*(associativity)*</td></tr>
<tr><td>2\. 0 ⊕ `a` = `a`</td><td>*(left identity)*</td></tr>
<tr><td>3\. `a` ⊕ 0 = `a`</td><td>*(right identity)*</td></tr>
</table>
</div>

In Haskell, perplexingly, "⊕" is called "`mappend`" and "0" is called
"`mempty`". (This nomenclature comes from the monoid instance for
lists.) The `ServerPartT` and `WebT` types have monoid definitions
that allow you to chain handlers together. For instance, the
expression:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
(exactdir "/foo" fooHandler) `mappend` (exactdir "/bar" barHandler)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

will cause `fooHandler` to be invoked if you request `"/foo"`, and
`barHandler` will be invoked if you request `"/bar"`. (Note that
`exactdir` comes from
[happstack-helpers](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/happstack-helpers).)


### Site behaviour ###

Let's take a look now at the
[`Homepage.Handlers`](http://github.com/gregorycollins/homepage/blob/v0.2/src/Homepage/Handlers.hs)
module, which defines the "toplevel" handler for the website:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
topLevelHandler :: HomepageHandler
topLevelHandler =
      frontpage             `mappend`
        aboutpage           `mappend`
        contactpage         `mappend`
        (liftH staticfiles) `mappend`
        temporaryPosts      `mappend`
        fourohfour
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each one of those handlers corresponds to a specific page or set of
pages on the website. For instance, the `aboutpage` handler looks like
this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
aboutpage :: HomepageHandler
aboutpage =
    exactdir "/about" $ do
      serveTemplate' "." "about" (setAttribute "whichCss"
                                               ("posts" :: String))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It reads like it's written: if the client requests the `"/about"`
page, we serve the
[`about.st`](http://github.com/gregorycollins/homepage/blob/v0.2/templates/about.st)
template, using the
[`posts.css`](http://github.com/gregorycollins/homepage/blob/v0.2/static/css/posts.css)
stylesheet. I won't go into `HStringTemplate` here, it's a pretty
typical templating engine; there's some material on it in the
[happstack tutorial](http://tutorial.happstack.com/).


### Something (slightly) non-trivial: integrating a delicious feed ###

I use [delicious](http://delicious.com/) a lot, to record bookmarks
I'm interested in across all of the computers I use. Since *of course*
I only read turbo-interesting stuff that other people would be
interested in, something I wanted to put on my front page was a
syndication of the last few entries from this feed. Luckily Haskell
already has a
[library](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/delicious)
to handle this; all we need to do is provide some plumbing. (It would
be very easy to cook up something similar for
[twitter](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hs-twitter),
assuming you can find any useful purpose for that dreck.)

First let's take a look at the `DeliciousState` type:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
data DeliciousState = DeliciousState ![D.Post] !UTCTime
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To be nice to delicious, we'll only pull my recent bookmarks once
every four hours. `DeliciousState` is just the recent posts on the
feed (the `D.Post` type comes from the Haskell delicious library) plus
the timestamp of the last time we retrieved the feed.

The `frontpage` handler reads as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
frontpage =
    exactdir "/" $ do
      bookmarks <- lift Delicious.getRecent
      serveTemplate' "." "home" (setAttribute "recentBookmarks" bookmarks .
                                 setAttribute "whichCss" ("home" :: String))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Again, it reads like it looks; we get the recent bookmarks and slop
them into the `"home"` template. Let's take a look at
[`Delicious.getRecent`](http://github.com/gregorycollins/homepage/blob/v0.2/src/Homepage/Util/Delicious.hs):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
getRecent :: HomepageMonad [DiffPost]
getRecent = do
    delMVar <- get >>= return . homepageDeliciousMVar
    now     <- liftIO $ getCurrentTime
    tz      <- liftIO $ getCurrentTimeZone

    liftIO $ getRecentPosts delMVar >>=
           return . map (agePost tz now)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

What's this `DiffPost` type?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
type Age = String

data DiffPost = DiffPost !D.Post !Age
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I wanted to use relative time to present the bookmarks (e.g. "2 hours
ago"). A `DiffPost` is just a delicious post plus a string containing
the human-readable timestamp. The reason we can just splat the posts
into the template directly is that we've told `HStringTemplate` how to
encode it:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
instance ToSElem DiffPost where
    toSElem (DiffPost (D.Post href _ desc notes tags _ _) age) =
        SM $! Map.fromList [ ("date",    toSElem age)
                           , ("title",   toSElem desc)
                           , ("summary", toSElem notes)
                           , ("href",    toSElem href)
                           , ("tags",    toSElem tags) ]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

So `getRecent` just grabs the `DeliciousState` out of the
`HomepageState`, passes it to `getRecentPosts`, and computes
human-readable ages for the results. The `getRecentPosts` function is
only a little bit hairier:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the `MVar` is empty then haven't yet pulled the feed and we should
do so. If the `MVar` contains data, we check whether the last update
was less than four hours ago, and if it was we just return the old
posts. Otherwise, we fetch the feed and update the timestamp.


### Next in this series ###

If you look at the
[source](http://github.com/gregorycollins/homepage/) for this website,
it's clear that these posts are hard-coded as static content. In Part
3 of this series, we'll create a simple dynamic content system which
will allow us to drop posts on the filesystem, in
[markdown](http://daringfireball.net/projects/markdown/) format, and
have them be published to the site.
