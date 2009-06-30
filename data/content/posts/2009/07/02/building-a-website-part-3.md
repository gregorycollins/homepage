| title: Building a website with Haskell, part 3
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2009-07-02T21:18:00-0400
| summary: Going dynamic: building a simple content management system

Hello! I'm back from a three-month posting hiatus. Together, the
[Haskell Platform](http://hackage.haskell.org/platform/) OSX installer
I've been working on and getting married have left me with precious
little leftover project time.

In the [last post](/posts/2009/03/30/building-a-website-part-2) in
this series, we discussed the design of this website, which was a
simple static [happstack](http://happstack.com/) app with some
dynamically-generated content.

In this post, I'll describe the new simple content management system
(code-named "Blaaargh"[^1]) I'm now using to power the site.

[^1]: short for "weblaaargh", of course, and chosen for its zombie feel


### Features & Requirements ###

I don't need too many fancy features for this personal site. I don't
have much use for admin consoles or post editors; I'm most comfortable
doing my editing in [Emacs](http://www.gnu.org/software/emacs/), and
working with flat files. What I do need:

* the ability to take a collection of marked-up text
  (pages/posts/articles) in a "content area" on the filesystem and
  have it published in HTML format using templates. (I chose
  [markdown](http://daringfireball.net/projects/markdown/) format on
  the basis of its ubiquity and the very convenient
  [pandoc](http://johnmacfarlane.net/pandoc/) library)

* the ability to syndicate an RSS/Atom feed for posts in a
  subdirectory, while excluding parts of the tree (static files, etc).

* templatable, via a tree of cascading templates corresponding to the
  content area files -- for example, requesting
  `content/foo/bar/baz.md` would cause us to search, in order:
      * `templates/foo/bar/baz.st`
      * `templates/foo/bar/post.st`
      * `templates/foo/post.st`
      * `templates/post.st`

* the ability to deal with static files

* requesting a directory will serve an "index template" instead, if it
  exists

* reads configurable parameters (site title, base URL, hostname,
  directories to exclude from the feed, etc.) from a conf file.


### Taking a look at the code... ###

You can check out my github page to take a look at the [current source
for this website](http://github.com/gregorycollins/homepage/tree), as
well as the [source for
Blaaargh](http://github.com/gregorycollins/blaaargh/tree), both of
which are a bit of a mess right now.

In a few days I'll write another post about the basic design of
Blaaargh (and clean up the code a little!).
