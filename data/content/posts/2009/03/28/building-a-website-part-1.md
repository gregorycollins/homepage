| title: Building a website with Haskell, part 1
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2009-03-28T15:31:00-0500
| updated: 2009-03-28T15:31:00-0500
| summary: Using the <a href="http://www.happstack.com/">happstack</a>
|          web framework to power a simple personal website.

I've been meaning for a long time to write more about the projects I'm
working on. I've had this web space for ages and have never really
done anything useful with it, and I've decided the time has come to
change that.

### Why Haskell? ###
Lately most of my personal project time has been spent hacking on
[Haskell](http://www.haskell.org/) code.  I'm a strong proponent of
functional programming: my years of research and study in computer
science and in programming have lead me to the conclusion that
functional programs are simpler, shorter, more likely to be correct,
and quicker to write than programs written in standard industry
languages like Java or C++.

I did most of my graduate work in [Standard
ML](http://www.smlnj.org/). Back then (2001-2003), Haskell was still
pretty immature, not very fast, and even more "boutique" than it is
now. ML, "Standard" and [otherwise](http://caml.inria.fr/ocaml/) had
several industrial-strength compilers, reasonable standard libraries,
and a small but dedicated user community.

The tables have turned: Haskell has improved so much in the past six
years, it's uncanny. The [GHC](http://www.haskell.org/ghc/) compiler
is getting acceptably close to C's performance (between 2X-3X in my
experience), libraries are proliferating, the user community is
thriving. Much progress has been made on a lot of the old issues
(e.g.: "space leaks everywhere").  Haskell is definitely my choice for
the current "best of breed" functional language.

So when choosing the "technology stack" for this website, I decided
that I would write the backend in Haskell. In a pathetic attempt at
evangelism, I'm also releasing the code on [my github
page](http://github.com/gregorycollins/homepage/tree/v0.1) as a
tutorial in the hope that it might help other people to make their own
happstack websites, or at least see an example of a functioning
(albeit simple) one "in the wild."


### Haskell web frameworks ###

A quick survey of [Hackage](http://hackage.haskell.org/) yields a
bunch of web toolkits, of varying levels of immaturity. My main
requirements are:

1. simple, flexible, easy to use
2. fast, preferably supporting
   [bytestrings](http://www.haskell.org/ghc/docs/latest/html/libraries/bytestring/Data-ByteString.html).
3. doesn't gobble or leak memory

After a bit of research I settled on
[happstack](http://happstack.com/). Happstack is still
under-documented and a little over-complicated in parts, and I won't
be trusting my data to its "MACID" system anytime soon; but it
supports bytestrings, it's modular enough to be used in mix-and-match
fashion, and it's quick: on my last-gen Macbook Pro, it serves up
static files about half as fast as Apache.  This is pretty impressive
given that Apache uses `sendfile()` to serve static files, avoiding a
couple of copies and context switches in the process. For an
equivalent dynamic program I suspect the race might be a little
closer.


### To be continued... ###

This website is running on happstack now (with static content),
reverse-proxied through apache. Although happstack's documentation is
sketchy, the tutorial contained enough detail to get me going. After
spending some time to grok the code and writing a few combinators, I
think I've come up with some code that's a little bit cleaner than the
tutorial's. And in case you were wondering "is it fast?", here's the
result of an `httperf` run (Macbook Pro, 2.4 GHz Core 2 Duo) for the
home page:

    Connection rate: 584.8 conn/s (1.7 ms/conn, <=1 concurrent connections)
    Connection time [ms]: min 0.4 avg 1.7 max 15.1 median 1.5 stddev 0.6

If I turn gzip compression on, I get nearly 3000 conn/s -- but I had
to turn it off because it looks like there's a bug serving static
files.

My [next post](/posts/2009/03/30/building-a-website-part-2) in this
series will outline the software design behind this basic happstack
website and demonstrate how simple it was to integrate the recent
bookmarks from my [delicious](http://www.delicious.com/) feed. In part
three, we'll bootstrap a simple content management system, with all
(some) of the usual amenities: RSS feeds, archives, markdown, etc. If
you're curious to read the source code now, you can browse it at [my
github page](http://github.com/gregorycollins/homepage/tree/v0.1).
