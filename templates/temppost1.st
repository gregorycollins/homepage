<p
>I&rsquo;ve been meaning for a long time to write more about the projects I&rsquo;m working on. I&rsquo;ve had this web space for ages and have never really done anything useful with it, and I&rsquo;ve decided the time has come to change that.</p
><h3 id="why-haskell"
>Why Haskell?</h3
><p
>Lately most of my personal project time has been spent hacking on <a href="http://www.haskell.org/"
  >Haskell</a
  > code. I&rsquo;m a strong proponent of functional programming: my years of research and study in computer science and in programming have lead me to the conclusion that functional programs are simpler, shorter, more likely to be correct, and quicker to write than programs written in standard industry languages like Java or C++.</p
><p
>I did most of my graduate work in <a href="http://www.smlnj.org/"
  >Standard ML</a
  >. Back then (2001&ndash;2003), Haskell was still pretty immature, not very fast, and even more &ldquo;boutique&rdquo; than it is now. ML, &ldquo;Standard&rdquo; and <a href="http://caml.inria.fr/ocaml/"
  >otherwise</a
  > had several industrial-strength compilers, reasonable standard libraries, and a small but dedicated user community.</p
><p
>The tables have turned: Haskell has improved so much in the past six years, it&rsquo;s uncanny. The <a href="http://www.haskell.org/ghc/"
  >GHC</a
  > compiler is getting acceptably close to C&rsquo;s performance (between 2X&ndash;3X in my experience), libraries are proliferating, the user community is thriving. Much progress has been made on a lot of the old issues (e.g.: &ldquo;space leaks everywhere&rdquo;). Haskell is definitely my choice for the current &ldquo;best of breed&rdquo; functional language.</p
><p
>So when choosing the &ldquo;technology stack&rdquo; for this website, I decided that I would write the backend in Haskell. In a pathetic attempt at evangelism, I&rsquo;m also releasing the code on <a href="http://github.com/gregorycollins/homepage/tree/v0.1"
  >my github page</a
  > as a tutorial in the hope that it might help other people to make their own happstack websites, or at least see an example of a functioning (albeit simple) one &ldquo;in the wild.&rdquo;</p
><h3 id="haskell-web-frameworks"
>Haskell web frameworks</h3
><p
>A quick survey of <a href="http://hackage.haskell.org/"
  >Hackage</a
  > yields a bunch of web toolkits, of varying levels of immaturity. My main requirements are:</p
><ol style="list-style-type: decimal;"
><li
  >simple, flexible, easy to use</li
  ><li
  >fast, preferably supporting <a href="http://www.haskell.org/ghc/docs/latest/html/libraries/bytestring/Data-ByteString.html"
    >bytestrings</a
    >.</li
  ><li
  >doesn&rsquo;t gobble or leak memory</li
  ></ol
><p
>After a bit of research I settled on <a href="http://happstack.com/"
  >happstack</a
  >. Happstack is still under-documented and a little over-complicated in parts, and I won&rsquo;t be trusting my data to its &ldquo;MACID&rdquo; system anytime soon; but it supports bytestrings, it&rsquo;s modular enough to be used in mix-and-match fashion, and it&rsquo;s quick: on my last-gen Macbook Pro, it serves up static files about half as fast as Apache. This is pretty impressive given that Apache uses <code
  >sendfile()</code
  > to serve static files, avoiding a couple of copies and context switches in the process. For an equivalent dynamic program I suspect the race might be a little closer.</p
><h3 id="to-be-continued"
>To be continued&hellip;</h3
><p
>This website is running on happstack now (with static content), reverse-proxied through apache. Although happstack&rsquo;s documentation is sketchy, the tutorial contained enough detail to get me going. After spending some time to grok the code and writing a few combinators, I think I&rsquo;ve come up with some code that&rsquo;s a little bit cleaner than the tutorial&rsquo;s. And in case you were wondering &ldquo;is it fast?&rdquo;, here&rsquo;s the result of an <code
  >httperf</code
  > run (Macbook Pro, 2.4 GHz Core 2 Duo) for the home page:</p
><pre
><code
  >Connection rate: 584.8 conn/s (1.7 ms/conn, &lt;=1 concurrent connections)
Connection time [ms]: min 0.4 avg 1.7 max 15.1 median 1.5 stddev 0.6
</code
  ></pre
><p
>If I turn gzip compression on, I get nearly 3000 conn/s &mdash; but I had to turn it off because it looks like there&rsquo;s a bug serving static files.</p
><p
>My next post in this series will outline the software design behind this basic happstack website and demonstrate how simple it was to integrate the recent bookmarks from my <a href="http://www.delicious.com/"
  >delicious</a
  > feed. In part three, we&rsquo;ll bootstrap a simple content management system, with all (some) of the usual amenities: RSS feeds, archives, markdown, etc. If you&rsquo;re curious to read the source code now, you can browse it at <a href="http://github.com/gregorycollins/homepage/tree/v0.1"
  >my github page</a
  >.</p
>
