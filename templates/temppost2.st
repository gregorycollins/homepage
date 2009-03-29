<p
>In the <a href="/posts/2009/03/28/building-a-website-part-1"
  >last post</a
  > in this series, I discussed why I chose <a href="http://happstack.com/"
  >happstack</a
  > to power this website. In this post, I&rsquo;ll describe its design. If you&rsquo;d like to follow along, you can browse the source at <a href="http://github.com/gregorycollins/homepage/tree/v0.2"
  >my github page</a
  >.</p
><h3 id="types"
>Types</h3
><p
>Let&rsquo;s begin by looking at the <a href="http://github.com/gregorycollins/homepage/blob/v0.2/src/Homepage/Types.hs"
  ><code
    >Homepage.Types</code
    ></a
  > module, which contains types and functions pertaining to the global website state; here &ldquo;global&rdquo; means &ldquo;state that is shared by all requests&rdquo;.</p
><p
>The datatype that holds the state for the site is called <code
  >HomepageState</code
  >:</p
><pre class="sourceCode haskell"
><code
  ><span class="Keyword"
    >data</span
    ><span class="Normal NormalText"
    > HomepageState = HomepageState {</span
    ><br
     /><span class="Normal NormalText"
    >      </span
    ><span class="Function FunctionDefinition"
    >homepageDeliciousMVar ::</span
    ><span class="Normal NormalText"
    > MVar  DeliciousState</span
    ><br
     /><span class="Normal NormalText"
    >    , </span
    ><span class="Function FunctionDefinition"
    >homepageTemplateMVar  ::</span
    ><span class="Normal NormalText"
    > IORef TemplateDirs</span
    ><br
     /><span class="Normal NormalText"
    >}</span
    ><br
     /></code
  ></pre
><p
>It holds the contents of my <a href="http://delicious.com/"
  >delicious</a
  > feed (which we&rsquo;ll discuss later), and the <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/HStringTemplate"
  >HStringTemplate</a
  > templates for the site. The delicious state is wrapped in an <a href="http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html"
  ><code
    >MVar</code
    ></a
  >; an <code
  >MVar</code
  > is a Haskell concurrency primitive that provides synchronized access to its underlying type. We use this to enforce mutual exclusion when accessing delicious, so that we don&rsquo;t contact it more than once at a time.</p
><p
>The homepage state will, of course, be used by most of the URL handlers for the website. In order to provide easy access to this value so that we don&rsquo;t have to pass it around everywhere, we&rsquo;ll use a standard Haskell trick and wrap it in a <a href="http://www.haskell.org/ghc/docs/latest/html/libraries/mtl/Control-Monad-Reader.html"
  >reader monad</a
  >:</p
><pre class="sourceCode haskell"
><code
  ><span class="Keyword"
    >type</span
    ><span class="Normal NormalText"
    > HomepageMonad = ReaderT HomepageState </span
    ><span class="DataType TypeConstructor"
    >IO</span
    ><br
     /></code
  ></pre
><p
>Terminology like &ldquo;reader monad&rdquo; tends to frighten off newcomers but really all this is doing is allowing us to write:</p
><pre class="sourceCode haskell"
><code
  ><span class="Function FunctionDefinition"
    >foo ::</span
    ><span class="Normal NormalText"
    > HomepageMonad</span
    ><br
     /><span class="Normal NormalText"
    >foo = </span
    ><span class="Keyword"
    >do</span
    ><br
     /><span class="Normal NormalText"
    >    homepageState &lt;- ask</span
    ><br
     /><span class="Normal NormalText"
    >    ...</span
    ><br
     /></code
  ></pre
><p
>We inject a <code
  >HomepageState</code
  > value into the monad when we evaluate it, and from then on we can chain monadic actions together without having to explicitly pass the state around. The &ldquo;<code
  >ReaderT</code
  >&rdquo; type is a &ldquo;monad transformer&rdquo;; that&rsquo;s another \$2 term that just means that it wraps an existing monad, producing a new monad that does everything the wrapped monad does, plus carrying some state around. In this case the wrapped monad is &ldquo;<code
  >IO</code
  >.&rdquo;</p
><p
>(A parenthetical: v0.1 used <code
  >StateT</code
  > here, which is clearly wrong.)</p
><p
>The next type definition covers the type of our URL handlers, or &ldquo;server parts&rdquo;:</p
><pre class="sourceCode haskell"
><code
  ><span class="Keyword"
    >type</span
    ><span class="Normal NormalText"
    > HomepageHandler = ServerPartT HomepageMonad Response</span
    ><br
     /></code
  ></pre
><h3 id="happstack-url-handlers"
>Happstack URL Handlers</h3
><p
>Happstack URL handlers have the weird type <a href="http://happstack.com/docs/0.2/happstack-server/0.2/Happstack-Server-SimpleHTTP.html#2"
  ><code
    >ServerPartT m a</code
    ></a
  > &mdash; this is just a wrapper around the monad transformer type:</p
><pre class="sourceCode haskell"
><code
  ><span class="Normal NormalText"
    >ReaderT Request (WebT m) a</span
    ><br
     /></code
  ></pre
><p
>The <code
  >ReaderT Request</code
  > part here just means that you can get at the HTTP request with <code
  >ask</code
  >, and <code
  >WebT</code
  > is itself a monad transformer which takes a monad you give it and wraps its result to be of the form:</p
><pre class="sourceCode haskell"
><code
  ><span class="Normal NormalText"
    >(</span
    ><span class="DataType TypeConstructor"
    >Maybe</span
    ><span class="Normal NormalText"
    > (</span
    ><span class="DataType TypeConstructor"
    >Either</span
    ><span class="Normal NormalText"
    > Response a, FilterFun Response))</span
    ><br
     /></code
  ></pre
><p
>This is a little abstruse, and the happstack docs don&rsquo;t do a great job of explaining it, which makes it tough to understand at first for n00bs like me with Master&rsquo;s degrees in type theory. Some things you can do with a <code
  >WebT</code
  >:</p
><ul
><li
  ><p
    >short-circuit the computation with &ldquo;<code
      >mzero</code
      >&rdquo;, which is represented by a return value of <code
      >Nothing</code
      >. This is what allows you to chain handlers together. Happstack will try your handlers in order, and the first handler that returns a non-<code
      >Nothing</code
      > value will be used to satisfy the request. The <code
      >mzero</code
      > value is <code
      >WebT</code
      >&rsquo;s way of saying &ldquo;I choose not to handle this.&rdquo;</p
    ></li
  ><li
  ><p
    >short-circuit the computation with &ldquo;<code
      >finishWith response</code
      >&rdquo;, which is represented by a return value of <code
      >Just (Left response, ...)</code
      >. This allows you to bomb out early with a response, ignoring any subsequent chained monadic actions.</p
    ></li
  ><li
  ><p
    >add a filtering function to the handler with <code
      >setFilter</code
      > or <code
      >composeFilter</code
      >, which corresponds to a return type of <code
      >Just (...,   f)</code
      >, where <code
      >f</code
      > is (roughly) a function of type <code
      >Response -&gt;   Response</code
      >. Ignore the baffling <code
      >SetAppend (Dual (Endo a))</code
      > type; that&rsquo;s just there to dictate how filter functions get bolted together by their <code
      >Monoid</code
      > instance.</p
    ></li
  ><li
  ><p
    >return a value of arbitrary type &ldquo;<code
      >a</code
      >&rdquo;, just like any other monad &mdash; that corresponds to a return type of <code
      >Just (Right v, ...)</code
      >.</p
    ></li
  ></ul
><p
>Happstack handlers belong to the <code
  >Monoid</code
  > typeclass. In case you didn&rsquo;t do a math degree, a <code
  >Monoid</code
  > is a set with an associative binary operator, usually called &ldquo;&#8853;&rdquo;, and a zero element. In order for a set to be a monoid when it grows up, it needs to satisfy the &ldquo;monoid laws&rdquo;:</p
><div class="indented">
<table>
<tr><td style="padding-right:2em">1. <code
>a</code
> &#8853; (<code
>b</code
> &#8853; <code
>c</code
>) = (<code
>a</code
> &#8853; <code
>b</code
>) &#8853; <code
>c</code
></td><td><em
>(associativity)</em
></td></tr>
<tr><td>2. 0 &#8853; <code
>a</code
> = <code
>a</code
></td><td><em
>(left identity)</em
></td></tr>
<tr><td>3. <code
>a</code
> &#8853; 0 = <code
>a</code
></td><td><em
>(right identity)</em
></td></tr>
</table>
</div>
<p
>In Haskell, perplexingly, &ldquo;&#8853;&rdquo; is called &ldquo;<code
  >mappend</code
  >&rdquo; and &ldquo;0&rdquo; is called &ldquo;<code
  >mempty</code
  >&rdquo;. (This nomenclature comes from the monoid instance for lists.) The <code
  >ServerPartT</code
  > and <code
  >WebT</code
  > types have monoid definitions that allow you to chain handlers together. For instance, the expression:</p
><pre class="sourceCode haskell"
><code
  ><span class="Normal NormalText"
    >(exactdir </span
    ><span class="String"
    >&quot;/foo&quot;</span
    ><span class="Normal NormalText"
    > fooHandler) </span
    ><span class="Others InfixOperator"
    >`mappend`</span
    ><span class="Normal NormalText"
    > (exactdir </span
    ><span class="String"
    >&quot;/bar&quot;</span
    ><span class="Normal NormalText"
    > barHandler)</span
    ><br
     /></code
  ></pre
><p
>will cause <code
  >fooHandler</code
  > to be invoked if you request <code
  >&quot;/foo&quot;</code
  >, and <code
  >barHandler</code
  > will be invoked if you request <code
  >&quot;/bar&quot;</code
  >. (Note that <code
  >exactdir</code
  > comes from <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/happstack-helpers"
  >happstack-helpers</a
  >.)</p
><h3 id="site-behaviour"
>Site behaviour</h3
><p
>Let&rsquo;s take a look now at the <a href="http://github.com/gregorycollins/homepage/blob/v0.2/src/Homepage/Handlers.hs"
  ><code
    >Homepage.Handlers</code
    ></a
  > module, which defines the &ldquo;toplevel&rdquo; handler for the website:</p
><pre class="sourceCode haskell"
><code
  ><span class="Function FunctionDefinition"
    >topLevelHandler ::</span
    ><span class="Normal NormalText"
    > HomepageHandler</span
    ><br
     /><span class="Normal NormalText"
    >topLevelHandler =</span
    ><br
     /><span class="Normal NormalText"
    >      frontpage             </span
    ><span class="Others InfixOperator"
    >`mappend`</span
    ><br
     /><span class="Normal NormalText"
    >        aboutpage           </span
    ><span class="Others InfixOperator"
    >`mappend`</span
    ><br
     /><span class="Normal NormalText"
    >        contactpage         </span
    ><span class="Others InfixOperator"
    >`mappend`</span
    ><br
     /><span class="Normal NormalText"
    >        (liftH staticfiles) </span
    ><span class="Others InfixOperator"
    >`mappend`</span
    ><br
     /><span class="Normal NormalText"
    >        temporaryPosts      </span
    ><span class="Others InfixOperator"
    >`mappend`</span
    ><br
     /><span class="Normal NormalText"
    >        fourohfour</span
    ><br
     /></code
  ></pre
><p
>Each one of those handlers corresponds to a specific page or set of pages on the website. For instance, the <code
  >aboutpage</code
  > handler looks like this:</p
><pre class="sourceCode haskell"
><code
  ><span class="Function FunctionDefinition"
    >aboutpage ::</span
    ><span class="Normal NormalText"
    > HomepageHandler</span
    ><br
     /><span class="Normal NormalText"
    >aboutpage =</span
    ><br
     /><span class="Normal NormalText"
    >    exactdir </span
    ><span class="String"
    >&quot;/about&quot;</span
    ><span class="Normal NormalText"
    > \$ </span
    ><span class="Keyword"
    >do</span
    ><br
     /><span class="Normal NormalText"
    >      serveTemplate' </span
    ><span class="String"
    >&quot;.&quot;</span
    ><span class="Normal NormalText"
    > </span
    ><span class="String"
    >&quot;about&quot;</span
    ><span class="Normal NormalText"
    > (setAttribute </span
    ><span class="String"
    >&quot;whichCss&quot;</span
    ><br
     /><span class="Normal NormalText"
    >                                               (</span
    ><span class="String"
    >&quot;posts&quot;</span
    ><span class="Normal NormalText"
    > :: </span
    ><span class="DataType TypeConstructor"
    >String</span
    ><span class="Normal NormalText"
    >))</span
    ><br
     /></code
  ></pre
><p
>It reads like it&rsquo;s written: if the client requests the <code
  >&quot;/about&quot;</code
  > page, we serve the <a href="http://github.com/gregorycollins/homepage/blob/v0.2/templates/about.st"
  ><code
    >about.st</code
    ></a
  > template, using the <a href="http://github.com/gregorycollins/homepage/blob/v0.2/static/css/posts.css"
  ><code
    >posts.css</code
    ></a
  > stylesheet. I won&rsquo;t go into <code
  >HStringTemplate</code
  > here, it&rsquo;s a pretty typical templating engine; there&rsquo;s some material on it in the <a href="http://tutorial.happstack.com/"
  >happstack tutorial</a
  >.</p
><h3 id="something-slightly-non-trivial-integrating-a-delicious-feed"
>Something (slightly) non-trivial: integrating a delicious feed</h3
><p
>I use <a href="http://delicious.com/"
  >delicious</a
  > a lot, to record bookmarks I&rsquo;m interested in across all of the computers I use. Since <em
  >of course</em
  > I only read turbo-interesting stuff that other people would be interested in, something I wanted to put on my front page was a syndication of the last few entries from this feed. Luckily Haskell already has a <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/delicious"
  >library</a
  > to handle this; all we need to do is provide some plumbing. (It would be very easy to cook up something similar for <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hs-twitter"
  >twitter</a
  >, assuming you can find any useful purpose for that dreck.)</p
><p
>First let&rsquo;s take a look at the <code
  >DeliciousState</code
  > type:</p
><pre class="sourceCode haskell"
><code
  ><span class="Keyword"
    >data</span
    ><span class="Normal NormalText"
    > DeliciousState = DeliciousState ![</span
    ><span class="Normal ModuleName"
    >D.Post</span
    ><span class="Normal NormalText"
    >] !UTCTime</span
    ><br
     /></code
  ></pre
><p
>To be nice to delicious, we&rsquo;ll only pull my recent bookmarks once every four hours. <code
  >DeliciousState</code
  > is just the recent posts on the feed (the <code
  >D.Post</code
  > type comes from the Haskell delicious library) plus the timestamp of the last time we retrieved the feed.</p
><p
>The <code
  >frontpage</code
  > handler reads as follows:</p
><pre class="sourceCode haskell"
><code
  ><span class="Normal NormalText"
    >frontpage =</span
    ><br
     /><span class="Normal NormalText"
    >    exactdir </span
    ><span class="String"
    >&quot;/&quot;</span
    ><span class="Normal NormalText"
    > \$ </span
    ><span class="Keyword"
    >do</span
    ><br
     /><span class="Normal NormalText"
    >      bookmarks &lt;- lift Delicious.getRecent</span
    ><br
     /><span class="Normal NormalText"
    >      serveTemplate' </span
    ><span class="String"
    >&quot;.&quot;</span
    ><span class="Normal NormalText"
    > </span
    ><span class="String"
    >&quot;home&quot;</span
    ><span class="Normal NormalText"
    > (setAttribute </span
    ><span class="String"
    >&quot;recentBookmarks&quot;</span
    ><span class="Normal NormalText"
    > bookmarks .</span
    ><br
     /><span class="Normal NormalText"
    >                                 setAttribute </span
    ><span class="String"
    >&quot;whichCss&quot;</span
    ><span class="Normal NormalText"
    > (</span
    ><span class="String"
    >&quot;home&quot;</span
    ><span class="Normal NormalText"
    > :: </span
    ><span class="DataType TypeConstructor"
    >String</span
    ><span class="Normal NormalText"
    >))</span
    ><br
     /></code
  ></pre
><p
>Again, it reads like it looks; we get the recent bookmarks and slop them into the <code
  >&quot;home&quot;</code
  > template. Let&rsquo;s take a look at <a href="http://github.com/gregorycollins/homepage/blob/v0.2/src/Homepage/Util/Delicious.hs"
  ><code
    >Delicious.getRecent</code
    ></a
  >:</p
><pre class="sourceCode haskell"
><code
  ><span class="Function FunctionDefinition"
    >getRecent ::</span
    ><span class="Normal NormalText"
    > HomepageMonad [DiffPost]</span
    ><br
     /><span class="Normal NormalText"
    >getRecent = </span
    ><span class="Keyword"
    >do</span
    ><br
     /><span class="Normal NormalText"
    >    delMVar &lt;- get &gt;&gt;= </span
    ><span class="Function"
    >return</span
    ><span class="Normal NormalText"
    > . homepageDeliciousMVar</span
    ><br
     /><span class="Normal NormalText"
    >    now     &lt;- liftIO \$ getCurrentTime</span
    ><br
     /><span class="Normal NormalText"
    >    tz      &lt;- liftIO \$ getCurrentTimeZone</span
    ><br
     /><br
     /><span class="Normal NormalText"
    >    liftIO \$ getRecentPosts delMVar &gt;&gt;=</span
    ><br
     /><span class="Normal NormalText"
    >           </span
    ><span class="Function"
    >return</span
    ><span class="Normal NormalText"
    > . </span
    ><span class="Function"
    >map</span
    ><span class="Normal NormalText"
    > (agePost tz now)</span
    ><br
     /></code
  ></pre
><p
>What&rsquo;s this <code
  >DiffPost</code
  > type?</p
><pre class="sourceCode haskell"
><code
  ><span class="Keyword"
    >type</span
    ><span class="Normal NormalText"
    > Age = </span
    ><span class="DataType TypeConstructor"
    >String</span
    ><br
     /><br
     /><span class="Keyword"
    >data</span
    ><span class="Normal NormalText"
    > DiffPost = DiffPost !</span
    ><span class="Normal ModuleName"
    >D.Post</span
    ><span class="Normal NormalText"
    > !Age</span
    ><br
     /></code
  ></pre
><p
>I wanted to use relative time to present the bookmarks (e.g. &ldquo;2 hours ago&rdquo;). A <code
  >DiffPost</code
  > is just a delicious post plus a string containing the human-readable timestamp. The reason we can just splat the posts into the template directly is that we&rsquo;ve told <code
  >HStringTemplate</code
  > how to encode it:</p
><pre class="sourceCode haskell"
><code
  ><span class="Keyword"
    >instance</span
    ><span class="Normal NormalText"
    > ToSElem DiffPost </span
    ><span class="Keyword"
    >where</span
    ><br
     /><span class="Normal NormalText"
    >    toSElem (DiffPost (</span
    ><span class="Normal ModuleName"
    >D.Post</span
    ><span class="Normal NormalText"
    > href _ desc notes tags _ _) age) =</span
    ><br
     /><span class="Normal NormalText"
    >        SM \$! Map.fromList [ (</span
    ><span class="String"
    >&quot;date&quot;</span
    ><span class="Normal NormalText"
    >,    toSElem age)</span
    ><br
     /><span class="Normal NormalText"
    >                           , (</span
    ><span class="String"
    >&quot;title&quot;</span
    ><span class="Normal NormalText"
    >,   toSElem desc)</span
    ><br
     /><span class="Normal NormalText"
    >                           , (</span
    ><span class="String"
    >&quot;summary&quot;</span
    ><span class="Normal NormalText"
    >, toSElem notes)</span
    ><br
     /><span class="Normal NormalText"
    >                           , (</span
    ><span class="String"
    >&quot;href&quot;</span
    ><span class="Normal NormalText"
    >,    toSElem href)</span
    ><br
     /><span class="Normal NormalText"
    >                           , (</span
    ><span class="String"
    >&quot;tags&quot;</span
    ><span class="Normal NormalText"
    >,    toSElem tags) ]</span
    ><br
     /></code
  ></pre
><p
>So <code
  >getRecent</code
  > just grabs the <code
  >DeliciousState</code
  > out of the <code
  >HomepageState</code
  >, passes it to <code
  >getRecentPosts</code
  >, and computes human-readable ages for the results. The <code
  >getRecentPosts</code
  > function is only a little bit hairier:</p
><pre class="sourceCode haskell"
><code
  ><span class="Function FunctionDefinition"
    >getRecentPosts ::</span
    ><span class="Normal NormalText"
    > MVar DeliciousState -&gt; </span
    ><span class="DataType TypeConstructor"
    >IO</span
    ><span class="Normal NormalText"
    > [</span
    ><span class="Normal ModuleName"
    >D.Post</span
    ><span class="Normal NormalText"
    >]</span
    ><br
     /><span class="Normal NormalText"
    >getRecentPosts mvar = </span
    ><span class="Keyword"
    >do</span
    ><br
     /><span class="Normal NormalText"
    >    now   &lt;- getCurrentTime</span
    ><br
     /><span class="Normal NormalText"
    >    empty &lt;- isEmptyMVar mvar</span
    ><br
     /><span class="Normal NormalText"
    >    </span
    ><span class="Keyword"
    >if</span
    ><span class="Normal NormalText"
    > empty </span
    ><span class="Keyword"
    >then</span
    ><span class="Normal NormalText"
    > </span
    ><span class="Keyword"
    >do</span
    ><br
     /><span class="Normal NormalText"
    >        posts &lt;- getRecentPosts'</span
    ><br
     /><span class="Normal NormalText"
    >        tryPutMVar mvar \$! DeliciousState posts now</span
    ><br
     /><span class="Normal NormalText"
    >        </span
    ><span class="Function"
    >return</span
    ><span class="Normal NormalText"
    > posts</span
    ><br
     /><span class="Normal NormalText"
    >      </span
    ><span class="Keyword"
    >else</span
    ><span class="Normal NormalText"
    > </span
    ><span class="Keyword"
    >do</span
    ><br
     /><span class="Normal NormalText"
    >        modifyMVar mvar \$! \oldstate@(DeliciousState oldposts oldtime) -&gt; </span
    ><span class="Keyword"
    >do</span
    ><br
     /><span class="Normal NormalText"
    >                       </span
    ><span class="Keyword"
    >if</span
    ><span class="Normal NormalText"
    > tooOld now oldtime </span
    ><span class="Keyword"
    >then</span
    ><span class="Normal NormalText"
    > </span
    ><span class="Keyword"
    >do</span
    ><br
     /><span class="Normal NormalText"
    >                           posts &lt;- getRecentPosts'</span
    ><br
     /><span class="Normal NormalText"
    >                           </span
    ><span class="Keyword"
    >let</span
    ><span class="Normal NormalText"
    > newstate = DeliciousState (posts </span
    ><span class="Others InfixOperator"
    >`seq`</span
    ><span class="Normal NormalText"
    > posts) now</span
    ><br
     /><span class="Normal NormalText"
    >                           </span
    ><span class="Function"
    >return</span
    ><span class="Normal NormalText"
    > \$! (newstate </span
    ><span class="Others InfixOperator"
    >`seq`</span
    ><span class="Normal NormalText"
    > newstate, posts </span
    ><span class="Others InfixOperator"
    >`seq`</span
    ><span class="Normal NormalText"
    > posts)</span
    ><br
     /><span class="Normal NormalText"
    >                         </span
    ><span class="Keyword"
    >else</span
    ><br
     /><span class="Normal NormalText"
    >                           </span
    ><span class="Function"
    >return</span
    ><span class="Normal NormalText"
    > \$! (oldstate, oldposts)</span
    ><br
     /><span class="Normal NormalText"
    >  </span
    ><span class="Keyword"
    >where</span
    ><br
     /><span class="Normal NormalText"
    >    </span
    ><span class="Function FunctionDefinition"
    >tooOld ::</span
    ><span class="Normal NormalText"
    > UTCTime -&gt; UTCTime -&gt; </span
    ><span class="DataType TypeConstructor"
    >Bool</span
    ><br
     /><span class="Normal NormalText"
    >    tooOld now old = diffUTCTime now old &gt; </span
    ><span class="DecVal Decimal"
    >60</span
    ><span class="Normal NormalText"
    > * </span
    ><span class="DecVal Decimal"
    >60</span
    ><span class="Normal NormalText"
    > * </span
    ><span class="DecVal Decimal"
    >4</span
    ><br
     /></code
  ></pre
><p
>If the <code
  >MVar</code
  > is empty then haven&rsquo;t yet pulled the feed and we should do so. If the <code
  >MVar</code
  > contains data, we check whether the last update was less than four hours ago, and if it was we just return the old posts. Otherwise, we fetch the feed and update the timestamp.</p
><h3 id="next-in-this-series"
>Next in this series</h3
><p
>If you look at the <a href="http://github.com/gregorycollins/homepage/"
  >source</a
  > for this website, it&rsquo;s clear that these posts are hard-coded as static content. In Part 3 of this series, we&rsquo;ll create a simple dynamic content system which will allow us to drop posts on the filesystem, in <a href="http://daringfireball.net/projects/markdown/"
  >markdown</a
  > format, and have them be published to the site.</p
>
