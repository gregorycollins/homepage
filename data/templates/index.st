$htmlheader(whichCss="home")$
<body>
  <div id="intro">
    <table><tr><td align="middle" valign="middle">
      <span id="hello">Hi!</span> Gregory David Collins is a
      compu-dork living in Toronto, Canada.
    </td></tr></table>
  </div>

  <div id="page">
    <div id="topbar">
      $header()$
      $navigation()$
    </div>

    <div id="content">
      <div id="recent-posts">
        <h2><a href="/posts">recent posts</a> <a href="/feed.xml"><img src="/static/i/feed.png"/></a></h2>
        <table>
          $recentPosts:indexPostRow()$
        </table>
      </div>

      <div id="recently-bookmarked">
        <h2><a href="http://delicious.com/how.gauche/">recently bookmarked</a>
            <a href="http://feeds.delicious.com/v2/rss/how.gauche?count=15"
            ><img src="/static/i/feed.png"/></a></h2>
        <table>
          $recentBookmarks:{elem | $deliciousBookmark()$}$
        </table>
      </div>

      <div id="links">
        <h2>links</h2>
        <table>
          <tr>
            <td class="date"></td>
            <td class="title">
              <a href="http://hackage.haskell.org/platform/">Haskell Platform</a>
            </td>
            <td class="summary">
              I am currently the maintainer of the OSX installer for
              the Haskell Platform project.
            </td>
          </tr>
          <tr>
            <td class="date"></td>
            <td class="title">
              <a href="http://github.com/gregorycollins">my github</a>
            </td>
            <td class="summary">
              I've posted some code I'm working on here,
              including <a href="http://github.com/gregorycollins/homepage/tree">the
              source for this website</a>.
            </td>
          </tr>

          <tr>
            <td class="date"></td>
            <td class="title">
              <a href="http://www.blocksblocksblocks.com/">blocks recording club</a>
            </td>
            <td class="summary">
              The □□□□□□ Recording Club is an artist-owned worker’s
              co-operative based in Toronto. I am its
              &ldquo;web-goblin&rdquo; as well as a member of the (now
              defunct) □□□□□□ rap group <em>Ninja High School</em>.
            </td>
          </tr>
        </table>
      </div>


    </div>

    $footer()$
  </div>

$htmlfooter()$
