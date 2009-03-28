$htmlheader()$
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
        <h2>posts</h2>
        <table>
          <tr>
            <td class="date">march 28, 2009</td>
            <td class="title">
              <a href="/posts/2009/03/28/building-a-website-part-1">Building
                a website with Haskell, part 1</a>
            </td>
            <td class="summary">
              Using
              the <a href="http://www.happstack.com/">happstack</a> web
              framework to power a simple personal website.
            </td>
          </tr>
        </table>
      </div>

      <div id="recently-bookmarked">
        <h2><a href="http://delicious.com/how.gauche/">recently bookmarked</a></h2>
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
              <a href="http://github.com/gregorycollins">my github</a>
            </td>
            <td class="summary">
              I've posted some code I'm working on here,
              including <a href="http://github.com/gregorycollins/homepage/tree/v0.1">the
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
