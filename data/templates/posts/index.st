$htmlheader(whichCss="posts")$
<body>
  <div id="page">
    <div id="topbar">
      $header()$
      $navigation()$
    </div>

    <div id="content">
      <div id="recent-posts">
        <h2><a href="/posts">posts</a> <a href="/feed.xml"><img src="/static/i/feed.png"/></a></h2>
        <table>
          $reverseChronologicalPosts:indexPostRow()$
        </table>
      </div>

    </div>

    $footer()$
  </div>

$htmlfooter()$
