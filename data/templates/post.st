$htmlheader(whichCss="posts")$
<body>
  <div id="page">
    <div id="topbar">
    $header()$
    $navigation()$
    </div>

    <div id="content">
      <div class="post">
        <h2>$post.title$</h2>
        <div class="post-meta">
          <div class="post-date">$post.date$</div>
          <div class="post-summary">$post.summary$</div>
        </div>

        <div class="post-content">
          $post.content$
        </div>

        $disqusThread()$
      </div>
    </div>
  </div>
$footer()$
  </body>
</html>
