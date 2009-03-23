$htmlheader()$
<body>
  <div id="page">
    <div id="topbar">
    $header()$
    $navigation()$
    </div>

    <div id="content">
      <div class="post">
        <h2>$postTitle$</h2>
        <div class="post-meta">
          <div class="post-date">$postDate$</div>
          <div class="post-summary">$postSummary$</div>
        </div>

        <div class="post-content">
          $postContent$
        </div>

        $disqusThread()$
      </div>
    </div>
  </div>
$footer()$
  </body>
</html>
