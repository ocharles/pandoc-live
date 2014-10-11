# Pandoc, LIVE!

This is a little utility can be used to watch a Pandoc document for changes, and
display the rendering in real-time in a web browser. Specifically, `pandoc-live`
establishes an `inotify`-watch on a source file, and uses HTML 5 server-sent
events to push these changes to connected web browsers. A little bit of
JavaScript pushes this event payload into the DOM. Easy!
