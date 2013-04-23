// ==UserScript==
// @name Rennrad-News Textarea-Fix
// @namespace https://github.com/Leonidas-from-XIV/sandbox/
// @description Fix the cursor navigation on Rennrad News
// @include http://www.rennrad-news.de/forum/*
// ==/UserScript==

(function () {
  $(document).ready(function () {
    var area = $('textarea#ctrl_message');
    // ugly dalay hack to wait for the events to be bound to unbind them
    area.delay(500).unbind();
  });
})();
