// ==UserScript==
// @name Rennrad-News Textarea & Privacy fix
// @namespace https://github.com/Leonidas-from-XIV/sandbox/
// @description Fix the cursor navigation on Rennrad News
// @include http://www.rennrad-news.de/forum/*
// ==/UserScript==

(function () {
  $(document).ready(function () {
    var area = $('textarea#ctrl_message');
    // ugly delay hack to wait for the events to be bound to unbind them
    area.delay(500).unbind();

    // also delete tracking link gateway
    setTimeout(function () {
      $("a[ao_sl_clk]").removeAttr("ao_sl_clk");
      $("a[ao_sl_cont]").removeAttr("ao_sl_cont");
      $("a[ao_sl_blur]").removeAttr("ao_sl_blur");
      $("a[ao_sl_mouseout]").removeAttr("ao_sl_mouseout");
    }, 500);
  });
})();
