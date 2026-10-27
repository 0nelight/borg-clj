// Progress without reloading, where a browser can do it.
//
// This file is an enhancement and nothing more. Every page works with it
// switched off: the run page then carries a <noscript> meta-refresh and the
// server re-renders the same numbers. That is what lets the recovery system
// get by with a text browser instead of 300-650 MB of graphical one.
(function () {
  var box = document.getElementById('lauf');
  if (!box || !window.EventSource) return;

  var bar = box.querySelector('.bar > i');
  var pct = box.querySelector('.pct');
  var eta = box.querySelector('.eta');
  var num = box.querySelector('.zahlen');
  var log = box.querySelector('pre.log');

  var es = new EventSource('/ereignisse');

  es.addEventListener('zustand', function (ev) {
    var d;
    try { d = JSON.parse(ev.data); } catch (e) { return; }

    if (d.hasProgress) {
      pct.textContent = d.percent.toFixed(1).replace('.', ',') + ' %';
      bar.style.width = d.percent + '%';
      num.textContent = d.zahlen;
      eta.textContent = d.eta;
    }
    if (d.lines && d.lines.length) {
      // Only append: the server sends what is new since our last event, so
      // rebuilding the whole protocol would be wasteful and would fight the
      // scroll position.
      var atEnd = log.scrollTop + log.clientHeight >= log.scrollHeight - 24;
      log.textContent += d.lines.join('\n') + '\n';
      if (atEnd) log.scrollTop = log.scrollHeight;
    }
    if (d.state !== 'running') {
      // Let the server render the finished state -- buttons, summary, result
      // colour all live in the page template, in one place.
      es.close();
      setTimeout(function () { location.reload(); }, 400);
    }
  });

  es.onerror = function () { /* the server is gone; the page stays as it is */ };
})();
