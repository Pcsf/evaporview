(function () {
  var errs = [];
  window.addEventListener('error', function (e) { errs.push(String(e.message)); });
  function snap(step) {
    var A = window.evw.state;
    return { step: step, marker: A.marker, alt: A.altMarker, vS: Math.round(A.vS), vE: Math.round(A.vE),
             items: A.items.length, sel: Object.keys(A.sel).length,
             primary: A.primary ? (A.primary.path || A.primary.name) : null };
  }
  function at(el, x, y, extra) {
    var r = el.getBoundingClientRect(), o = { bubbles: true, cancelable: true, clientX: r.left + x, clientY: r.top + y };
    for (var k in extra) o[k] = extra[k];
    return o;
  }
  function rowY(i) { var A = window.evw.state, r = A.rows[i]; return 30 + r.top - A.scrollY + r.h / 2; }
  function key(k, mods) { var o = { key: k, bubbles: true, cancelable: true }; for (var m in mods) o[m] = mods[m]; document.dispatchEvent(new KeyboardEvent('keydown', o)); }
  function pixels(c) { var d = c.getContext('2d').getImageData(0, 0, c.width, c.height).data, n = 0; for (var i = 3; i < d.length; i += 4) if (d[i]) n++; return n; }
  setTimeout(function () {
    var wave = document.getElementById('wave'), W = wave.getBoundingClientRect().width, log = [];
    log.push(snap('init'));
    var flag = [].slice.call(document.querySelectorAll('#tree *')).filter(function (e) { return e.childElementCount <= 2 && /^flag/.test(e.textContent.trim()); })[0];
    flag.dispatchEvent(new MouseEvent('dblclick', { bubbles: true }));
    log.push(snap('dblclick flag'));
    var y0 = rowY(0);
    wave.dispatchEvent(new MouseEvent('mousedown', at(wave, W * 0.3, y0, { button: 0 })));
    window.dispatchEvent(new MouseEvent('mouseup', at(wave, W * 0.3, y0, { button: 0 })));
    log.push(snap('click clk row'));
    key('ArrowRight'); log.push(snap('ArrowRight'));
    key('ArrowRight'); log.push(snap('ArrowRight'));
    key('ArrowLeft'); log.push(snap('ArrowLeft'));
    wave.dispatchEvent(new MouseEvent('mousedown', at(wave, W * 0.6, y0, { button: 0, altKey: true })));
    log.push(snap('alt+click'));
    wave.dispatchEvent(new WheelEvent('wheel', at(wave, W * 0.5, y0, { deltaY: -400, ctrlKey: true })));
    log.push(snap('ctrl+wheel in'));
    key('0', { ctrlKey: true }); log.push(snap('ctrl+0'));
    wave.dispatchEvent(new MouseEvent('mousedown', at(wave, W * 0.2, y0, { button: 0 })));
    window.dispatchEvent(new MouseEvent('mousemove', at(wave, W * 0.4, y0, { button: 0 })));
    window.dispatchEvent(new MouseEvent('mouseup', at(wave, W * 0.4, y0, { button: 0 })));
    log.push(snap('drag zoom'));
    key('0', { ctrlKey: true });
    key('Delete'); log.push(snap('Delete'));
    key('z', { ctrlKey: true }); log.push(snap('ctrl+z'));
    setTimeout(function () {
      var pre = document.createElement('pre'); pre.id = 'harness-result';
      pre.textContent = JSON.stringify({ errs: errs, canvas: [wave.width, wave.height, pixels(wave)], log: log });
      document.body.appendChild(pre);
    }, 300);
  }, 400);
})();
