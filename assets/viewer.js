/* evaporview browser viewer: pure logic first, UI below. */
'use strict';

var UNITS = ['fs', 'ps', 'ns', 'us', 'ms', 's'];
var UNIT_EXP = { fs: -15, ps: -12, ns: -9, us: -6, ms: -3, s: 0 };
var FORMATS = ['binary', 'hex', 'octal', 'unsigned', 'signed', 'ascii', 'float', 'real'];
var COLOR_COUNT = 8;

/* ---------- value formatting ---------- */

function isBin(bits) { return /^[01]+$/.test(bits); }

function repeatStr(s, n) { return n > 0 ? new Array(n + 1).join(s) : ''; }

function stripZeros(s) {
  if (s.indexOf('.') < 0) return s;
  return s.replace(/0+$/, '').replace(/\.$/, '');
}

/* Round a positive finite double to p significant digits, half-to-even on the
   exact binary value, as C printf does. */
function roundDigits(x, p) {
  var m = /^(\d)\.(\d+)e([+-]\d+)$/.exec(x.toExponential(100));
  var all = m[1] + m[2];
  var exp = parseInt(m[3], 10);
  var keep = all.slice(0, p);
  var rest = all.slice(p);
  var up = false;
  if (rest.charAt(0) > '5') up = true;
  else if (rest.charAt(0) === '5') {
    up = /[1-9]/.test(rest.slice(1)) || ((keep.charCodeAt(p - 1) - 48) % 2 === 1);
  }
  if (up) {
    var n = (BigInt(keep) + BigInt(1)).toString();
    if (n.length > p) { n = n.slice(0, p); exp += 1; }
    keep = n;
  }
  return { digits: keep, exp: exp };
}

/* C printf("%g") with the default precision of 6. */
function formatG(x) {
  if (x !== x) return 'nan';
  if (x === Infinity) return 'inf';
  if (x === -Infinity) return '-inf';
  if (x === 0) return (1 / x < 0) ? '-0' : '0';
  var r = roundDigits(Math.abs(x), 6);
  var d = r.digits, e = r.exp, out;
  if (e < -4 || e >= 6) {
    var ea = Math.abs(e);
    out = stripZeros(d.charAt(0) + '.' + d.slice(1)) + 'e' + (e < 0 ? '-' : '+') + (ea < 10 ? '0' : '') + ea;
  } else if (e >= 0) {
    out = stripZeros(d.slice(0, e + 1) + '.' + d.slice(e + 1));
  } else {
    out = stripZeros('0.' + repeatStr('0', -e - 1) + d);
  }
  return (x < 0 ? '-' : '') + out;
}

function formatReal(str) {
  var s = String(str).trim().toLowerCase();
  if (s === 'nan') return 'nan';
  if (s === 'inf' || s === '+inf' || s === 'infinity') return 'inf';
  if (s === '-inf' || s === '-infinity') return '-inf';
  var x = parseFloat(s);
  if (x !== x) return String(str);
  return formatG(x);
}

function decodeFloat(bits) {
  var w = bits.length, dv;
  if (w === 16) {
    var neg = bits.charAt(0) === '1';
    var ex = parseInt(bits.slice(1, 6), 2);
    var fr = parseInt(bits.slice(6), 2);
    var v;
    if (ex === 31) v = fr ? NaN : Infinity;
    else if (ex === 0) v = fr * Math.pow(2, -24);
    else v = (1024 + fr) * Math.pow(2, ex - 25);
    return neg ? -v : v;
  }
  dv = new DataView(new ArrayBuffer(8));
  if (w === 32) { dv.setUint32(0, parseInt(bits, 2)); return dv.getFloat32(0); }
  if (w === 64) { dv.setBigUint64(0, BigInt('0b' + bits)); return dv.getFloat64(0); }
  return null;
}

function groupFormat(bits, g) {
  var pad = (g - bits.length % g) % g;
  bits = repeatStr('0', pad) + bits;
  var out = '';
  for (var i = 0; i < bits.length; i += g) {
    var grp = bits.substr(i, g);
    if (/^z+$/.test(grp)) out += 'z';
    else if (isBin(grp)) out += parseInt(grp, 2).toString(16).toUpperCase();
    else out += 'x';
  }
  return out;
}

function formatBits(bits, fmt) {
  bits = String(bits).toLowerCase();
  var n = bits.length;
  switch (fmt) {
    case 'hex': return groupFormat(bits, 4);
    case 'octal': return groupFormat(bits, 3);
    case 'unsigned':
      return isBin(bits) ? BigInt('0b' + bits).toString() : bits;
    case 'signed':
      if (!isBin(bits)) return bits;
      var v = BigInt('0b' + bits);
      if (bits.charAt(0) === '1') v -= BigInt(1) << BigInt(n);
      return v.toString();
    case 'ascii':
      if (!isBin(bits)) return bits;
      var p = repeatStr('0', (8 - n % 8) % 8) + bits, s = '';
      for (var i = 0; i < p.length; i += 8) {
        var c = parseInt(p.substr(i, 8), 2);
        s += (c >= 32 && c <= 126) ? String.fromCharCode(c) : '.';
      }
      return s;
    case 'float':
      if (!isBin(bits) || (n !== 16 && n !== 32 && n !== 64)) return bits;
      return formatG(decodeFloat(bits));
    default:
      return bits;
  }
}

/* LSB = bit 0 at the right end of the string. */
function sliceBits(bits, msb, lsb) {
  var w = bits.length;
  return bits.substring(Math.max(0, w - 1 - msb), Math.max(0, w - lsb));
}

function sliceWidth(sig, slice) { return slice ? slice[0] - slice[1] + 1 : sig.width; }

/* val: raw dump value; sig: {real:boolean}. A real signal ignores fmt. */
/* Real and enumeration signals carry text values, not bit strings. */
function isText(sig) { return !!(sig.real || sig.enum); }

function formatValue(sig, val, fmt, slice) {
  if (val === null || val === undefined) return '';
  if (sig.enum) return String(val);
  if (sig.real) return formatReal(val);
  return formatBits(slice ? sliceBits(val, slice[0], slice[1]) : val, fmt);
}

/* ---------- time ---------- */

function pow10Str(n) { return '1' + repeatStr('0', n); }

/* ts: [multiplier, unit]. Returns e.g. "12.5 ns". */
function formatTime(ticks, ts, unit) {
  var shift = UNIT_EXP[ts[1]] - UNIT_EXP[unit];
  var n = BigInt(Math.round(ticks)) * BigInt(ts[0]);
  var neg = n < BigInt(0);
  if (neg) n = -n;
  var s = n.toString(), intStr, fracStr;
  if (shift >= 0) { intStr = s + repeatStr('0', shift); fracStr = ''; }
  else {
    var fl = -shift;
    if (s.length < fl + 1) s = repeatStr('0', fl + 1 - s.length) + s;
    intStr = s.slice(0, s.length - fl);
    fracStr = s.slice(s.length - fl);
  }
  intStr = intStr.replace(/^0+(?=.)/, '');
  var decimals;
  if (intStr !== '0') decimals = Math.max(0, 6 - intStr.length);
  else {
    var z = /^0*/.exec(fracStr)[0].length;
    decimals = z + 6;
  }
  var all = BigInt(intStr + fracStr), fl2 = fracStr.length;
  var drop = fl2 - decimals;
  if (drop > 0) {
    var div = BigInt(pow10Str(drop));
    var q = all / div, r = all % div;
    if (r * BigInt(2) >= div) q += BigInt(1);
    all = q; fl2 = decimals;
  }
  var t = all.toString();
  if (fl2 > 0) {
    if (t.length <= fl2) t = repeatStr('0', fl2 - t.length + 1) + t;
    t = stripZeros(t.slice(0, t.length - fl2) + '.' + t.slice(t.length - fl2));
  }
  return (neg && /[1-9]/.test(t) ? '-' : '') + t + ' ' + unit;
}

/* Dump ticks in one display unit; may be fractional. */
function ticksPerUnit(ts, unit) {
  return Math.pow(10, UNIT_EXP[unit] - UNIT_EXP[ts[1]]) / ts[0];
}

function autoUnit(ts, endTicks) {
  for (var i = UNITS.length - 1; i >= 0; i--) {
    if (endTicks / ticksPerUnit(ts, UNITS[i]) >= 1) return UNITS[i];
  }
  return ts[1];
}

function niceStep(minTicks) {
  if (!(minTicks > 1)) return 1;
  var p = Math.pow(10, Math.floor(Math.log(minTicks) / Math.LN10));
  var m = [1, 2, 5, 10];
  for (var i = 0; i < m.length; i++) if (m[i] * p >= minTicks) return m[i] * p;
  return 10 * p;
}

function formatFrequency(seconds) {
  if (!(seconds > 0)) return '';
  var f = Number((1 / seconds).toPrecision(9)), names = ['Hz', 'kHz', 'MHz', 'GHz', 'THz'], i = 0;
  while (f >= 1000 && i < names.length - 1) { f /= 1000; i++; }
  while (f < 1 && i > 0) { f *= 1000; i--; }
  return formatG(f) + ' ' + names[i];
}

/* ---------- searching the change lists ---------- */

/* Index of the last t[i] <= x, or -1. */
function lastIdxLE(t, x) {
  var lo = 0, hi = t.length;
  while (lo < hi) { var m = (lo + hi) >> 1; if (t[m] <= x) lo = m + 1; else hi = m; }
  return lo - 1;
}

/* Index of the last t[i] < x, or -1. */
function lastIdxLT(t, x) {
  var lo = 0, hi = t.length;
  while (lo < hi) { var m = (lo + hi) >> 1; if (t[m] < x) lo = m + 1; else hi = m; }
  return lo - 1;
}

function keyAt(sig, k, slice) {
  var v = sig.v[k];
  return slice && !isText(sig) ? sliceBits(v, slice[0], slice[1]) : v;
}

function valueAt(sig, time) {
  var i = lastIdxLE(sig.t, time);
  return i < 0 ? null : sig.v[i];
}

/* Next (dir > 0) or previous (dir < 0) time the value differs; the first
   recorded value counts as a change. null when there is none. */
function findChange(sig, time, dir, slice) {
  var t = sig.t, n = t.length, k;
  if (dir > 0) {
    for (k = lastIdxLE(t, time) + 1; k < n; k++) {
      if (k === 0 || keyAt(sig, k, slice) !== keyAt(sig, k - 1, slice)) return t[k];
    }
  } else {
    for (k = lastIdxLT(t, time); k >= 0; k--) {
      if (k === 0 || keyAt(sig, k, slice) !== keyAt(sig, k - 1, slice)) return t[k];
    }
  }
  return null;
}

function isHigh(c) { return c === '1' || c === 'h'; }
function isLow(c) { return c === '0' || c === 'l'; }

function isEdge(prev, cur, kind) {
  if (kind === 'rising') return isHigh(cur) && !isHigh(prev);
  return isLow(cur) && !isLow(prev);
}

/* kind: 'rising' | 'falling'. Only for one-bit values. */
function findEdge(sig, time, dir, kind, slice) {
  if (sliceWidth(sig, slice) !== 1 || isText(sig)) return null;
  var t = sig.t, n = t.length, k;
  if (dir > 0) {
    for (k = Math.max(1, lastIdxLE(t, time) + 1); k < n; k++) {
      if (isEdge(keyAt(sig, k - 1, slice), keyAt(sig, k, slice), kind)) return t[k];
    }
  } else {
    for (k = lastIdxLT(t, time); k >= 1; k--) {
      if (isEdge(keyAt(sig, k - 1, slice), keyAt(sig, k, slice), kind)) return t[k];
    }
  }
  return null;
}

function normValue(s) {
  return String(s).trim().toLowerCase().replace(/^0+(?=.)/, '');
}

/* Next/previous time at which the formatted value equals query. */
function searchValue(sig, fmt, slice, query, time, dir) {
  var want = normValue(query), t = sig.t, n = t.length, k;
  var test = function (j) {
    if (j > 0 && keyAt(sig, j, slice) === keyAt(sig, j - 1, slice)) return false;
    return normValue(formatValue(sig, sig.v[j], fmt, slice)) === want;
  };
  if (dir > 0) {
    for (k = lastIdxLE(t, time) + 1; k < n; k++) if (test(k)) return t[k];
  } else {
    for (k = lastIdxLT(t, time); k >= 0; k--) if (test(k)) return t[k];
  }
  return null;
}

/* ---------- WaveDrom ---------- */

function waveBitChar(key) {
  var c = key.charAt(0);
  if (isHigh(c)) return '1';
  if (isLow(c)) return '0';
  return c === 'z' ? 'z' : 'x';
}

/* lanes: [{name, sig, fmt, slice, clock}]; clock: {lane, edge:'rise'|'fall'}
   or null. Returns {doc, truncated, events}. */
function waveDrom(lanes, t0, t1, clock, max) {
  var events = [], truncated = false, i, k;
  max = max || 32;
  if (clock) {
    var kind = clock.edge === 'fall' ? 'falling' : 'rising';
    var tc = t0 - 1, nx;
    while ((nx = findEdge(clock.lane.sig, tc, 1, kind, clock.lane.slice)) !== null && nx <= t1) {
      if (events.length >= max) { truncated = true; break; }
      events.push(nx); tc = nx;
    }
  } else {
    var seen = {};
    seen[t0] = true; events.push(t0);
    lanes.forEach(function (ln) {
      var t = ln.sig.t;
      for (k = lastIdxLE(t, t0) + 1; k < t.length && t[k] <= t1; k++) {
        if ((k === 0 || keyAt(ln.sig, k, ln.slice) !== keyAt(ln.sig, k - 1, ln.slice)) && !seen[t[k]]) {
          seen[t[k]] = true; events.push(t[k]);
        }
      }
    });
    events.sort(function (a, b) { return a - b; });
    if (events.length > max) { events.length = max; truncated = true; }
  }
  var signal = lanes.map(function (ln) {
    var wave = '', data = [], prev = null;
    var w = sliceWidth(ln.sig, ln.slice);
    var one = w === 1 && !isText(ln.sig);
    for (i = 0; i < events.length; i++) {
      var raw = valueAt(ln.sig, events[i]);
      var key = raw === null ? 'x' : (ln.slice && !isText(ln.sig) ? sliceBits(raw, ln.slice[0], ln.slice[1]) : raw);
      if (clock && ln.clock) { wave += i === 0 ? (clock.edge === 'fall' ? 'n' : 'p') : '.'; continue; }
      var ch;
      if (one) ch = waveBitChar(key);
      else if (isText(ln.sig) || isBin(key)) ch = '=';
      else ch = /^z+$/.test(key) ? 'z' : 'x';
      var same = i > 0 && (one ? waveBitChar(prev) === ch : prev === key);
      if (same) wave += '.';
      else {
        wave += ch;
        if (ch === '=') data.push(formatValue(ln.sig, raw, ln.fmt, ln.slice));
      }
      prev = key;
    }
    var o = { name: ln.name, wave: wave };
    if (data.length) o.data = data;
    return o;
  });
  return { doc: { signal: signal }, truncated: truncated, events: events };
}

/* ---------- session ---------- */

function clampColor(c) {
  c = Math.round(Number(c));
  return c >= 1 && c <= COLOR_COUNT ? c : 1;
}

function intOrNull(x) {
  return typeof x === 'number' && isFinite(x) ? Math.round(x) : null;
}

function normalizeItem(raw) {
  if (!raw || typeof raw !== 'object') return null;
  if (raw.type === 'group') {
    var g = { type: 'group', name: typeof raw.name === 'string' ? raw.name : 'group',
              expanded: raw.expanded !== false, items: normalizeItems(raw.items) };
    if (raw._id !== undefined) g._id = raw._id;
    return g;
  }
  if (raw.type !== 'signal' || typeof raw.path !== 'string') return null;
  var it = {
    type: 'signal', path: raw.path,
    name: typeof raw.name === 'string' ? raw.name : null,
    format: FORMATS.indexOf(raw.format) >= 0 ? raw.format : 'binary',
    color: clampColor(raw.color),
    style: raw.style === 'analog' ? 'analog' : 'digital',
    slice: null
  };
  if (Array.isArray(raw.slice) && raw.slice.length === 2) {
    var m = intOrNull(raw.slice[0]), l = intOrNull(raw.slice[1]);
    if (m !== null && l !== null && m >= l && l >= 0) it.slice = [m, l];
  }
  if (raw.lineStyle === 'step' || raw.lineStyle === 'linear') it.lineStyle = raw.lineStyle;
  if (typeof raw.height === 'number' && raw.height >= 12 && raw.height <= 400) it.height = Math.round(raw.height);
  if (raw._id !== undefined) it._id = raw._id;
  return it;
}

function normalizeItems(arr) {
  var out = [];
  (Array.isArray(arr) ? arr : []).forEach(function (r) {
    var it = normalizeItem(r);
    if (it) out.push(it);
  });
  return out;
}

/* Fixed key order, as the shared schema shows. */
function itemToPlain(it) {
  if (it.type === 'group') {
    return { type: 'group', name: it.name, expanded: !!it.expanded, items: it.items.map(itemToPlain) };
  }
  var o = {
    type: 'signal', path: it.path, name: it.name === undefined ? null : it.name,
    format: it.format, color: it.color, style: it.style,
    slice: it.slice ? [it.slice[0], it.slice[1]] : null
  };
  if (it.style === 'analog' && it.lineStyle) o.lineStyle = it.lineStyle;
  if (it.height) o.height = it.height;
  return o;
}

function sessionToJSON(st) {
  var o = {
    version: 1,
    file: st.file === undefined ? null : st.file,
    marker: intOrNull(st.marker),
    altMarker: intOrNull(st.altMarker),
    viewStart: intOrNull(st.viewStart),
    viewEnd: intOrNull(st.viewEnd),
    timeUnit: st.timeUnit === undefined ? null : st.timeUnit,
    items: (st.items || []).map(itemToPlain)
  };
  return JSON.stringify(o, null, 2) + '\n';
}

function sessionFromJSON(text) {
  var o = typeof text === 'string' ? JSON.parse(text) : text;
  if (!o || typeof o !== 'object' || Array.isArray(o)) throw new Error('not a session object');
  if (o.version !== 1) throw new Error('unsupported session version: ' + o.version);
  return {
    version: 1,
    file: typeof o.file === 'string' ? o.file : null,
    marker: intOrNull(o.marker),
    altMarker: intOrNull(o.altMarker),
    viewStart: intOrNull(o.viewStart),
    viewEnd: intOrNull(o.viewEnd),
    timeUnit: UNITS.indexOf(o.timeUnit) >= 0 ? o.timeUnit : null,
    items: normalizeItems(o.items)
  };
}

/* ---------- UI ---------- */

/* Visit the visible part of a change list, one callback per drawable
   segment. Pixels holding several changes collapse into one dense callback
   after a binary search, so the cost follows the pixel count, not the
   number of changes. */
function scanSegments(t, vs, ve, tpp, onSeg, onDense) {
  var n = t.length;
  if (!n || t[0] > ve) return;
  var k = lastIdxLE(t, vs);
  if (k < 0) k = 0;
  while (k < n && t[k] <= ve) {
    var next = k + 1;
    var hasNext = next < n && t[next] <= ve;
    var xs = (t[k] - vs) / tpp;
    var xe = ((hasNext ? t[next] : ve) - vs) / tpp;
    if (hasNext) {
      var ps = Math.floor(Math.max(xs, 0));
      if (ps === Math.floor(xe)) {
        var lastK = lastIdxLT(t, vs + (ps + 1) * tpp);
        if (lastK > k) { onDense(ps, k, lastK); k = lastK; continue; }
      }
    }
    onSeg(k, Math.max(xs, -20), xe, t[k] < vs, !hasNext);
    k = next;
  }
}

function numericCache(sig, item) {
  var mode = sig.enum ? 'enum' : sig.real ? 'real' : (item.format === 'signed' ? 'signed' : (item.format === 'float' ? 'float' : 'unsigned'));
  var key = mode + '|' + (item.slice && !sig.real ? item.slice.join(':') : '');
  if (!sig._num) sig._num = {};
  if (sig._num[key]) return sig._num[key];
  var n = sig.t.length, vals = new Float64Array(n), min = Infinity, max = -Infinity;
  for (var k = 0; k < n; k++) {
    var x = NaN, v = sig.v[k];
    if (sig.enum) x = sig.enum.indexOf(v);
    else if (sig.real) x = parseFloat(v);
    else {
      var b = item.slice ? sliceBits(v, item.slice[0], item.slice[1]) : v;
      if (isBin(b)) {
        if (mode === 'float') { var f = decodeFloat(b); x = f === null ? NaN : f; }
        else {
          x = b.length <= 52 ? parseInt(b, 2) : Number(BigInt('0b' + b));
          if (mode === 'signed' && b.charAt(0) === '1') x -= Math.pow(2, b.length);
        }
      }
    }
    vals[k] = x;
    if (isFinite(x)) { if (x < min) min = x; if (x > max) max = x; }
  }
  if (min > max) { min = 0; max = 1; }
  if (min === max) { min -= 1; max += 1; }
  return (sig._num[key] = { vals: vals, min: min, max: max });
}

function bootUI() {
  var $ = function (id) { return document.getElementById(id); };
  var D;
  try { D = JSON.parse($('evw-data').textContent); } catch (err) {
    document.body.textContent = 'evaporview: this page has no embedded waveform data.';
    return;
  }
  if (!(D.end > D.start)) D.end = D.start + 1;
  var TS = D.timescale || [1, 'ns'];
  var RULER_H = 30;
  var FONT = '12px ui-monospace, Menlo, Consolas, "DejaVu Sans Mono", monospace';
  var varByPath = {};
  D.vars.forEach(function (v) { varByPath[v.path] = v; });

  var A = {
    items: [], sel: {}, primary: null, nextId: 1, marker: null, altMarker: null,
    vS: D.start, vE: D.end, unit: autoUnit(TS, D.end - D.start), scrollY: 0,
    rows: [], totalH: 0, undo: [], redo: [], expanded: {}, theme: null, hatch: null,
    drag: null, cursor: null, modal: null, menu: null, pending: false, msgTimer: 0
  };

  var wave = $('wave'), ctx = wave.getContext('2d');
  var filter = $('filter');

  /* ----- items ----- */

  function sigOf(item) {
    var v = varByPath[item.path];
    return v ? D.sigs[v.sig] : null;
  }

  function isAnalog(item) { return item.type === 'signal' && item.style === 'analog'; }

  function rowHeight(item) {
    if (item.type === 'group') return 24;
    return item.height || (isAnalog(item) ? 64 : 28);
  }

  function labelOf(item) {
    if (item.name) return item.name;
    if (item.type === 'group') return 'group';
    var v = varByPath[item.path];
    if (!v) return item.path;
    if (item.slice) {
      var s = item.slice;
      return v.name + '[' + s[0] + (s[0] !== s[1] ? ':' + s[1] : '') + ']';
    }
    return v.name + (v.range || '');
  }

  function assignIds(items) {
    items.forEach(function (it) {
      if (it._id === undefined) it._id = A.nextId++;
      else if (it._id >= A.nextId) A.nextId = it._id + 1;
      if (it.type === 'group') assignIds(it.items);
    });
  }

  function countSignals(items) {
    var n = 0;
    items.forEach(function (it) { n += it.type === 'group' ? countSignals(it.items) : 1; });
    return n;
  }

  function newSignalItem(v) {
    var sig = D.sigs[v.sig];
    return {
      _id: A.nextId++, type: 'signal', path: v.path, name: null,
      format: sig.real ? 'real' : (sig.width === 1 ? 'binary' : 'hex'),
      color: countSignals(A.items) % COLOR_COUNT + 1,
      style: sig.real ? 'analog' : 'digital', slice: null
    };
  }

  function findLoc(id, arr) {
    arr = arr || A.items;
    for (var i = 0; i < arr.length; i++) {
      if (arr[i]._id === id) return { arr: arr, idx: i, item: arr[i] };
      if (arr[i].type === 'group') {
        var r = findLoc(id, arr[i].items);
        if (r) return r;
      }
    }
    return null;
  }

  function containsId(item, id) {
    if (item._id === id) return true;
    return item.type === 'group' && item.items.some(function (c) { return containsId(c, id); });
  }

  function selectedItems() {
    var out = [];
    A.rows.forEach(function (r) { if (A.sel[r.item._id]) out.push(r.item); });
    return out;
  }

  function selectedSignals() {
    return selectedItems().filter(function (i) { return i.type === 'signal'; });
  }

  function primarySignal() {
    var loc = A.primary !== null ? findLoc(A.primary) : null;
    if (loc && loc.item.type === 'signal' && varByPath[loc.item.path]) return loc.item;
    var s = selectedSignals().filter(function (i) { return varByPath[i.path]; });
    return s.length ? s[0] : null;
  }

  /* ----- undo ----- */

  function pushUndo() {
    A.undo.push(JSON.stringify(A.items));
    if (A.undo.length > 200) A.undo.shift();
    A.redo = [];
  }

  function mutate(fn) {
    pushUndo();
    fn();
    relayout();
  }

  function restore(text) {
    A.items = JSON.parse(text);
    var keep = {};
    (function walk(items) {
      items.forEach(function (it) { if (A.sel[it._id]) keep[it._id] = true; if (it.type === 'group') walk(it.items); });
    })(A.items);
    A.sel = keep;
    if (A.primary !== null && !keep[A.primary]) A.primary = null;
    relayout();
  }

  function undo() {
    if (!A.undo.length) { flash('Nothing to undo'); return; }
    A.redo.push(JSON.stringify(A.items));
    restore(A.undo.pop());
  }

  function redo() {
    if (!A.redo.length) { flash('Nothing to redo'); return; }
    A.undo.push(JSON.stringify(A.items));
    restore(A.redo.pop());
  }

  /* ----- layout and name column ----- */

  function relayout() {
    A.rows = [];
    var y = 0;
    (function walk(items, depth) {
      items.forEach(function (it) {
        var h = rowHeight(it);
        A.rows.push({ item: it, depth: depth, top: y, h: h });
        y += h;
        if (it.type === 'group' && it.expanded) walk(it.items, depth + 1);
      });
    })(A.items, 0);
    A.totalH = y;
    clampScroll();
    buildNames();
    requestDraw();
  }

  function viewH() { return Math.max(0, wave.clientHeight - RULER_H); }

  function clampScroll() {
    var max = Math.max(0, A.totalH - viewH());
    A.scrollY = Math.min(Math.max(0, A.scrollY), max);
    $('namerows').style.transform = 'translateY(' + (-A.scrollY) + 'px)';
  }

  function colorVar(c) { return 'var(--c' + c + ')'; }

  function buildNames() {
    var box = $('namerows');
    box.textContent = '';
    A.rows.forEach(function (row) {
      var it = row.item;
      var el = document.createElement('div');
      el.className = 'row' + (it.type === 'group' ? ' group' : '');
      el.style.height = row.h + 'px';
      el.draggable = true;
      el.title = it.type === 'signal' ? it.path : labelOf(it);
      var nm = document.createElement('span');
      nm.className = 'nm';
      nm.style.paddingLeft = (6 + row.depth * 14) + 'px';
      var tw = document.createElement('span');
      tw.className = 'twisty';
      if (it.type === 'group') {
        tw.textContent = it.expanded ? '\u25be' : '\u25b8';
        tw.addEventListener('mousedown', function (e) {
          e.stopPropagation();
          mutate(function () { it.expanded = !it.expanded; });
        });
      } else {
        tw.className = 'swatch';
        tw.style.background = colorVar(it.color);
      }
      var tx = document.createElement('span');
      tx.className = 'lbl';
      tx.textContent = labelOf(it);
      nm.appendChild(tw);
      nm.appendChild(tx);
      var val = document.createElement('span');
      val.className = 'val';
      el.appendChild(nm);
      el.appendChild(val);
      row.el = el;
      row.valEl = val;
      el.addEventListener('mousedown', function (e) { if (e.button === 0) selectRow(it, e); });
      el.addEventListener('dblclick', function () { renameItems([it]); });
      el.addEventListener('contextmenu', function (e) { e.preventDefault(); rowMenu(it, e.clientX, e.clientY); });
      el.addEventListener('dragstart', function (e) {
        if (!A.sel[it._id]) selectOnly(it);
        A.dragIds = selectedItems().map(function (i) { return i._id; });
        e.dataTransfer.effectAllowed = 'move';
        e.dataTransfer.setData('text/plain', labelOf(it));
      });
      el.addEventListener('dragover', function (e) {
        if (!A.dragIds) return;
        e.preventDefault();
        var zone = dropZone(e, el, it);
        el.classList.toggle('drop-before', zone === 'before');
        el.classList.toggle('drop-after', zone === 'after');
        el.classList.toggle('drop-into', zone === 'into');
      });
      el.addEventListener('dragleave', function () { clearDrop(el); });
      el.addEventListener('drop', function (e) {
        e.preventDefault();
        var zone = dropZone(e, el, it);
        clearDrop(el);
        if (A.dragIds) moveItems(A.dragIds, it._id, zone);
        A.dragIds = null;
      });
      el.addEventListener('dragend', function () { A.dragIds = null; });
      box.appendChild(el);
    });
    refreshSelection();
    updateValues();
  }

  function dropZone(e, el, it) {
    var r = el.getBoundingClientRect(), f = (e.clientY - r.top) / r.height;
    if (it.type === 'group' && f > 0.25 && f < 0.75) return 'into';
    return f < 0.5 ? 'before' : 'after';
  }

  function clearDrop(el) { el.classList.remove('drop-before', 'drop-after', 'drop-into'); }

  function refreshSelection() {
    A.rows.forEach(function (r) {
      if (!r.el) return;
      r.el.classList.toggle('sel', !!A.sel[r.item._id]);
      r.el.classList.toggle('primary', A.primary === r.item._id);
    });
  }

  function updateValues() {
    A.rows.forEach(function (r) {
      if (!r.valEl) return;
      r.valEl.textContent = r.item.type === 'signal' && A.marker !== null ? itemValueText(r.item, A.marker) : '';
    });
  }

  function itemValueText(item, t) {
    var sig = sigOf(item);
    if (!sig) return '';
    var raw = valueAt(sig, t);
    return raw === null ? '' : formatValue(sig, raw, item.format, item.slice);
  }

  /* ----- selection ----- */

  function selectOnly(item) {
    A.sel = {};
    if (item) { A.sel[item._id] = true; A.primary = item._id; } else A.primary = null;
    refreshSelection();
    requestDraw();
  }

  function selectRow(item, e) {
    if (e && (e.ctrlKey || e.metaKey)) {
      if (A.sel[item._id]) delete A.sel[item._id]; else A.sel[item._id] = true;
      A.primary = item._id;
    } else if (e && e.shiftKey && A.primary !== null) {
      var a = rowIndex(A.primary), b = rowIndex(item._id);
      if (a >= 0 && b >= 0) {
        A.sel = {};
        for (var i = Math.min(a, b); i <= Math.max(a, b); i++) A.sel[A.rows[i].item._id] = true;
      }
    } else {
      A.sel = {}; A.sel[item._id] = true; A.primary = item._id;
    }
    refreshSelection();
    requestDraw();
  }

  function rowIndex(id) {
    for (var i = 0; i < A.rows.length; i++) if (A.rows[i].item._id === id) return i;
    return -1;
  }

  function scrollToRow(item) {
    var i = rowIndex(item._id);
    if (i < 0) return;
    var r = A.rows[i];
    if (r.top < A.scrollY) A.scrollY = r.top;
    else if (r.top + r.h > A.scrollY + viewH()) A.scrollY = r.top + r.h - viewH();
    clampScroll();
    requestDraw();
  }

  /* ----- editing operations ----- */

  function removeFromTree(ids) {
    ids.forEach(function (id) {
      var loc = findLoc(id);
      if (loc) loc.arr.splice(loc.idx, 1);
    });
  }

  function topLevelSelection(items) {
    return items.filter(function (it) {
      return !items.some(function (o) { return o !== it && o.type === 'group' && containsId(o, it._id); });
    });
  }

  function removeSelected() {
    var sel = topLevelSelection(selectedItems());
    if (!sel.length) return;
    mutate(function () {
      removeFromTree(sel.map(function (i) { return i._id; }));
      A.sel = {}; A.primary = null;
    });
  }

  function moveItems(ids, targetId, zone) {
    var items = topLevelSelection(ids.map(function (id) { var l = findLoc(id); return l && l.item; }).filter(Boolean));
    if (!items.length) return;
    if (items.some(function (i) { return containsId(i, targetId); })) return;
    mutate(function () {
      removeFromTree(items.map(function (i) { return i._id; }));
      var t = findLoc(targetId);
      if (!t) { A.items = A.items.concat(items); return; }
      if (zone === 'into') Array.prototype.push.apply(t.item.items, items);
      else {
        var at = zone === 'before' ? t.idx : t.idx + 1;
        Array.prototype.splice.apply(t.arr, [at, 0].concat(items));
      }
    });
  }

  function reorderSelected(dir) {
    var ids = selectedItems().map(function (i) { return i._id; });
    if (!ids.length) return;
    var is = {};
    ids.forEach(function (id) { is[id] = true; });
    pushUndo();
    var moved = false;
    (function pass(arr) {
      var order = [];
      for (var i = 0; i < arr.length; i++) order.push(i);
      if (dir > 0) order.reverse();
      order.forEach(function (i) {
        var j = i + dir;
        if (is[arr[i]._id] && j >= 0 && j < arr.length && !is[arr[j]._id]) {
          var tmp = arr[i]; arr[i] = arr[j]; arr[j] = tmp; moved = true;
        }
      });
      arr.forEach(function (it) { if (it.type === 'group') pass(it.items); });
    })(A.items);
    if (!moved) A.undo.pop();
    relayout();
    if (A.primary !== null) { var l = findLoc(A.primary); if (l) scrollToRow(l.item); }
  }

  function groupSelected() {
    var sel = topLevelSelection(selectedItems());
    if (!sel.length) { flash('Select signals to group first'); return; }
    mutate(function () {
      var first = findLoc(sel[0]._id);
      var g = { _id: A.nextId++, type: 'group', name: 'group', expanded: true, items: sel.slice() };
      var anchor = first.arr, at = first.idx;
      removeFromTree(sel.map(function (i) { return i._id; }));
      at = Math.min(at, anchor.length);
      anchor.splice(at, 0, g);
      A.sel = {}; A.sel[g._id] = true; A.primary = g._id;
    });
  }

  function ungroup(g) {
    mutate(function () {
      var loc = findLoc(g._id);
      Array.prototype.splice.apply(loc.arr, [loc.idx, 1].concat(g.items));
      A.sel = {}; A.primary = null;
    });
  }

  function applyToSelected(fn) {
    var sigs = selectedSignals();
    if (!sigs.length) return;
    mutate(function () { sigs.forEach(fn); });
  }

  function setColor(c) { applyToSelected(function (it) { it.color = c; }); }

  function setStyle(style, line) {
    applyToSelected(function (it) {
      it.style = style;
      if (style === 'analog') it.lineStyle = line || it.lineStyle || 'step'; else delete it.lineStyle;
    });
  }

  function setHeight(h) {
    applyToSelected(function (it) { if (h) it.height = h; else delete it.height; });
  }

  function itemWidth(item) {
    var sig = sigOf(item);
    return sig ? sliceWidth(sig, item.slice) : 0;
  }

  function splitItem(item, size) {
    var sig = sigOf(item);
    if (!sig || isText(sig)) return;
    var w = itemWidth(item), lsb0 = item.slice ? item.slice[1] : 0, msb0 = lsb0 + w - 1;
    var kids = [];
    for (var hi = msb0; hi >= lsb0; hi -= size) {
      var lo = Math.max(lsb0, hi - size + 1);
      kids.push({
        _id: A.nextId++, type: 'signal', path: item.path, name: null,
        format: size === 1 ? 'binary' : item.format, color: item.color, style: 'digital', slice: [hi, lo]
      });
    }
    var g = { _id: A.nextId++, type: 'group', name: labelOf(item), expanded: true, items: kids };
    var loc = findLoc(item._id);
    loc.arr.splice(loc.idx, 1, g);
  }

  function splitSelected(size) {
    var sigs = selectedSignals().filter(function (i) { return itemWidth(i) > 1 && !isText(sigOf(i) || {}); });
    if (!sigs.length) { flash('Select a multi-bit signal to split'); return; }
    mutate(function () { sigs.forEach(function (i) { splitItem(i, size); }); A.sel = {}; A.primary = null; });
  }

  function newFromRange(item) {
    var sig = sigOf(item);
    if (!sig || isText(sig)) return;
    askText('New signal from bit range', 'Bit range as msb:lsb (or a single bit), 0 to ' + (sig.width - 1), '', function (txt) {
      var m = /^\s*(\d+)\s*(?::\s*(\d+))?\s*$/.exec(txt);
      if (!m) { flash('Not a bit range: ' + txt); return; }
      var msb = parseInt(m[1], 10), lsb = m[2] === undefined ? msb : parseInt(m[2], 10);
      if (msb < lsb || msb >= sig.width) { flash('Bit range outside 0..' + (sig.width - 1)); return; }
      mutate(function () {
        var loc = findLoc(item._id);
        var ni = {
          _id: A.nextId++, type: 'signal', path: item.path, name: null,
          format: msb === lsb ? 'binary' : item.format, color: item.color, style: 'digital', slice: [msb, lsb]
        };
        loc.arr.splice(loc.idx + 1, 0, ni);
        A.sel = {}; A.sel[ni._id] = true; A.primary = ni._id;
      });
    });
  }

  function renameItems(items) {
    var it = items[0];
    if (!it) return;
    askText('Rename', 'Name (empty restores the default)', labelOf(it), function (txt) {
      mutate(function () {
        txt = txt.trim();
        if (it.type === 'group') it.name = txt || 'group'; else it.name = txt || null;
      });
    });
  }

  function addVar(v) {
    var it = newSignalItem(v);
    mutate(function () { A.items.push(it); A.sel = {}; A.sel[it._id] = true; A.primary = it._id; });
    scrollToRow(it);
  }

  function addVars(list) {
    if (!list.length) return;
    mutate(function () {
      list.forEach(function (v) { A.items.push(newSignalItem(v)); });
    });
    flash('Added ' + list.length + ' signal' + (list.length === 1 ? '' : 's'));
  }

  /* ----- view ----- */

  function total() { return D.end - D.start; }
  function minSpan() { return Math.min(4, total()); }

  function setView(s, e) {
    var span = Math.min(Math.max(e - s, minSpan()), total());
    if (s < D.start) s = D.start;
    if (s + span > D.end) s = D.end - span;
    A.vS = s; A.vE = s + span;
    requestDraw();
  }

  function waveW() { return Math.max(1, wave.clientWidth); }
  function tpp() { return (A.vE - A.vS) / waveW(); }
  function timeAt(x) { return A.vS + x * tpp(); }

  function zoomAt(factor, t) {
    var span = A.vE - A.vS, f = (t - A.vS) / span;
    var ns = Math.min(Math.max(span * factor, minSpan()), total());
    setView(t - f * ns, t - f * ns + ns);
  }

  function viewCenter() { return (A.vS + A.vE) / 2; }

  function zoomIn() { zoomAt(0.5, A.marker !== null && A.marker >= A.vS && A.marker <= A.vE ? A.marker : viewCenter()); }
  function zoomOut() { zoomAt(2, A.marker !== null && A.marker >= A.vS && A.marker <= A.vE ? A.marker : viewCenter()); }
  function zoomFit() { setView(D.start, D.end); }

  function ensureVisible(t) {
    if (t >= A.vS && t <= A.vE) return;
    var span = A.vE - A.vS;
    setView(t - span / 2, t - span / 2 + span);
  }

  function clampT(t) { return Math.min(Math.max(Math.round(t), D.start), D.end); }

  function setMarker(which, t) {
    A[which] = t === null ? null : clampT(t);
    updateValues();
    updateStatus();
    requestDraw();
  }

  function stepTicks() { return Math.max(1, Math.round(ticksPerUnit(TS, A.unit))); }

  function moveMarker(which, dir) {
    var item = primarySignal();
    var cur = A[which];
    if (cur === null) cur = which === 'altMarker' && A.marker !== null ? A.marker : A.vS;
    var t;
    if (item) {
      t = findChange(sigOf(item), cur, dir, item.slice);
      if (t === null) { flash('No ' + (dir > 0 ? 'next' : 'previous') + ' change of ' + labelOf(item)); return; }
    } else t = cur + dir * stepTicks();
    setMarker(which, t);
    ensureVisible(A[which]);
  }

  function edgeJump(kind, dir) {
    var item = primarySignal();
    if (!item || itemWidth(item) !== 1 || isText(sigOf(item))) { flash('Select a 1-bit signal first'); return; }
    var cur = A.marker === null ? A.vS - 1 : A.marker;
    var t = findEdge(sigOf(item), cur, dir, kind, item.slice);
    if (t === null) { flash('No ' + (dir > 0 ? 'next ' : 'previous ') + kind + ' edge'); return; }
    setMarker('marker', t);
    ensureVisible(t);
  }

  function valueSearch(dir) {
    var q = $('search').value;
    var item = primarySignal();
    if (!item) { flash('Select a signal to search'); return; }
    if (!q.trim()) { flash('Type a value to search for'); return; }
    var cur = A.marker === null ? A.vS - 1 : A.marker;
    var t = searchValue(sigOf(item), item.format, item.slice, q, cur, dir);
    if (t === null) { flash('"' + q + '" not found ' + (dir > 0 ? 'after' : 'before') + ' the marker'); return; }
    setMarker('marker', t);
    ensureVisible(t);
  }

  /* ----- status ----- */

  function fmtT(t) { return formatTime(t, TS, A.unit); }

  function updateStatus() {
    $('st-marker').textContent = 'Marker: ' + (A.marker === null ? '-' : fmtT(A.marker));
    $('st-alt').textContent = 'Alt: ' + (A.altMarker === null ? '-' : fmtT(A.altMarker));
    var d = '';
    if (A.marker !== null && A.altMarker !== null) {
      var dt = Math.abs(A.marker - A.altMarker);
      d = '\u0394 ' + fmtT(dt);
      if (dt > 0) {
        var f = formatFrequency(dt * TS[0] * Math.pow(10, UNIT_EXP[TS[1]]));
        if (f) d += ' (' + f + ')';
      }
    }
    $('st-delta').textContent = d;
    $('st-cursor').textContent = A.cursor === null ? '' : 'Cursor: ' + fmtT(A.cursor);
  }

  function flash(msg) {
    var el = $('st-msg');
    el.textContent = msg;
    clearTimeout(A.msgTimer);
    A.msgTimer = setTimeout(function () { el.textContent = ''; }, 6000);
  }

  /* ----- drawing ----- */

  function readTheme() {
    var cs = getComputedStyle(document.documentElement);
    var g = function (n) { return cs.getPropertyValue(n).trim(); };
    A.theme = {
      bg: g('--wave-bg'), fg: g('--fg'), muted: g('--muted'), grid: g('--grid'), rowline: g('--rowline'),
      sel: g('--sel-wave'), marker: g('--marker'), alt: g('--alt'), x: g('--x'), z: g('--z'),
      ruler: g('--ruler-bg'), c: [null]
    };
    for (var i = 1; i <= COLOR_COUNT; i++) A.theme.c.push(g('--c' + i));
    var pc = document.createElement('canvas');
    pc.width = pc.height = 8;
    var pg = pc.getContext('2d');
    pg.strokeStyle = A.theme.x;
    pg.lineWidth = 1;
    pg.beginPath();
    pg.moveTo(0, 8); pg.lineTo(8, 0);
    pg.moveTo(-2, 2); pg.lineTo(2, -2);
    pg.moveTo(6, 10); pg.lineTo(10, 6);
    pg.stroke();
    A.hatch = ctx.createPattern(pc, 'repeat');
  }

  function requestDraw() {
    if (A.pending) return;
    A.pending = true;
    requestAnimationFrame(draw);
  }

  function fitText(str, maxW) {
    var w = ctx.measureText(str).width;
    if (w <= maxW) return str;
    var n = Math.floor(str.length * maxW / w) - 1;
    while (n > 0 && ctx.measureText(str.slice(0, n) + '\u2026').width > maxW) n--;
    return n > 0 ? str.slice(0, n) + '\u2026' : '';
  }

  function draw() {
    A.pending = false;
    var cw = wave.clientWidth, ch = wave.clientHeight;
    if (!cw || !ch) return;
    if (!A.theme) readTheme();
    var dpr = window.devicePixelRatio || 1;
    if (wave.width !== Math.round(cw * dpr) || wave.height !== Math.round(ch * dpr)) {
      wave.width = Math.round(cw * dpr);
      wave.height = Math.round(ch * dpr);
    }
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    var T = A.theme, pp = tpp();
    ctx.fillStyle = T.bg;
    ctx.fillRect(0, 0, cw, ch);
    ctx.font = FONT;
    ctx.textBaseline = 'middle';

    var step = niceStep(pp * 100);
    var first = Math.ceil(A.vS / step) * step;
    ctx.strokeStyle = T.grid;
    ctx.lineWidth = 1;
    ctx.beginPath();
    for (var gt = first; gt <= A.vE; gt += step) {
      var gx = Math.round((gt - A.vS) / pp) + 0.5;
      ctx.moveTo(gx, RULER_H); ctx.lineTo(gx, ch);
    }
    ctx.stroke();

    ctx.save();
    ctx.beginPath();
    ctx.rect(0, RULER_H, cw, ch - RULER_H);
    ctx.clip();
    for (var i = 0; i < A.rows.length; i++) {
      var row = A.rows[i], y = RULER_H + row.top - A.scrollY;
      if (y + row.h < RULER_H || y > ch) continue;
      if (A.sel[row.item._id]) { ctx.fillStyle = T.sel; ctx.fillRect(0, y, cw, row.h); }
      if (row.item.type === 'group') {
        ctx.fillStyle = T.rowline; ctx.globalAlpha = 0.35; ctx.fillRect(0, y, cw, row.h); ctx.globalAlpha = 1;
      } else drawSignalRow(row.item, y, row.h, cw, pp);
      ctx.strokeStyle = T.rowline;
      ctx.beginPath(); ctx.moveTo(0, y + row.h - 0.5); ctx.lineTo(cw, y + row.h - 0.5); ctx.stroke();
    }
    ctx.restore();

    drawRuler(cw, step, first, pp);
    if (A.altMarker !== null) drawMarker(A.altMarker, T.alt, true, ch, pp);
    if (A.marker !== null) drawMarker(A.marker, T.marker, false, ch, pp);
    if (A.drag && A.drag.moved) {
      var x0 = Math.min(A.drag.x0, A.drag.x), x1 = Math.max(A.drag.x0, A.drag.x);
      ctx.fillStyle = T.sel;
      ctx.fillRect(x0, RULER_H, x1 - x0, ch - RULER_H);
      ctx.strokeStyle = T.marker;
      ctx.strokeRect(x0 + 0.5, RULER_H + 0.5, x1 - x0, ch - RULER_H - 1);
    }
  }

  function drawRuler(cw, step, first, pp) {
    var T = A.theme;
    ctx.fillStyle = T.ruler;
    ctx.fillRect(0, 0, cw, RULER_H);
    ctx.strokeStyle = T.rowline;
    ctx.beginPath(); ctx.moveTo(0, RULER_H - 0.5); ctx.lineTo(cw, RULER_H - 0.5); ctx.stroke();
    ctx.fillStyle = T.muted;
    ctx.textAlign = 'left';
    ctx.beginPath();
    ctx.strokeStyle = T.muted;
    for (var t = first; t <= A.vE; t += step) {
      var x = Math.round((t - A.vS) / pp) + 0.5;
      ctx.moveTo(x, RULER_H - 6); ctx.lineTo(x, RULER_H - 1);
      ctx.fillText(fmtT(t), x + 3, RULER_H - 15);
    }
    ctx.stroke();
  }

  function drawMarker(t, color, dashed, ch, pp) {
    var x = (t - A.vS) / pp;
    if (x < -1 || x > wave.clientWidth + 1) return;
    x = Math.round(x) + 0.5;
    ctx.strokeStyle = color;
    ctx.lineWidth = 1;
    if (dashed) ctx.setLineDash([5, 3]);
    ctx.beginPath(); ctx.moveTo(x, 0); ctx.lineTo(x, ch); ctx.stroke();
    ctx.setLineDash([]);
    var label = fmtT(t), w = ctx.measureText(label).width + 8;
    var lx = Math.min(Math.max(x - w / 2, 0), wave.clientWidth - w);
    ctx.fillStyle = color;
    ctx.fillRect(lx, 1, w, 14);
    ctx.fillStyle = '#fff';
    ctx.textAlign = 'left';
    ctx.fillText(label, lx + 4, 8.5);
  }

  function drawSignalRow(item, y, h, cw, pp) {
    var sig = sigOf(item);
    if (!sig) return;
    var color = A.theme.c[item.color] || A.theme.c[1];
    var width = sliceWidth(sig, item.slice);
    if (isAnalog(item)) drawAnalog(item, sig, y, h, color, pp);
    else if (width === 1 && !isText(sig)) drawBit(item, sig, y, h, color, pp);
    else drawBus(item, sig, y, h, color, pp);
  }

  function drawBit(item, sig, y, h, color, pp) {
    var T = A.theme, hi = y + 6, lo = y + h - 7, mid = (hi + lo) / 2;
    var main = new Path2D(), zp = new Path2D(), bands = [], prev = null;
    scanSegments(sig.t, A.vS, A.vE, pp, function (k, xs, xe, clipL) {
      var c = keyAt(sig, k, item.slice).charAt(0), lvl;
      if (isHigh(c)) lvl = hi;
      else if (isLow(c)) lvl = lo;
      else if (c === 'z') { zp.moveTo(xs, mid); zp.lineTo(xe, mid); prev = null; return; }
      else { bands.push([xs, xe]); prev = null; return; }
      if (prev !== null && prev !== lvl && !clipL) { main.moveTo(xs, prev); main.lineTo(xs, lvl); }
      main.moveTo(xs, lvl); main.lineTo(xe, lvl);
      prev = lvl;
    }, function (px) {
      main.moveTo(px + 0.5, hi); main.lineTo(px + 0.5, lo);
      prev = null;
    });
    bands.forEach(function (b) {
      var bw = Math.max(b[1] - b[0], 1);
      ctx.globalAlpha = 0.3; ctx.fillStyle = T.x; ctx.fillRect(b[0], hi, bw, lo - hi);
      ctx.globalAlpha = 1; ctx.fillStyle = A.hatch; ctx.fillRect(b[0], hi, bw, lo - hi);
      ctx.strokeStyle = T.x; ctx.lineWidth = 1;
      ctx.strokeRect(b[0] + 0.5, hi + 0.5, bw, lo - hi - 1);
    });
    ctx.lineWidth = 1.5;
    ctx.strokeStyle = color; ctx.stroke(main);
    ctx.strokeStyle = T.z; ctx.stroke(zp);
    ctx.lineWidth = 1;
  }

  function drawBus(item, sig, y, h, color, pp) {
    var T = A.theme, top = y + 5, bot = y + h - 6, mid = (top + bot) / 2;
    ctx.lineWidth = 1.2;
    scanSegments(sig.t, A.vS, A.vE, pp, function (k, xs, xe, clipL, clipR) {
      var key = keyAt(sig, k, item.slice);
      var kind = (isText(sig) || isBin(key)) ? 'ok' : (/^z+$/.test(key) ? 'z' : 'x');
      if (kind === 'z') {
        ctx.strokeStyle = T.z; ctx.lineWidth = 1.5;
        ctx.beginPath(); ctx.moveTo(xs, mid); ctx.lineTo(xe, mid); ctx.stroke();
        ctx.lineWidth = 1.2;
        return;
      }
      var col = kind === 'x' ? T.x : color;
      var s = Math.min(4, (xe - xs) / 2), sl = clipL ? 0 : s, sr = clipR ? 0 : s;
      ctx.beginPath();
      ctx.moveTo(xs + sl, top);
      ctx.lineTo(xe - sr, top);
      if (sr > 0) ctx.lineTo(xe, mid);
      ctx.lineTo(xe - sr, bot);
      ctx.lineTo(xs + sl, bot);
      if (sl > 0) ctx.lineTo(xs, mid);
      ctx.closePath();
      ctx.globalAlpha = kind === 'x' ? 0.3 : 0.2; ctx.fillStyle = col; ctx.fill();
      ctx.globalAlpha = 1;
      if (kind === 'x') { ctx.fillStyle = A.hatch; ctx.fill(); }
      ctx.strokeStyle = col; ctx.stroke();
      var vx0 = Math.max(xs, 0), vx1 = xe;
      if (vx1 - vx0 > 18) {
        var txt = fitText(formatValue(sig, sig.v[k], item.format, item.slice), vx1 - vx0 - 8 - sl - sr);
        if (txt) {
          ctx.fillStyle = T.fg; ctx.textAlign = 'center';
          ctx.fillText(txt, (vx0 + vx1) / 2, mid + 0.5);
          ctx.textAlign = 'left';
        }
      }
    }, function (px) {
      ctx.globalAlpha = 0.6; ctx.fillStyle = color;
      ctx.fillRect(px, top, 1, bot - top);
      ctx.globalAlpha = 1;
    });
    ctx.lineWidth = 1;
  }

  function drawAnalog(item, sig, y, h, color, pp) {
    var T = A.theme, top = y + 6, bot = y + h - 6;
    var num = numericCache(sig, item), vals = num.vals, range = num.max - num.min;
    var Y = function (v) { return bot - (v - num.min) / range * (bot - top); };
    var linear = item.lineStyle === 'linear';
    var path = new Path2D(), have = false, prevY = 0, errs = [], lastY = 0, lastX = 0;
    scanSegments(sig.t, A.vS, A.vE, pp, function (k, xs, xe) {
      var v = vals[k];
      if (!isFinite(v)) { errs.push([xs, xe]); have = false; return; }
      var yv = Y(v);
      if (linear) {
        if (!have) path.moveTo(xs, yv); else path.lineTo(xs, yv);
      } else if (!have) path.moveTo(xs, yv);
      else { path.lineTo(xs, prevY); path.lineTo(xs, yv); }
      if (!linear) path.lineTo(xe, yv);
      have = true; prevY = yv; lastY = yv; lastX = xe;
    }, function (px, kFrom, kTo) {
      var lo = Infinity, hi = -Infinity, st = Math.max(1, Math.floor((kTo - kFrom) / 48));
      for (var j = kFrom; j <= kTo; j += st) {
        var v = vals[j];
        if (isFinite(v)) { if (v < lo) lo = v; if (v > hi) hi = v; }
      }
      if (hi >= lo) { path.moveTo(px + 0.5, Y(lo)); path.lineTo(px + 0.5, Y(hi)); }
      have = false;
    });
    if (linear && have) path.lineTo(lastX, lastY);
    ctx.strokeStyle = T.rowline;
    ctx.beginPath(); ctx.moveTo(0, top + 0.5); ctx.lineTo(wave.clientWidth, top + 0.5);
    ctx.moveTo(0, bot + 0.5); ctx.lineTo(wave.clientWidth, bot + 0.5); ctx.stroke();
    ctx.lineWidth = 1.5; ctx.strokeStyle = color; ctx.stroke(path);
    ctx.strokeStyle = T.x;
    errs.forEach(function (e) {
      ctx.beginPath(); ctx.moveTo(e[0], bot + 0.5); ctx.lineTo(Math.max(e[1], e[0] + 1), bot + 0.5); ctx.stroke();
    });
    ctx.lineWidth = 1;
    ctx.fillStyle = T.muted; ctx.textAlign = 'left';
    var isNum = sig.real || item.format === 'float';
    var lab = function (v) { return isNum ? formatG(v) : String(v); };
    ctx.fillText(lab(num.max), 3, top + 7);
    ctx.fillText(lab(num.min), 3, bot - 7);
  }

  /* ----- canvas mouse ----- */

  function rowAtY(y) {
    var yy = y - RULER_H + A.scrollY;
    for (var i = 0; i < A.rows.length; i++) {
      if (yy >= A.rows[i].top && yy < A.rows[i].top + A.rows[i].h) return A.rows[i];
    }
    return null;
  }

  function snap(row, t) {
    if (!row || row.item.type !== 'signal') return t;
    var sig = sigOf(row.item);
    if (!sig) return t;
    var i = lastIdxLE(sig.t, t), best = t, bd = 4 * tpp();
    [i, i + 1].forEach(function (j) {
      if (j >= 0 && j < sig.t.length && Math.abs(sig.t[j] - t) <= bd) { bd = Math.abs(sig.t[j] - t); best = sig.t[j]; }
    });
    return best;
  }

  function localXY(e) {
    var r = wave.getBoundingClientRect();
    return { x: e.clientX - r.left, y: e.clientY - r.top };
  }

  function startRulerDrag(which) {
    var move = function (ev) { setMarker(which, timeAt(localXY(ev).x)); };
    var up = function () { window.removeEventListener('mousemove', move); window.removeEventListener('mouseup', up); };
    window.addEventListener('mousemove', move);
    window.addEventListener('mouseup', up);
    return move;
  }

  wave.addEventListener('mousedown', function (e) {
    var p = localXY(e);
    if (e.button === 1) { e.preventDefault(); setMarker('altMarker', snap(rowAtY(p.y), timeAt(p.x))); return; }
    if (e.button !== 0) return;
    if (p.y < RULER_H) {
      var which = e.altKey ? 'altMarker' : 'marker';
      startRulerDrag(which)(e);
      return;
    }
    var row = rowAtY(p.y);
    if (e.altKey) { setMarker('altMarker', snap(row, timeAt(p.x))); return; }
    var d = { x0: p.x, x: p.x, moved: false, row: row, ctrl: e.ctrlKey || e.metaKey, shift: e.shiftKey };
    A.drag = d;
    var move = function (ev) {
      if (A.drag !== d) return;
      d.x = Math.min(Math.max(localXY(ev).x, 0), waveW());
      if (Math.abs(d.x - d.x0) > 4) d.moved = true;
      if (d.moved) requestDraw();
    };
    var up = function () {
      window.removeEventListener('mousemove', move);
      window.removeEventListener('mouseup', up);
      if (A.drag !== d) return;
      A.drag = null;
      if (d.moved) {
        var t0 = timeAt(Math.min(d.x0, d.x)), t1 = timeAt(Math.max(d.x0, d.x));
        setView(t0, t1);
      } else {
        if (d.row) selectRow(d.row.item, { ctrlKey: d.ctrl, shiftKey: d.shift });
        setMarker('marker', snap(d.row, timeAt(d.x0)));
      }
      requestDraw();
    };
    window.addEventListener('mousemove', move);
    window.addEventListener('mouseup', up);
  });

  wave.addEventListener('mousemove', function (e) {
    A.cursor = clampT(timeAt(localXY(e).x));
    updateStatus();
  });
  wave.addEventListener('mouseleave', function () { A.cursor = null; updateStatus(); });
  wave.addEventListener('auxclick', function (e) { e.preventDefault(); });

  wave.addEventListener('contextmenu', function (e) {
    e.preventDefault();
    var row = rowAtY(localXY(e).y);
    if (!row) return;
    if (!A.sel[row.item._id]) selectOnly(row.item);
    rowMenu(row.item, e.clientX, e.clientY);
  });

  function onWheel(e) {
    e.preventDefault();
    var dy = e.deltaMode === 1 ? e.deltaY * 16 : e.deltaY;
    var dx = e.deltaMode === 1 ? e.deltaX * 16 : e.deltaX;
    if (e.ctrlKey || e.metaKey) {
      var f = Math.pow(1.002, dy);
      zoomAt(f, timeAt(localXY(e).x));
    } else if (e.shiftKey) {
      A.scrollY += dy || dx;
      clampScroll();
      requestDraw();
    } else {
      var d = Math.abs(dx) > Math.abs(dy) ? dx : dy;
      var pp = tpp();
      setView(A.vS + d * pp, A.vE + d * pp);
    }
  }
  wave.addEventListener('wheel', onWheel, { passive: false });
  $('nameclip').addEventListener('wheel', function (e) {
    e.preventDefault();
    A.scrollY += e.deltaMode === 1 ? e.deltaY * 16 : e.deltaY;
    clampScroll();
    requestDraw();
  }, { passive: false });
  $('nameclip').addEventListener('mousedown', function (e) {
    if (e.target === $('nameclip') && e.button === 0) selectOnly(null);
  });

  /* ----- modals, menus ----- */

  function closeModal() {
    if (A.modal) { A.modal.remove(); A.modal = null; }
  }

  function openModal(title, body, buttons) {
    closeModal();
    var ov = document.createElement('div');
    ov.className = 'overlay';
    var box = document.createElement('div');
    box.className = 'modal';
    var h = document.createElement('h2');
    h.textContent = title;
    box.appendChild(h);
    box.appendChild(body);
    var foot = document.createElement('div');
    foot.className = 'mfoot';
    buttons.forEach(function (b) {
      var btn = document.createElement('button');
      btn.textContent = b.label;
      if (b.primary) btn.className = 'primary';
      btn.addEventListener('click', function () {
        if (!b.onClick || b.onClick() !== false) closeModal();
      });
      foot.appendChild(btn);
    });
    box.appendChild(foot);
    ov.appendChild(box);
    ov.addEventListener('mousedown', function (e) { if (e.target === ov) closeModal(); });
    document.body.appendChild(ov);
    A.modal = ov;
    return box;
  }

  function askText(title, label, value, cb) {
    var body = document.createElement('div');
    var lab = document.createElement('label');
    lab.textContent = label;
    var inp = document.createElement('input');
    inp.type = 'text';
    inp.value = value;
    inp.className = 'wide';
    body.appendChild(lab);
    body.appendChild(inp);
    var go = function () { var v = inp.value; closeModal(); cb(v); return false; };
    openModal(title, body, [{ label: 'Cancel' }, { label: 'OK', primary: true, onClick: go }]);
    inp.addEventListener('keydown', function (e) { if (e.key === 'Enter') go(); });
    inp.focus();
    inp.select();
  }

  function showCopyModal(text, what) {
    var body = document.createElement('div');
    var p = document.createElement('p');
    p.textContent = 'The browser blocked clipboard access. The ' + what + ' is selected below; press Ctrl+C to copy it.';
    var ta = document.createElement('textarea');
    ta.className = 'copybox';
    ta.readOnly = true;
    ta.value = text;
    body.appendChild(p);
    body.appendChild(ta);
    openModal('Copy ' + what, body, [{ label: 'Close', primary: true }]);
    ta.focus();
    ta.select();
    try { if (document.execCommand('copy')) p.textContent = 'Copied. The ' + what + ' is also shown here.'; } catch (err) { /* manual copy */ }
  }

  function copyText(text, what) {
    try {
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(text).then(function () { flash(what + ' copied to the clipboard'); },
          function () { showCopyModal(text, what); });
        return;
      }
    } catch (err) { /* fall through */ }
    showCopyModal(text, what);
  }

  function closeMenu() {
    if (A.menu) { A.menu.remove(); A.menu = null; }
  }

  function buildMenu(entries) {
    var m = document.createElement('div');
    m.className = 'menu';
    entries.forEach(function (en) {
      if (en.sep) { var s = document.createElement('div'); s.className = 'sep'; m.appendChild(s); return; }
      var it = document.createElement('div');
      it.className = 'mi' + (en.disabled ? ' disabled' : '') + (en.items ? ' has-sub' : '');
      if (en.swatch) {
        var sw = document.createElement('span');
        sw.className = 'swatch';
        sw.style.background = colorVar(en.swatch);
        it.appendChild(sw);
      }
      var lb = document.createElement('span');
      lb.textContent = (en.checked ? '\u2713 ' : '') + en.label;
      it.appendChild(lb);
      if (en.items) {
        var sub = buildMenu(en.items);
        sub.classList.add('sub');
        it.appendChild(sub);
      } else if (!en.disabled) {
        it.addEventListener('click', function (e) { e.stopPropagation(); closeMenu(); en.action(); });
      }
      m.appendChild(it);
    });
    return m;
  }

  function showMenu(x, y, entries) {
    closeMenu();
    var m = buildMenu(entries);
    m.classList.add('root');
    m.style.left = x + 'px';
    m.style.top = y + 'px';
    document.body.appendChild(m);
    var r = m.getBoundingClientRect();
    if (r.right > window.innerWidth) m.style.left = Math.max(0, window.innerWidth - r.width - 4) + 'px';
    if (r.bottom > window.innerHeight) m.style.top = Math.max(0, window.innerHeight - r.height - 4) + 'px';
    if (r.right + 180 > window.innerWidth) m.classList.add('flip');
    A.menu = m;
  }

  document.addEventListener('mousedown', function (e) {
    if (A.menu && !A.menu.contains(e.target)) closeMenu();
  });

  function rowMenu(item, x, y) {
    if (item.type === 'group') {
      showMenu(x, y, [
        { label: 'Rename...', action: function () { renameItems([item]); } },
        { label: item.expanded ? 'Collapse' : 'Expand', action: function () { mutate(function () { item.expanded = !item.expanded; }); } },
        { label: 'Ungroup', action: function () { ungroup(item); } },
        { sep: true },
        { label: 'Remove', action: removeSelected }
      ]);
      return;
    }
    var cur = item;
    var fm = FORMATS.map(function (f) {
      return { label: f, checked: cur.format === f, action: function () { applyToSelected(function (it) { it.format = f; }); } };
    });
    var cm = [];
    for (var c = 1; c <= COLOR_COUNT; c++) {
      (function (c) {
        cm.push({ label: 'Color ' + c, swatch: c, checked: cur.color === c, action: function () { setColor(c); } });
      })(c);
    }
    var hm = [20, 28, 40, 56, 80, 120, 200].map(function (h) {
      return { label: h + ' px', checked: cur.height === h, action: function () { setHeight(h); } };
    });
    hm.unshift({ label: 'Default', action: function () { setHeight(0); } });
    var multi = itemWidth(cur) > 1 && !isText(sigOf(cur) || {});
    showMenu(x, y, [
      { label: 'Format', items: fm },
      { label: 'Color', items: cm },
      { label: 'Rename...', action: function () { renameItems([cur]); } },
      { label: 'Style', items: [
        { label: 'Digital', checked: cur.style === 'digital', action: function () { setStyle('digital'); } },
        { label: 'Analog (step)', checked: cur.style === 'analog' && cur.lineStyle !== 'linear', action: function () { setStyle('analog', 'step'); } },
        { label: 'Analog (linear)', checked: cur.style === 'analog' && cur.lineStyle === 'linear', action: function () { setStyle('analog', 'linear'); } }
      ] },
      { label: 'Row height', items: hm },
      { sep: true },
      { label: 'Split into bits', disabled: !multi, action: function () { splitSelected(1); } },
      { label: 'Split into nibbles', disabled: !multi, action: function () { splitSelected(4); } },
      { label: 'Split into bytes', disabled: !multi, action: function () { splitSelected(8); } },
      { label: 'New signal from bit range...', disabled: !multi && itemWidth(cur) <= 1, action: function () { newFromRange(cur); } },
      { sep: true },
      { label: 'Remove', action: removeSelected }
    ]);
  }

  /* ----- netlist ----- */

  function scopeKey(path) { return path.join('.'); }

  function initExpanded() {
    (D.scope.children || []).forEach(function (c) { A.expanded[c.name] = true; });
    (function chain(s, path) {
      if ((s.children || []).length === 1 && !(s.vars || []).length) {
        var p = path.concat([s.children[0].name]);
        A.expanded[scopeKey(p)] = true;
        chain(s.children[0], p);
      }
    })(D.scope, []);
  }

  function scopeVarsDeep(s) {
    var out = (s.vars || []).map(function (i) { return D.vars[i]; });
    (s.children || []).forEach(function (c) { out = out.concat(scopeVarsDeep(c)); });
    return out;
  }

  function buildTree() {
    var tree = $('tree'), keepTop = tree.scrollTop;
    tree.textContent = '';
    var terms = filter.value.trim().toLowerCase().split(/\s+/).filter(Boolean);
    var budget = { n: 1500, cut: 0 };
    var matchVar = function (v) {
      var p = v.path.toLowerCase();
      return terms.every(function (t) { return p.indexOf(t) >= 0; });
    };
    var hasMatch = function (s) {
      if ((s.vars || []).some(function (i) { return matchVar(D.vars[i]); })) return true;
      return (s.children || []).some(hasMatch);
    };
    function renderScope(s, path, depth, parent) {
      if (terms.length && !hasMatch(s)) return;
      var key = scopeKey(path), open = terms.length ? true : !!A.expanded[key];
      var node = document.createElement('div');
      node.className = 'node';
      var head = document.createElement('div');
      head.className = 'nhead';
      head.style.paddingLeft = (4 + depth * 12) + 'px';
      head.innerHTML = '<span class="twisty"></span><span class="sname"></span><span class="kind"></span>';
      head.children[0].textContent = open ? '\u25be' : '\u25b8';
      head.children[1].textContent = s.name;
      head.children[2].textContent = s.type;
      head.addEventListener('click', function () { A.expanded[key] = !open; buildTree(); });
      head.addEventListener('contextmenu', function (e) {
        e.preventDefault();
        showMenu(e.clientX, e.clientY, [
          { label: 'Add all in scope', action: function () { addVars((s.vars || []).map(function (i) { return D.vars[i]; })); } },
          { label: 'Add all including subscopes', action: function () { addVars(scopeVarsDeep(s)); } }
        ]);
      });
      node.appendChild(head);
      if (open) {
        (s.children || []).forEach(function (c) { renderScope(c, path.concat([c.name]), depth + 1, node); });
        (s.vars || []).forEach(function (i) {
          var v = D.vars[i];
          if (terms.length && !matchVar(v)) return;
          if (budget.n <= 0) { budget.cut++; return; }
          budget.n--;
          var row = document.createElement('div');
          row.className = 'nvar';
          row.style.paddingLeft = (20 + depth * 12) + 'px';
          row.title = v.path;
          var nm = document.createElement('span');
          nm.className = 'vname';
          nm.textContent = v.name + (v.range || '');
          var meta = document.createElement('span');
          meta.className = 'kind';
          meta.textContent = v.type + ' ' + v.width;
          var add = document.createElement('button');
          add.className = 'addbtn';
          add.textContent = '+';
          add.title = 'Add to waveform';
          add.addEventListener('click', function (e) { e.stopPropagation(); addVar(v); });
          row.appendChild(nm); row.appendChild(meta); row.appendChild(add);
          row.addEventListener('dblclick', function () { addVar(v); });
          row.addEventListener('click', function () {
            var prev = tree.querySelector('.nvar.cur');
            if (prev) prev.classList.remove('cur');
            row.classList.add('cur');
          });
          row.addEventListener('contextmenu', function (e) {
            e.preventDefault();
            showMenu(e.clientX, e.clientY, [{ label: 'Add to waveform', action: function () { addVar(v); } }]);
          });
          node.appendChild(row);
        });
      }
      parent.appendChild(node);
    }
    (D.scope.children || []).forEach(function (c) { renderScope(c, [c.name], 0, tree); });
    (D.scope.vars || []).forEach(function (i) {
      var v = D.vars[i];
      if (terms.length && !matchVar(v)) return;
      var row = document.createElement('div');
      row.className = 'nvar';
      row.textContent = v.name + (v.range || '');
      row.addEventListener('dblclick', function () { addVar(v); });
      tree.appendChild(row);
    });
    if (budget.cut) {
      var more = document.createElement('div');
      more.className = 'more';
      more.textContent = budget.cut + ' more signals hidden. Narrow the filter.';
      tree.appendChild(more);
    }
    tree.scrollTop = keepTop;
  }

  filter.addEventListener('input', buildTree);
  filter.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') { filter.value = ''; buildTree(); filter.blur(); }
  });

  /* ----- session ----- */

  function currentSession() {
    return {
      file: D.file, marker: A.marker, altMarker: A.altMarker,
      viewStart: A.vS, viewEnd: A.vE, timeUnit: A.unit, items: A.items
    };
  }

  function download(name, text) {
    var blob = new Blob([text], { type: 'application/json' });
    var url = URL.createObjectURL(blob);
    var a = document.createElement('a');
    a.href = url;
    a.download = name;
    document.body.appendChild(a);
    a.click();
    setTimeout(function () { a.remove(); URL.revokeObjectURL(url); }, 1000);
  }

  function saveSession() {
    var base = String(D.file || 'waves').replace(/\.[^.]*$/, '');
    download(base + '.session.json', sessionToJSON(currentSession()));
    flash('Session saved to your downloads folder');
  }

  function pruneMissing(items, counter) {
    return items.filter(function (it) {
      if (it.type === 'group') { it.items = pruneMissing(it.items, counter); return true; }
      if (!varByPath[it.path]) { counter.n++; return false; }
      return true;
    });
  }

  function applySession(s, record) {
    var counter = { n: 0 };
    if (record) pushUndo();
    A.items = pruneMissing(s.items, counter);
    assignIds(A.items);
    A.sel = {}; A.primary = null;
    A.marker = s.marker === null ? null : clampT(s.marker);
    A.altMarker = s.altMarker === null ? null : clampT(s.altMarker);
    if (s.timeUnit) { A.unit = s.timeUnit; $('unit').value = A.unit; }
    if (s.viewStart !== null && s.viewEnd !== null && s.viewEnd > s.viewStart) setView(s.viewStart, s.viewEnd);
    else zoomFit();
    relayout();
    updateStatus();
    return counter.n;
  }

  function loadSessionText(text) {
    try {
      var s = sessionFromJSON(text);
      var missing = applySession(s, true);
      var msg = 'Session loaded';
      if (s.file && D.file && s.file !== D.file) msg += ' (saved for ' + s.file + ')';
      if (missing) msg += '; ' + missing + ' signal' + (missing === 1 ? '' : 's') + ' not in this dump';
      flash(msg);
    } catch (err) { flash('Could not load session: ' + err.message); }
  }

  function loadSessionFile(file) {
    var rd = new FileReader();
    rd.onload = function () { loadSessionText(String(rd.result)); };
    rd.onerror = function () { flash('Could not read ' + file.name); };
    rd.readAsText(file);
  }

  $('sessionfile').addEventListener('change', function (e) {
    if (e.target.files && e.target.files[0]) loadSessionFile(e.target.files[0]);
    e.target.value = '';
  });
  document.addEventListener('dragover', function (e) {
    if (e.dataTransfer && Array.prototype.indexOf.call(e.dataTransfer.types || [], 'Files') >= 0) e.preventDefault();
  });
  document.addEventListener('drop', function (e) {
    if (e.dataTransfer && e.dataTransfer.files && e.dataTransfer.files.length) {
      e.preventDefault();
      loadSessionFile(e.dataTransfer.files[0]);
    }
  });

  /* ----- WaveDrom dialog ----- */

  function allSignalItems() {
    var out = [];
    A.rows.forEach(function (r) { if (r.item.type === 'signal' && sigOf(r.item)) out.push(r.item); });
    return out;
  }

  function waveDromDialog() {
    if (A.marker === null || A.altMarker === null) { flash('Place the marker and the alt marker first (Alt+click)'); return; }
    var items = allSignalItems();
    if (!items.length) { flash('No signals to export'); return; }
    var ones = items.filter(function (i) { return itemWidth(i) === 1 && !isText(sigOf(i)); });
    var body = document.createElement('div');
    body.className = 'wdform';
    var mk = function (html) { var d = document.createElement('div'); d.innerHTML = html; return d; };
    var r1 = mk('<label><input type="radio" name="wdmode" value="free" checked> Unclocked: every change is an event</label>');
    var r2 = mk('<label><input type="radio" name="wdmode" value="clk"> Clocked: sample on clock edges</label>');
    var sel = document.createElement('select');
    ones.forEach(function (i, n) { var o = document.createElement('option'); o.value = n; o.textContent = labelOf(i); sel.appendChild(o); });
    var edge = document.createElement('select');
    edge.innerHTML = '<option value="rise">rising edge</option><option value="fall">falling edge</option>';
    var row = document.createElement('div');
    row.className = 'indent';
    row.appendChild(sel); row.appendChild(edge);
    if (!ones.length) { r2.querySelector('input').disabled = true; row.style.display = 'none'; }
    var note = document.createElement('p');
    note.className = 'hint';
    note.textContent = 'Range ' + fmtT(Math.min(A.marker, A.altMarker)) + ' to ' + fmtT(Math.max(A.marker, A.altMarker)) + ', at most 32 events.';
    body.appendChild(r1); body.appendChild(r2); body.appendChild(row); body.appendChild(note);
    openModal('Copy as WaveDrom', body, [
      { label: 'Cancel' },
      { label: 'Copy', primary: true, onClick: function () {
        var clocked = r2.querySelector('input').checked;
        var lanes = items.map(function (i) {
          return { name: labelOf(i), sig: sigOf(i), fmt: i.format, slice: i.slice, clock: false, item: i };
        });
        var clock = null;
        if (clocked) {
          var ck = ones[parseInt(sel.value, 10)];
          lanes.forEach(function (l) { if (l.item === ck) { l.clock = true; clock = { lane: l, edge: edge.value }; } });
        }
        var t0 = Math.min(A.marker, A.altMarker), t1 = Math.max(A.marker, A.altMarker);
        var res = waveDrom(lanes, t0, t1, clock, 32);
        copyText(JSON.stringify(res.doc, null, 2), 'WaveDrom JSON');
        if (res.truncated) flash('WaveDrom output limited to 32 events');
      } }
    ]);
  }

  /* ----- help ----- */

  var HELP = [
    ['Left / Right, n / N', 'Marker to previous / next change of the selected signal'],
    ['Alt+Left / Alt+Right', 'Same for the alt marker'],
    ['Up / Down', 'Change the selected row'],
    ['Alt+Up / Alt+Down', 'Move selected rows up / down'],
    ['Ctrl+A', 'Select all rows'],
    ['Delete / Backspace', 'Remove selected rows'],
    ['1 - 8', 'Set the color of selected signals'],
    ['F2', 'Rename the selected row'],
    ['g', 'Group the selected rows'],
    ['Ctrl+0', 'Zoom to fit'],
    ['+ / -', 'Zoom in / out'],
    ['Home / End', 'Marker to the start / end of the dump'],
    ['Ctrl+Z, Ctrl+Shift+Z', 'Undo / redo edits to the signal list'],
    ['Ctrl+S', 'Save the session as a JSON file'],
    ['Ctrl+F', 'Focus the netlist filter'],
    ['Escape', 'Abort a drag, close menus and dialogs'],
    ['?', 'This help'],
    ['Click', 'Place the marker (snaps to a change of that row)'],
    ['Alt+click, middle click', 'Place the alt marker'],
    ['Drag over the waveform', 'Zoom to that range'],
    ['Drag in the ruler', 'Move the marker'],
    ['Wheel', 'Pan in time'],
    ['Shift+wheel', 'Scroll the signal list'],
    ['Ctrl+wheel', 'Zoom around the pointer'],
    ['Drag a name row', 'Reorder (drop on a group to move inside it)'],
    ['Right click a row', 'Format, color, style, row height, split, remove'],
    ['Double-click a netlist signal', 'Add it to the waveform']
  ];

  function showHelp() {
    var body = document.createElement('table');
    body.className = 'helptable';
    HELP.forEach(function (r) {
      var tr = document.createElement('tr');
      var a = document.createElement('td'), b = document.createElement('td');
      a.textContent = r[0]; b.textContent = r[1];
      tr.appendChild(a); tr.appendChild(b);
      body.appendChild(tr);
    });
    openModal('Keyboard and mouse', body, [{ label: 'Close', primary: true }]);
  }

  /* ----- toolbar and keyboard ----- */

  var ACTIONS = {
    zoomin: zoomIn, zoomout: zoomOut, zoomfit: zoomFit,
    prevrise: function () { edgeJump('rising', -1); },
    nextrise: function () { edgeJump('rising', 1); },
    prevfall: function () { edgeJump('falling', -1); },
    nextfall: function () { edgeJump('falling', 1); },
    searchprev: function () { valueSearch(-1); },
    searchnext: function () { valueSearch(1); },
    save: saveSession,
    load: function () { $('sessionfile').click(); },
    wavedrom: waveDromDialog,
    help: showHelp,
    netlist: function () { $('netlist').classList.toggle('hidden'); setTimeout(requestDraw, 0); }
  };

  Array.prototype.forEach.call(document.querySelectorAll('[data-act]'), function (b) {
    b.addEventListener('click', function () { ACTIONS[b.getAttribute('data-act')](); });
  });

  $('search').addEventListener('keydown', function (e) {
    if (e.key === 'Enter') { e.preventDefault(); valueSearch(e.shiftKey ? -1 : 1); }
    if (e.key === 'Escape') $('search').blur();
  });

  $('unit').addEventListener('change', function () {
    A.unit = $('unit').value;
    updateStatus();
    requestDraw();
  });

  function moveSelection(dir) {
    if (!A.rows.length) return;
    var i = A.primary === null ? -1 : rowIndex(A.primary);
    i = i < 0 ? (dir > 0 ? 0 : A.rows.length - 1) : Math.min(Math.max(i + dir, 0), A.rows.length - 1);
    selectOnly(A.rows[i].item);
    scrollToRow(A.rows[i].item);
  }

  document.addEventListener('keydown', function (e) {
    var tag = (e.target && e.target.tagName) || '';
    var typing = tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT';
    var ctrl = e.ctrlKey || e.metaKey;
    if (e.key === 'Escape') {
      if (A.menu) { closeMenu(); return; }
      if (A.modal) { closeModal(); return; }
      if (A.drag) { A.drag = null; requestDraw(); return; }
      if (typing) { e.target.blur(); return; }
      selectOnly(null);
      return;
    }
    if (A.modal) return;
    if (ctrl && !e.altKey && (e.key === 's' || e.key === 'S')) { e.preventDefault(); saveSession(); return; }
    if (ctrl && !e.altKey && (e.key === 'f' || e.key === 'F')) {
      e.preventDefault();
      $('netlist').classList.remove('hidden');
      filter.focus(); filter.select();
      return;
    }
    if (typing) return;
    var key = e.key;
    if (ctrl) {
      if (key === 'a' || key === 'A') { e.preventDefault(); A.sel = {}; A.rows.forEach(function (r) { A.sel[r.item._id] = true; }); refreshSelection(); requestDraw(); }
      else if (key === '0') { e.preventDefault(); zoomFit(); }
      else if (key === 'z' || key === 'Z') { e.preventDefault(); if (e.shiftKey) redo(); else undo(); }
      else if (key === 'y' || key === 'Y') { e.preventDefault(); redo(); }
      else if (key === '+' || key === '=') { e.preventDefault(); zoomIn(); }
      else if (key === '-') { e.preventDefault(); zoomOut(); }
      return;
    }
    switch (key) {
      case 'ArrowLeft': e.preventDefault(); moveMarker(e.altKey ? 'altMarker' : 'marker', -1); break;
      case 'ArrowRight': e.preventDefault(); moveMarker(e.altKey ? 'altMarker' : 'marker', 1); break;
      case 'ArrowUp': e.preventDefault(); if (e.altKey) reorderSelected(-1); else moveSelection(-1); break;
      case 'ArrowDown': e.preventDefault(); if (e.altKey) reorderSelected(1); else moveSelection(1); break;
      case 'n': moveMarker('marker', 1); break;
      case 'N': moveMarker('marker', -1); break;
      case 'Delete': case 'Backspace': e.preventDefault(); removeSelected(); break;
      case 'F2': e.preventDefault(); renameItems(selectedItems()); break;
      case 'g': groupSelected(); break;
      case '+': case '=': zoomIn(); break;
      case '-': case '_': zoomOut(); break;
      case 'Home': e.preventDefault(); setMarker('marker', D.start); ensureVisible(D.start); break;
      case 'End': e.preventDefault(); setMarker('marker', D.end); ensureVisible(D.end); break;
      case '?': showHelp(); break;
      default:
        if (/^[1-8]$/.test(key) && !e.altKey) setColor(parseInt(key, 10));
    }
  });

  /* ----- start ----- */

  UNITS.forEach(function (u) {
    var o = document.createElement('option');
    o.value = u; o.textContent = u;
    $('unit').appendChild(o);
  });

  var loaded = false;
  if (D.session) {
    try {
      var sess = sessionFromJSON(D.session);
      var miss = applySession(sess, false);
      loaded = true;
      if (miss) flash(miss + ' saved signal' + (miss === 1 ? '' : 's') + ' not found in this dump');
    } catch (err) { flash('Ignoring embedded session: ' + err.message); }
  }
  if (!loaded) { zoomFit(); relayout(); }
  $('unit').value = A.unit;
  initExpanded();
  buildTree();
  updateStatus();
  $('title').textContent = D.file || '';

  if (window.matchMedia) {
    var mq = window.matchMedia('(prefers-color-scheme: dark)');
    var onTheme = function () { A.theme = null; buildNames(); requestDraw(); };
    if (mq.addEventListener) mq.addEventListener('change', onTheme);
    else if (mq.addListener) mq.addListener(onTheme);
  }
  window.addEventListener('resize', function () { clampScroll(); requestDraw(); });
  if (window.ResizeObserver) new ResizeObserver(function () { clampScroll(); requestDraw(); }).observe($('wavewrap'));
  requestDraw();

  /* hooks for tests that drive the page */
  window.evw = { state: A, data: D, relayout: relayout, addVar: addVar, draw: draw };
}

if (typeof document !== 'undefined') {
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', bootUI);
  else bootUI();
}

if (typeof module !== 'undefined') {
  module.exports = {
    isText: isText,
    formatBits: formatBits, formatReal: formatReal, formatG: formatG, formatValue: formatValue,
    sliceBits: sliceBits, formatTime: formatTime, ticksPerUnit: ticksPerUnit, autoUnit: autoUnit,
    niceStep: niceStep, formatFrequency: formatFrequency,
    lastIdxLE: lastIdxLE, lastIdxLT: lastIdxLT, valueAt: valueAt,
    findChange: findChange, findEdge: findEdge, searchValue: searchValue, waveDrom: waveDrom,
    sessionToJSON: sessionToJSON, sessionFromJSON: sessionFromJSON, normalizeItems: normalizeItems,
    scanSegments: scanSegments, UNITS: UNITS, FORMATS: FORMATS
  };
}
