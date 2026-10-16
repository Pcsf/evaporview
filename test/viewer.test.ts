import { describe, expect, test } from "bun:test";
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const root = join(import.meta.dir, "..");
// eslint-disable-next-line @typescript-eslint/no-var-requires
const V = require(join(root, "assets/viewer.js"));

const cases: any[] = JSON.parse(readFileSync(join(import.meta.dir, "format-cases.json"), "utf8"));

function bitsOf(c: any): string {
  if (c.bits !== undefined) return c.bits;
  return BigInt("0x" + c.hex).toString(2).padStart(c.width, "0").slice(-c.width);
}

describe("format-cases.json", () => {
  cases.forEach((c, i) => {
    test(`row ${i}: ${c.fmt} ${c.bits ?? c.hex ?? c.real}`, () => {
      if (c.real !== undefined) {
        expect(V.formatValue({ real: true }, c.real, c.fmt, null)).toBe(c.out);
        expect(V.formatReal(c.real)).toBe(c.out);
      } else {
        expect(V.formatBits(bitsOf(c), c.fmt)).toBe(c.out);
        expect(V.formatValue({ real: false }, bitsOf(c), c.fmt, null)).toBe(c.out);
      }
    });
  });
});

describe("%g clone", () => {
  test("rounding and exponent boundaries", () => {
    expect(V.formatG(123456)).toBe("123456");
    expect(V.formatG(1234567)).toBe("1.23457e+06");
    expect(V.formatG(1234565)).toBe("1.23456e+06"); // exact tie rounds to even
    expect(V.formatG(999999.5)).toBe("1e+06");
    expect(V.formatG(0.0001)).toBe("0.0001");
    expect(V.formatG(0.00001)).toBe("1e-05");
    expect(V.formatG(100)).toBe("100");
    expect(V.formatG(-0.000123456789)).toBe("-0.000123457");
  });
});

describe("slices", () => {
  test("LSB is bit 0 at the right", () => {
    expect(V.sliceBits("11010110", 3, 0)).toBe("0110");
    expect(V.sliceBits("11010110", 7, 4)).toBe("1101");
    expect(V.sliceBits("11010110", 5, 5)).toBe("0");
    expect(V.formatValue({ real: false }, "11010110", "unsigned", [3, 0])).toBe("6");
  });
});

describe("time formatting", () => {
  const fs: [number, string] = [1, "fs"];
  test("units and significant digits", () => {
    expect(V.formatTime(12000000, fs, "ns")).toBe("12 ns");
    expect(V.formatTime(12500000, fs, "ns")).toBe("12.5 ns");
    expect(V.formatTime(30000, fs, "ns")).toBe("0.03 ns");
    expect(V.formatTime(0, fs, "ns")).toBe("0 ns");
    expect(V.formatTime(159000000, fs, "us")).toBe("0.159 us");
    expect(V.formatTime(1234567, fs, "ns")).toBe("1.23457 ns");
    expect(V.formatTime(5, [10, "ns"], "ps")).toBe("50000 ps");
    expect(V.formatTime(5, [1, "ns"], "ns")).toBe("5 ns");
    expect(V.formatTime(1, [100, "ps"], "ns")).toBe("0.1 ns");
    expect(V.formatTime(-12500000, fs, "ns")).toBe("-12.5 ns");
  });
  test("units", () => {
    expect(V.autoUnit(fs, 159000000)).toBe("ns");
    expect(V.ticksPerUnit(fs, "ns")).toBe(1000000);
    expect(V.niceStep(73)).toBe(100);
    expect(V.niceStep(1.5)).toBe(2);
    expect(V.formatFrequency(1e-8)).toBe("100 MHz");
    expect(V.formatFrequency(1e9 * 1e-15)).toBe("1 MHz");
  });
});

// clk: rising at 5,15,25..., falling at 10,20...; cnt 8-bit
const clk = { width: 1, real: false, t: [0, 5, 10, 15, 20, 25], v: ["0", "1", "0", "1", "0", "1"] };
const cnt = { width: 8, real: false, t: [0, 15, 25, 35], v: ["00000000", "00000001", "00000010", "00000011"] };
const ana = { width: 64, real: true, t: [0, 10, 20], v: ["0.0", "-1.0", "2.5e-1"] };

describe("edges and search", () => {
  test("rising and falling", () => {
    expect(V.findEdge(clk, 0, 1, "rising", null)).toBe(5);
    expect(V.findEdge(clk, 5, 1, "rising", null)).toBe(15);
    expect(V.findEdge(clk, 15, -1, "rising", null)).toBe(5);
    expect(V.findEdge(clk, 5, -1, "rising", null)).toBe(null);
    expect(V.findEdge(clk, 0, 1, "falling", null)).toBe(10);
    expect(V.findEdge(clk, 25, 1, "rising", null)).toBe(null);
    expect(V.findEdge(cnt, 0, 1, "rising", null)).toBe(null);
    expect(V.findEdge(cnt, 0, 1, "rising", [0, 0])).toBe(15);
  });
  test("edge classes", () => {
    const s = { width: 1, real: false, t: [0, 1, 2, 3, 4], v: ["x", "1", "z", "h", "l"] };
    expect(V.findEdge(s, 0, 1, "rising", null)).toBe(1);
    expect(V.findEdge(s, 1, 1, "rising", null)).toBe(3);
    expect(V.findEdge(s, 0, 1, "falling", null)).toBe(4);
  });
  test("changes", () => {
    expect(V.findChange(cnt, 15, 1, null)).toBe(25);
    expect(V.findChange(cnt, 15, -1, null)).toBe(0);
    expect(V.findChange(cnt, 0, -1, null)).toBe(null);
    expect(V.findChange(cnt, 35, 1, null)).toBe(null);
    expect(V.findChange(cnt, 0, 1, [7, 4])).toBe(null);
    expect(V.findChange(cnt, -5, 1, null)).toBe(0);
  });
  test("value search ignores case and leading zeros", () => {
    expect(V.searchValue(cnt, "hex", null, "2", 0, 1)).toBe(25);
    expect(V.searchValue(cnt, "hex", null, "02", 0, 1)).toBe(25);
    expect(V.searchValue(cnt, "hex", null, "3", 0, 1)).toBe(35);
    expect(V.searchValue(cnt, "hex", null, "3", 35, -1)).toBe(null);
    expect(V.searchValue(cnt, "hex", null, "1", 35, -1)).toBe(15);
    expect(V.searchValue(cnt, "hex", null, "0", 15, -1)).toBe(0);
    expect(V.searchValue(cnt, "unsigned", null, "2", 0, 1)).toBe(25);
    expect(V.searchValue(ana, "hex", null, "0.25", 0, 1)).toBe(20);
    const x = { width: 4, real: false, t: [0, 5], v: ["10x1", "1010"] };
    expect(V.searchValue(x, "hex", null, "X", -1, 1)).toBe(0);
    expect(V.searchValue(x, "hex", null, "A", 0, 1)).toBe(5);
  });
});

describe("WaveDrom", () => {
  const lanes = (clkFlag: boolean) => [
    { name: "clk", sig: clk, fmt: "binary", slice: null, clock: clkFlag },
    { name: "cnt", sig: cnt, fmt: "hex", slice: null, clock: false },
  ];
  test("unclocked", () => {
    const r = V.waveDrom(lanes(false), 15, 25, null, 32);
    expect(r.events).toEqual([15, 20, 25]);
    expect(r.doc.signal[0].wave).toBe("101");
    expect(r.doc.signal[1].wave).toBe("=.=");
    expect(r.doc.signal[1].data).toEqual(["01", "02"]);
    expect(r.doc.signal[0].data).toBeUndefined();
  });
  test("clocked", () => {
    const r = V.waveDrom(lanes(true), 0, 25, { lane: lanes(true)[0], edge: "rise" }, 32);
    expect(r.events).toEqual([5, 15, 25]);
    expect(r.doc.signal[0].wave).toBe("p..");
    expect(r.doc.signal[1].wave).toBe("===");
    expect(r.doc.signal[1].data).toEqual(["00", "01", "02"]);
    const f = V.waveDrom(lanes(true), 0, 25, { lane: lanes(true)[0], edge: "fall" }, 32);
    expect(f.doc.signal[0].wave).toBe("n.");
  });
  test("limit and non-2-state", () => {
    const big = { width: 1, real: false, t: [] as number[], v: [] as string[] };
    for (let i = 0; i < 100; i++) { big.t.push(i); big.v.push(i % 2 ? "1" : "0"); }
    const r = V.waveDrom([{ name: "b", sig: big, fmt: "binary", slice: null, clock: false }], 0, 99, null, 32);
    expect(r.doc.signal[0].wave.length).toBe(32);
    expect(r.truncated).toBe(true);
    const m = { width: 4, real: false, t: [0, 1, 2, 3], v: ["0001", "zzzz", "10x1", "0001"] };
    const w = V.waveDrom([{ name: "m", sig: m, fmt: "hex", slice: null, clock: false }], 0, 3, null, 32);
    expect(w.doc.signal[0].wave).toBe("=zx=");
    expect(w.doc.signal[0].data).toEqual(["1", "1"]);
    const q = { width: 1, real: false, t: [0, 1, 2], v: ["u", "l", "z"] };
    const y = V.waveDrom([{ name: "q", sig: q, fmt: "binary", slice: null, clock: false }], 0, 2, null, 32);
    expect(y.doc.signal[0].wave).toBe("x0z");
  });
});

describe("session", () => {
  const state = {
    file: "counter.vcd",
    marker: 15000000,
    altMarker: 5000000,
    viewStart: 0,
    viewEnd: 159000000,
    timeUnit: "ns",
    items: [
      { type: "signal", path: "counter_tb.clk", name: null, format: "binary", color: 1, style: "digital", slice: null },
      { type: "signal", path: "counter_tb.cnt", name: null, format: "hex", color: 2, style: "digital", slice: null },
      {
        type: "group", name: "buses", expanded: true,
        items: [
          { type: "signal", path: "counter_tb.bus4", name: null, format: "binary", color: 3, style: "digital", slice: null },
          { type: "signal", path: "counter_tb.cnt", name: "cnt low", format: "unsigned", color: 4, style: "digital", slice: [3, 0] },
        ],
      },
    ],
  };
  const fixture = join(import.meta.dir, "data/session-from-viewer.json");

  test("serialized form matches the shared fixture byte for byte", () => {
    const text = V.sessionToJSON(state);
    if (process.env.UPDATE === "1") writeFileSync(fixture, text);
    expect(existsSync(fixture)).toBe(true);
    expect(readFileSync(fixture, "utf8")).toBe(text);
  });
  test("fixture shape for the Emacs side", () => {
    const o = JSON.parse(readFileSync(fixture, "utf8"));
    expect(o.version).toBe(1);
    expect(o.items[0].path).toBe("counter_tb.clk");
    expect(o.items[1].path).toBe("counter_tb.cnt");
    expect(o.items[1].format).toBe("hex");
    expect(o.items[2].type).toBe("group");
    expect(o.items[2].items[0].path).toBe("counter_tb.bus4");
    expect(o.items[2].items[1].slice).toEqual([3, 0]);
    expect(o.altMarker).toBe(5000000);
    expect(o.marker).toBe(15000000);
    expect(Object.keys(o)).toEqual(["version", "file", "marker", "altMarker", "viewStart", "viewEnd", "timeUnit", "items"]);
    expect(Object.keys(o.items[0])).toEqual(["type", "path", "name", "format", "color", "style", "slice"]);
    expect(Object.keys(o.items[2])).toEqual(["type", "name", "expanded", "items"]);
  });
  test("round trip", () => {
    const text = V.sessionToJSON(state);
    expect(V.sessionToJSON(V.sessionFromJSON(text))).toBe(text);
    expect(V.sessionToJSON({ ...state, marker: null, altMarker: null })).toContain('"marker": null');
  });
  test("tolerates unknown keys and rejects bad versions", () => {
    const s = V.sessionFromJSON({ version: 1, extra: 1, items: [{ type: "signal", path: "a", format: "nope", color: 99, other: 1 }, { type: "wat" }] });
    expect(s.items.length).toBe(1);
    expect(s.items[0].format).toBe("binary");
    expect(s.items[0].color).toBe(1);
    expect(() => V.sessionFromJSON({ version: 2 })).toThrow();
    expect(() => V.sessionFromJSON("{")).toThrow();
  });
  test("analog keeps its line style", () => {
    const text = V.sessionToJSON({ ...state, items: [{ type: "signal", path: "x", name: null, format: "real", color: 1, style: "analog", slice: null, lineStyle: "linear", height: 80 }] });
    const o = JSON.parse(text);
    expect(o.items[0].lineStyle).toBe("linear");
    expect(V.sessionFromJSON(text).items[0].height).toBe(80);
  });
});

describe("rendering cost", () => {
  test("segment visits follow pixels, not changes", () => {
    const n = 2_000_000, t: number[] = new Array(n);
    for (let i = 0; i < n; i++) t[i] = i * 10;
    let calls = 0;
    const W = 1000, end = n * 10;
    V.scanSegments(t, 0, end, end / W, () => { calls++; }, () => { calls++; });
    expect(calls).toBeLessThanOrEqual(2 * W);
    calls = 0;
    V.scanSegments(t, 5000, 5400, 0.4, () => { calls++; }, () => { calls++; });
    expect(calls).toBeLessThanOrEqual(60);
  });
});

describe("assets obey the corporate constraints", () => {
  const read = (f: string) => readFileSync(join(root, "assets", f), "utf8");
  const bad = /(https?:|\/\/cdn|fetch\(|XMLHttpRequest|import\(|importScripts|WebSocket|EventSource)/;
  for (const f of ["viewer.html", "viewer.css", "viewer.js"]) {
    test(`${f}: no network references, ASCII only`, () => {
      const s = read(f);
      expect(bad.test(s)).toBe(false);
      expect(/[^\x00-\x7f]/.test(s)).toBe(false);
    });
  }
  test("the audit pattern fires on a planted reference", () => {
    expect(bad.test("x = fetch(u)")).toBe(true);
  });
  test("placeholders appear exactly once each", () => {
    const h = read("viewer.html");
    for (const p of ["/*EVW:TITLE*/", "/*EVW:CSS*/", "/*EVW:DATA*/", "/*EVW:JS*/"]) {
      expect(h.split(p).length - 1).toBe(1);
    }
    expect(/<script[^>]*\ssrc=|<link\b/i.test(h)).toBe(false);
    expect(read("viewer.js").includes("</script")).toBe(false);
  });
});
