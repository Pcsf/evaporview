---
phase: complete
progress: 18/18
task: "VaporView-style waveform viewer for Emacs that runs under corporate IT restrictions"
slug: evaporview
iteration: 1
started: 2026-10-16T00:00:00Z
updated: 2026-10-16T00:00:00Z
principal_stated_goal: "please create a emacs pkg that reproduces the functionalities of vaporview for visual studio code (https://github.com/Lramseyer/vaporview). It should be able to run on corporative environment with lots of IT restrictions though. You can scafold the viewer using html (exported like done in org) if it makes more feaseble."
principal_stated_goal_source: prompt
principal_stated_goal_signal: 1
principal_stated_goal_locked: 2026-10-16T00:00:00Z
---

# evaporview -- VaporView-style waveform viewing from Emacs

## Problem

VaporView gives VS Code an interactive VCD/FST/GHW viewer. At work VS Code
extensions (and often GTKWave, Node, Python packages) are blocked; Emacs and a
browser are what is reliably there.

## Vision

Open a simulation dump at work with `M-x evaporview-open` and get the
VaporView workflow without installing anything but one Emacs package: a netlist
to pick signals from, waveforms with markers, edge stepping, formats, search,
saved sessions. One key moves the same view into a browser tab for full mouse
interaction, and that page still works when copied to a machine with no
network.

## Out of Scope (v1)

- FST and GHW decoding (binary/compressed formats); FSDB; remote viewing; WCP;
  terminal links; CRT glow; LSP/source navigation.

## Principles

- One parser, in Emacs Lisp; the browser viewer consumes its output.
- Runtime touches nothing outside Emacs except `browse-url` on an explicit
  command.

## Claims

- [x] ISC-1: The VCD parser recovers timescale, scope tree, vars (type, width,
  range), aliases sharing one id code, and every value change of a GHDL-produced
  VCD. Probe: ERT `evaporview-test-ghdl-fixture` against `test/data/counter.vcd`.
- [x] ISC-2: Vector values shorter than the declared width are extended per IEEE
  1364 (0/1 -> 0, x -> x, z -> z); `$dumpvars`/`$dumpoff`/`$comment` handled.
  Probe: ERT parser cases.
- [x] ISC-3: Value formats binary, hex, octal, unsigned, signed, ascii, float
  (16/32/64) and real are correct, and non-2-state values fall back to binary for
  the formats that cannot show them. Probe: shared table `test/format-cases.json`
  run by ERT.
- [x] ISC-4: The browser viewer's formatter agrees with the Emacs one on every row
  of the same `test/format-cases.json`. Probe: `bun test test/viewer.test.ts`.
- [x] ISC-5: Value-at-time, next/previous edge, next/previous rising/falling edge
  and value search return the right times, including at the dump's ends.
  Probe: ERT navigation cases.
- [x] ISC-6: The native viewer renders a known wave to the expected text
  (bit, bus with labels, x/z, marker column). Probe: ERT golden render.
- [x] ISC-7: Native viewer edits -- add/remove/reorder/rename/color/format,
  bit slice, split, groups -- round-trip through undo/redo and through a saved
  JSON session. Probe: ERT session cases.
- [x] ISC-8: WaveDrom export of the marker to alt-marker range yields strict-JSON
  WaveJSON with at most 32 events, clocked and unclocked, and is exposed as a
  string-returning function so ewavedrom can consume it later. Probe: ERT
  wavedrom cases parse the output with `json-read-from-string`.
- [x] ISC-9: The exported HTML is one file with no external reference (no
  `src=`/`href=` to any URL, no `@import`, no `fetch`). Probe: ERT scan of a
  generated page plus a planted-positive check.
- [x] ISC-10: In a real browser the HTML viewer shows the netlist, adds a signal
  on double-click, places marker on click and alt-marker on Alt+click, zooms with
  Ctrl+wheel and Ctrl+0, steps edges with arrow keys. Probe: Interceptor session
  with screenshots.
- [x] ISC-11: Runtime elisp calls no process, network or server function and no
  function newer than Emacs 27.1. Probe: `make audit` with planted positives.
- [x] ISC-12: All runtime files byte-compile with zero warnings. Probe:
  `make compile` with `byte-compile-error-on-warn`.
- [x] ISC-13: Parse cost is measured and stated in README for a generated
  large dump. Probe: `make bench` output.
- [x] ISC-14: README.org lists every dependency and every keybinding of both
  viewers. Probe: Read README; ERT checks each viewer command key appears.
- [x] ISC-15: Sessions saved by the browser load in Emacs and vice versa (same
  JSON schema). Probe: ERT loads a session file written by the viewer's
  serializer under bun.
- [x] ISC-16: Opening a dump never modifies it. Probe: ERT hashes fixture before
  and after open + session save.

## Anti-claims

- [x] ISC-A1: No runtime dependency beyond Emacs 27.1+ and any browser: no
  Node, Python, GTKWave, CDN, local server or listening port. Probe: `make audit`.
- [x] ISC-A2: No code copied from VaporView (GPL-3 compatible anyway, but this is
  a reimplementation). Probe: SELF.

## Not yet specified

- FST/GHW decoding in pure elisp (zlib via `zlib-decompress-region` exists, LZ4
  does not); a GHW reader is the most plausible v2 for GHDL users.
- Parse speed limits for >100 MB dumps.
- ewavedrom integration (principal, 2026-10-16: "a nice integration between
  these two pkg later on"): render the copied WaveDrom selection directly in
  Emacs. Seam: `ewavedrom-render-string` takes the WaveJSON string ISC-8
  produces, so the hook is a soft `(require 'ewavedrom nil t)`, never a
  Package-Requires entry.

## Decisions

- 2026-10-16 Two front ends over one parser: text viewer for TTY/SSH and
  in-editor use, HTML for mouse-grade interaction. SVG-in-buffer rejected: needs
  librsvg and fails under `-nw`.
- 2026-10-16 Decimal/signed formatting uses base-10^9 limbs, not bignums, so
  wide buses are exact and the arithmetic stays inside fixnums.
- 2026-10-16 Learned: Emacs 31 refuses `aset` of a non-ASCII char into a string
  built with `make-string ?\s` (byte length would change). Rows and ruler are
  built as char vectors and `concat`ed. Applies to any text-art code here.
- 2026-10-16 Learned: a column's "value before" must be the initial value when
  the dump starts inside it, or a dense first column hides its changes (caught
  by the dense-column golden case).
- 2026-10-16 Plain wheel keeps vertical scrolling in the Emacs buffer (Emacs
  convention); time panning is S-wheel. Deviation from VaporView, documented.
- 2026-10-16 Browser viewer built by a delegated agent against a fixed data and
  session contract; split there replaces the signal with a group of slices,
  Emacs inserts slices after it. Both write the same session schema.
- 2026-10-16 Live-page verification: the Interceptor test tab is hidden on
  this desktop (no rAF), and `VerifyViewport.ts` cannot attach on Linux (its
  CDP path needs the macOS bridge). Rendering and interaction were verified in
  headless Chromium instead, now `make verify`.

## Test Strategy

`make all` = compile (warnings are errors) + ERT + audit; `bun test` for the
viewer's pure functions; Interceptor for the live page.

## Evidence

- ISC-1,2,3,5,6,7,8,14,16: `make test-el` 18/18 (2026-10-16); format table planted-row check failed as expected.
- ISC-11, ISC-A1: `make audit` clean with three planted positives caught.
- ISC-12: `make compile` with `byte-compile-error-on-warn`, no output.
- ISC-13: `make bench` 16.3 MB / 1.12 M changes parse 0.86 s; README "Limits".
- ISC-4: `bun test` 81/0; planted "2B" row -> 2 fails, restored.
- ISC-9: ERT html test + `evaporview-html-external-refs` nil on build/counter.html.
- ISC-10: Interceptor (Brave test profile) loaded the file:// page, netlist and
  session DOM correct; tab hidden so canvas checked via `make verify`
  (headless Chromium, real DOM events): state log + build/driven.png viewed.
- ISC-15: ERT `session-from-browser` loads the fixture the JS serializer wrote;
  Emacs session drives the page's initial view (screenshot).
- ISC-A2: SELF -- written from the feature list; VaporView source never fetched.
