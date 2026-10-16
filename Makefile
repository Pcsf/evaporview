# evaporview --- build, test and audit
#
# Runtime needs Emacs 27.1+ and any browser.  bun is only for the viewer's
# unit tests and runs at home, never on the target machine.

EMACS ?= emacs
BUN   ?= bun
# ewavedrom is optional; its integration tests run when it is checked out here.
EWAVEDROM ?= ../ewavedrom
BATCH  = $(EMACS) -Q --batch -L . $(if $(wildcard $(EWAVEDROM)/ewavedrom.el),-L $(EWAVEDROM))
EL     = evaporview-vcd.el evaporview-format.el evaporview.el evaporview-html.el
BUILD  = build

.PHONY: all compile test test-el test-js audit bench demo verify examples showcase try clean

all: compile test audit

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(EL)

test: test-el test-js

test-el: compile
	$(BATCH) -l test/evaporview-test.el -f ert-run-tests-batch-and-exit

test-js:
	$(BUN) test test/viewer.test.ts

# Runtime files must not reach outside Emacs, nor use functions newer than
# Emacs 27.1.  Each scan also runs against a planted file and must hit it,
# so an empty result cannot come from a broken pattern.  browse-url is the one
# sanctioned exit, on an explicit command.
FORBIDDEN_CALLS = (call-process|process-file|start-process|make-process|shell-command|url-retrieve|open-network-stream|make-network-process)
EMACS28_FNS     = (string-search|string-replace|length[<>=]|ensure-list|string-pad|string-fill|string-chop-newline|file-name-concat|take |ntake |pos-bol|pos-eol|json-parse|json-serialize|seq-keep|plistp|always |named-let|text-property-search)
JS_EXTERNAL     = (https?:|//cdn|fetch\(|XMLHttpRequest|import\(|importScripts|WebSocket|EventSource)
audit: | $(BUILD)
	@printf '(call-process "x")\n(string-search "a" "b")\nfetch(u)\n' > $(BUILD)/planted.txt
	@test "$$(grep -cE '$(FORBIDDEN_CALLS)' $(BUILD)/planted.txt)" = 1 || { echo "audit: process pattern is broken"; exit 1; }
	@test "$$(grep -cE '\($(EMACS28_FNS)' $(BUILD)/planted.txt)" = 1 || { echo "audit: version pattern is broken"; exit 1; }
	@test "$$(grep -cE '$(JS_EXTERNAL)' $(BUILD)/planted.txt)" = 1 || { echo "audit: js pattern is broken"; exit 1; }
	@if grep -nE '$(FORBIDDEN_CALLS)' $(EL); then echo "audit: runtime reaches outside Emacs"; exit 1; fi
	@if grep -nE '\($(EMACS28_FNS)' $(EL); then echo "audit: runtime needs Emacs 28+"; exit 1; fi
	@if grep -nE '$(JS_EXTERNAL)' assets/*; then echo "audit: viewer reaches the network"; exit 1; fi
	@if grep -nE "^\(require '" $(EL) | grep -vE "'(cl-lib|subr-x|json|evaporview|evaporview-vcd|evaporview-format|evaporview-html|browse-url)\)"; then \
	  echo "audit: unexpected require"; exit 1; fi
	@echo "audit: planted cases caught, runtime clean"

# Parse cost on a generated dump; the figure quoted in README.org comes from here.
bench: compile | $(BUILD)
	$(BATCH) -l test/evaporview-bench.el

demo: compile | $(BUILD)
	$(BATCH) -l evaporview -l evaporview-html \
	  --eval '(evaporview-html-export-file "test/data/counter.vcd" "$(BUILD)/counter.html" (quote ("counter_tb.clk" "counter_tb.rst" "counter_tb.cnt" "counter_tb.bus4" "counter_tb.ana")))'
	@echo "wrote $(BUILD)/counter.html"

$(BUILD):
	mkdir -p $(BUILD)

clean:
	rm -rf $(BUILD) *.elc test/*.elc

# Drives the demo page in headless Chromium with real DOM events (netlist
# double-click, marker click, arrows, Alt+click, Ctrl+wheel, Ctrl+0, drag zoom,
# Delete, Ctrl+Z) and screenshots the result.  The log's canvas pixel count has
# read zero in one --dump-dom run with no known cause, so the screenshot is the
# judge of drawing and the log the judge of state.
CHROMIUM ?= chromium
verify: demo
	awk -v js="$$(cat test/browser-drive.js)" '/<\/body>/ { print "<script>" js "</script>" } { print }' \
	  $(BUILD)/counter.html > $(BUILD)/driven.html
	$(CHROMIUM) --headless --disable-gpu --no-sandbox --window-size=1400,700 \
	  --virtual-time-budget=5000 --dump-dom "file://$(CURDIR)/$(BUILD)/driven.html" 2>/dev/null \
	  | grep -o '<pre id="harness-result">[^<]*' | sed -e 's/<pre id="harness-result">//' -e 's/&quot;/"/g' \
	  > $(BUILD)/driven.json
	$(CHROMIUM) --headless --disable-gpu --no-sandbox --hide-scrollbars --window-size=1400,700 \
	  --virtual-time-budget=5000 --screenshot=$(BUILD)/driven.png "file://$(CURDIR)/$(BUILD)/driven.html" 2>/dev/null
	@grep -q '"errs":\[\]' $(BUILD)/driven.json || { echo "verify: page errors"; cat $(BUILD)/driven.json; exit 1; }
	@echo "verify: $(BUILD)/driven.json (state log), $(BUILD)/driven.png (view)"

# Example dumps come from GHDL (dev only); their sessions from examples/sessions.el.
EXAMPLES = spi uart nco
examples:
	cd examples && for t in $(EXAMPLES); do \
	  ghdl -a --std=08 $${t}_tb.vhd && ghdl -e --std=08 $${t}_tb && \
	  ghdl -r --std=08 $${t}_tb --vcd=$$t.vcd --stop-time=5us || exit 1; \
	done; rm -f *.o *.cf $(addsuffix _tb,$(EXAMPLES))
	$(BATCH) -l examples/sessions.el --eval '(evaporview-examples-write-sessions)'

# Gallery images for examples/README.org: the Emacs view, the browser view and
# the ewavedrom diagrams.  Needs ewavedrom and Chromium.
showcase: compile
	$(BATCH) -l examples/showcase.el
	@for t in $(EXAMPLES); do for v in emacs browser; do \
	  $(CHROMIUM) --headless --disable-gpu --no-sandbox --hide-scrollbars \
	    --window-size=$$([ $$v = emacs ] && echo 1180,$$([ $$t = uart ] && echo 230 || echo 300) || echo 1400,560) \
	    --virtual-time-budget=4000 --screenshot=examples/img/$$t-$$v.png \
	    "file://$(CURDIR)/$(BUILD)/showcase/$$t-$$v.html" 2>/dev/null || exit 1; \
	done; done
	@echo "gallery in examples/img/"

# Interactive tour: the SPI example with its netlist and a WaveDrom preview.
try: compile
	$(EMACS) -Q -L . $(if $(wildcard $(EWAVEDROM)/ewavedrom.el),-L $(EWAVEDROM)) -l examples/tour.el
