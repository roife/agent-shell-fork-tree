# Performance experiments

The performance work follows one rule: an optimization stays in production only
when an isolated ablation supports a general workload.  Scale-specific ideas are
kept in the benchmark as rejected candidates when they improve an extreme case
but slow down the common case.

## Method

The current implementation is compared with one optimization disabled at a
time.  Variants run in alternating order for seven rounds, and the tables below
report medians.  Every pair must produce the same SHA-256 digest of its observable
result; the runner aborts on a mismatch.

Historical functions are read from baseline commit
`0271ad9cd590319c64227a1ce59608bee9d406e9` and macroexpanded before evaluation.
This mirrors normal source loading and avoids charging an old function for macro
expansion during every call.

Environment for the checked-in run:

- Emacs 31.1
- `aarch64-apple-darwin25.5.0`
- 17 scenarios × 2 variants × 7 rounds = 238 samples
- Raw data: [tests/benchmark-general-results.json](tests/benchmark-general-results.json)
- Harness: [tests/benchmark-general.el](tests/benchmark-general.el)

Run it with the same dependency load paths used by the test suite:

```sh
AFT_PERF_REPEATS=7 \
AFT_PERF_OUTPUT=tests/benchmark-general-results.json \
emacs -Q --batch \
  -L /path/to/agent-shell -L /path/to/acp -L /path/to/shell-maker \
  -L /path/to/markdown-mode -L /path/to/map -L . \
  -l tests/benchmark-general.el -f aft-perf-run
```

## Retained optimizations

| Optimization and workload | Current median | Ablated median | Improvement |
| --- | ---: | ---: | ---: |
| Constant-time child append, 6,000 root children | 0.167253 s | 0.384137 s | 56.5% |
| Reuse cached path spine, replay 5,000 turns 10 times | 0.078131 s | 0.081384 s | 4.0% |
| Reuse cached path spine, append 10 turns after 5,000 | 0.008096 s | 0.008763 s | 7.6% |
| Cache path endpoint, 1,000 lookups on 5,000 turns | 0.000406 s | 0.068152 s | 99.4% |
| Constant-time page linking, 6,000 rows in pages of 32 | 0.004904 s | 0.155211 s | 96.8% |
| Maintain page count incrementally, same workload | 0.004946 s | 0.018162 s | 72.8% |
| Cache scan total for 60,000 progress updates | 0.018020 s | 0.293890 s | 93.9% |
| Hash membership for stable priority partitioning | 0.045348 s | 0.467023 s | 90.3% |
| Cache visible-node set, 60 reads of a 1,500-turn view | 0.000018 s | 0.020274 s | 99.9% |
| Reuse rendered row positions, 60 parent/child moves | 0.000143 s | 1.886020 s | 99.99% |

The path-spine results are intentionally modest: validation still has to inspect
the replayed prefix, but it no longer copies the full path merely to append or
confirm it.  Endpoint caching removes a separate repeated `last` traversal.

The `cached-discovery` scenario is an end-to-end regression check rather than a
single ablation.  A 6,000-session cached scan takes 0.036635 s with all retained
scan changes versus 0.070020 s with the historical scan function, a 47.7%
reduction.  Individual page-tail, page-count, progress-total, and priority rows
above provide the single-change evidence.

## Rejected candidates

| Candidate | Common-case result | Large/repeated result | Decision |
| --- | ---: | ---: | --- |
| Per-turn tool-call hash | 4 tools: 2.2% slower | 300 tools: 19.5% faster | Rejected; a typical turn has few tools, so the bookkeeping cost is paid more often than the extreme-case gain is realized. |
| Persistent first-turn session index | 1 query in a 1,000-session lifecycle: 31.9% slower | 10 queries: 36.8% faster; lookup-only workloads improve 77.7–97.7% | Rejected; the UI normally computes a related group once per store revision, so insertion/deletion maintenance is the dominant general cost. |

Both candidates remain implemented only inside the benchmark harness so their
crossover behavior can be reproduced if future usage patterns change.

## Earlier end-to-end results

The repository also keeps the earlier rebuild benchmark and its raw results:

- [tests/benchmark-results.json](tests/benchmark-results.json): 2.452791 s to
  0.243572 s median, about 10.1× faster.
- [tests/benchmark-large-results.json](tests/benchmark-large-results.json):
  22.938738 s to 1.038167 s median, about 22.1× faster.

Those experiments cover semantic refresh suppression, indexed rendering, and
batched cache checkpoints.  They are end-to-end combinations; the newer general
suite focuses on isolated algorithmic costs and explicitly records rejected
optimizations.

## Limits

These benchmarks measure local Lisp processing with deterministic synthetic
ACP data.  Network latency, backend replay cost, filesystem variability, font
rendering, and redisplay latency in a live GUI can dominate wall-clock time.
Absolute timings therefore should not be compared across machines; use paired
ratios and verify behavior with the integration tests.
