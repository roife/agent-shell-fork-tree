# agent-shell-fork-tree

Browse related agent-shell conversations, including forks created before this package was installed. A project-wide cache discovers candidates; the displayed tree contains only the current conversation and sessions sharing its nonempty history prefix.

The package uses generic ACP `session/list`, `session/load` and native `session/fork`. It does not read provider databases, private parent metadata, or message-point fork extensions.

## Requirements

- Emacs 29.1+
- agent-shell 0.75.2+
- acp 0.14.3+
- markdown-mode 2.4+
- An already configured agent-shell backend advertising `session/list` and `loadSession`.

## Install

```elisp
(use-package agent-shell-fork-tree
  :load-path "~/code/agent-shell-fork-tree"
  :hook (agent-shell-mode . agent-shell-fork-tree-mode)
  :bind ("C-c g t" . agent-shell-fork-tree)
  :custom
  (agent-shell-fork-tree-auto-rebuild nil)
  (agent-shell-fork-tree-message-truncation 80)
  (agent-shell-fork-tree-scan-concurrency 8))
```

With straight.el managing packages by default, also add `:straight nil` to this declaration.

Open an agent-shell conversation and run `M-x agent-shell-fork-tree`. Automatic rebuilding is **off by default**: opening the tree displays the cache, and `g` discovers or updates history.

## Controls

| Key | Action |
| --- | --- |
| `g` | Incremental rebuild from cached checkpoints |
| `C-u g` | Full rebuild into a fresh cache |
| `a` | Toggle automatic rebuilding |
| `n` / `p`, `j` / `k`, arrow keys | Select rows; preview follows point |
| `h` / `l` | Parent / visible child |
| `/` | Search turns in this conversation only |
| `s` | Label a turn; empty input removes the label |
| `d` | Toggle historical tool diff |
| `RET` on a `↳` session row | Continue that native session |
| `f` on a `↳` session row | Fork that session's current endpoint |
| `C-c C-k` | Cancel discovery, keeping confirmed checkpoints |
| `q` | Close the tree and preview |

Each native session has its own `↳` row, even when several sessions have identical history. Empty sessions do not relate to one another through an empty prefix. Historical turn rows are previews, not backend checkpoints: arbitrary old turns cannot be restored or forked through generic ACP.

Conversation text in tree rows is truncated to 80 display columns by default. Customize `agent-shell-fork-tree-message-truncation`, or set it to nil to show complete prompts. Search, previews and custom labels always retain their full text.

Automatic mode updates on opening and after a tracked shell finishes a turn or initializes a session while its related tree is visible. It does not poll continuously. Changes made by other clients are discovered on reopening or with `g`.

## Matching and caching

The fingerprint takes **only user text and assistant reply text**, with an unambiguous boundary. Tools, reasoning, IDs, timestamps and parent metadata are excluded. IDs locate candidate nodes without hashing again; matches remain constrained to the same parent. Text equality prevents session-local IDs such as counters from merging different content.

Nodes use parent-scoped ID and text-fingerprint indexes. The implementation does not use the proposed global parent-fingerprint chain. A changed session validates its cached prefix using saved node references, then appends only new turns; unchanged sessions skip history reads entirely. Prefix validation checks text and IDs, but does not rehash already matched IDs or walk other sessions' paths.

New unrelated candidates retain a first-turn classification. Their remaining turns are not added to the current tree. If that conversation later becomes the focus, its full history is loaded.

Cache files live in `agent-shell-fork-tree-cache-directory` (default `~/.emacs.d/var/agent-shell-fork-tree/`) as atomic UTF-8 JSON files with mode `0600`. They contain conversation text, IDs, labels and preview data, but no executable agent configuration. Set the directory to nil to disable disk persistence.

Completed turns from a cancelled replay can advance an in-memory/disk checkpoint; the last unfinished replay turn does not. A successful full rebuild replaces the cache only after every read completes, and carries labels across equivalent paths.

## Rebuild performance

Redraws are content-driven, not timer-driven. Progress messages update the header only. The tree updates when related endpoints, titles or coverage change; timestamps, reassigned message IDs and unrelated histories do not trigger redraws. Changed rows are reconciled by node/session identity without clearing the buffer. The selected endpoint and window's top row are preserved, and unchanged previews are not rewritten or reinitialized.

Incremental discovery saves the entire cache every `agent-shell-fork-tree-checkpoint-batch-size` history reads (default 16), then flushes pending changes on completion, cancellation or failure. In-memory checkpoints advance on each committed replay. An abrupt Emacs/process crash can lose the unsaved batch; set the batch size to 1 for per-history disk durability. An unchanged scan performs no disk writes. Full rebuilds still save only after success.

History discovery uses up to `agent-shell-fork-tree-scan-concurrency` independent ACP clients (default 8) when the backend advertises both `session/fork` and `session/delete`. Each client reads a private temporary fork, and completed histories are committed in the same priority order as serial discovery even if RPCs finish out of order. Backends without deletable forks remain serial and load original sessions directly.

See [PERFORMANCE.md](PERFORMANCE.md) for reproducible baseline and leave-one-out ablation experiments, raw measurements, and backend/GUI measurement limitations.

## Failure and cleanup

Incremental failure reports its reason and asks once whether to perform a full rebuild. Declining retains the existing tree. Full rebuild failure also retains the old cache and does not recursively prompt or retry.

When both fork and delete are advertised, history is read through a temporary fork on the worker's connection and then deleted. Otherwise, the worker loads the original session directly; backend writer restrictions surface as ordinary read failures. This avoids creating undeletable inspection forks on backends without deletion.

`session/delete` follows the backend's semantics; some implementations archive rather than physically erase records. Connection failure or timeout may leave a temporary copy. Closing the preview does not abandon the hidden worker's cleanup callbacks.

The current source must be idle before rebuilding. Loading may still replay an entire history even though local indexing resumes from a checkpoint. Shared visible history is not proof of actual backend fork ancestry. No operation rolls back files or Git state.

## Files and tests

- `agent-shell-fork-tree.el`: interactive tree, preview, automatic mode and session actions.
- `agent-shell-fork-tree-store.el`: normalized turns, parent-scoped indexes and private cache.
- `agent-shell-fork-tree-acp.el`: paginated discovery, replay and temporary-session cleanup.
- [DESIGN.md](DESIGN.md): functional design in Chinese.

Run tests through an existing Emacs server with the dependencies on `load-path`:

```sh
emacsclient --eval '
(load "/absolute/path/agent-shell-fork-tree/tests/run-tests.el" nil t)
'
```

The runner uses ERT without exiting Emacs. Integration tests start a local Python ACP fixture and require `python3`; they use no model or credentials. They cover pagination, preexisting and identical forks, changed fork IDs, direct loading without delete, cached refresh, incremental mismatch/full replacement, native continue/fork, and failed restore without silent creation of an empty session. Unit tests cover fingerprints, ID collisions, cancellation, partial checkpoints, automatic mode and cache round trips.

License: GPL-3.0-or-later.
