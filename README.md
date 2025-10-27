# modpatch.el

`modpatch.el` is an Emacs minor mode for producing and maintaining patch files instead of editing the original sources in place.

It solves the following workflow:

* You want to edit `Assets/Lua/foo.lua`, but you are not allowed to (or don't want to) commit direct changes there.
* Instead, you want to maintain one or more `.patch` files somewhere under your mods directory (for example `Mods/MyBalanceMod/Lua/foo.lua.patch`) that describe how `foo.lua` should be changed.
* You want Emacs to show you the final, patched version while you're working, and treat that as the "real" buffer text.
* When you save, you do not want to overwrite `Assets/Lua/foo.lua`. You want to regenerate the unified diff and write that to your patch file(s).
* Sometimes you really do want to edit the real upstream file, and when you do, you want your patch file(s) automatically rewritten so they still produce the desired result against the new upstream version.

`modpatch.el` makes that workflow first-class.

It provides:

1. A "patch-authoring" mode where saving writes `.patch` files instead of writing to the original file.
2. A "rebase" mode where you temporarily edit the original file on disk and patches get regenerated on save.
3. Support for multiple patch variants per base file, and an interactive command to switch between them.
4. Automatic activation: reopening a file you've patched automatically shows the patched view and puts you back in patch-authoring mode.
5. Persistence of associations (which base file maps to which patch files, and what the intended modded result is).

## Concepts

### Base file

The original file in the game/assets tree. Example:
`/game/Assets/Lua/testmodule.lua`

### Patch file(s)

Unified diff files that describe modifications to the base file. Example:
`/mods/balance/Lua/testmodule.lua.patch`
`/mods/cheats/Lua/testmodule.lua.patch`

Each patch file corresponds to a variant. You may choose which one you're authoring at any given time.

### Desired text

The full text of "what you want the game to see" for that file after all edits are applied. In normal patch-authoring mode, your buffer shows the desired text, not the original base file.

`modpatch.el` keeps track of the most recent desired text for each base file so it can:

* regenerate patches,
* restore buffers after reload,
* rebase cleanly when the base file changes.

## Installation

Place `modpatch.el` in some directory, for example `~/.emacs.d/modpatch/`.

Then in your init:

```elisp
(add-to-list 'load-path (expand-file-name "modpatch" "~/.emacs.d"))
(load-file (expand-file-name "modpatch/modpatch.el" "~/.emacs.d"))
```

This loads the package, defines the minor modes, and also loads any saved associations from the default `modpatch-associations-file` path (see below).

## Associations file

`modpatch.el` keeps a global hash table mapping each base file to:

* its patch files,
* its last known desired text.

This table is persisted to disk so Emacs can restore state between sessions.

### Configuration

You can choose where that state is stored by setting `modpatch-associations-file` before calling `modpatch-load-associations`, like this:

```elisp
(setq modpatch-associations-file "/path/to/modpatch-assoc.el")
(modpatch-load-associations)

(add-to-list 'load-path (expand-file-name "modpatch" "~/.emacs.d"))
(load-file (expand-file-name "modpatch/modpatch.el" "~/.emacs.d"))
```

This does two things:

1. It sets where associations will be read from and written to.
2. It then loads the table from that file before you start working.

You can also change the associations file later while Emacs is already running. For example:

```elisp
(setq modpatch-associations-file "/some/other/location/alt-assoc.el")
(modpatch-load-associations)
```

After that call, `modpatch.el` will start using the new file for persistence going forward. This lets you maintain different sets of mod associations in different projects.

## Lifecycle overview

1. You open a base file.

2. `modpatch.el` sees that the file has patches associated with it, applies the patch (or selected patch) to generate the desired text, replaces the buffer contents with that desired text, and enables `modpatch-mode`.

   At this point:

   * What you see in the buffer is your modded version, not the raw file on disk.
   * Saving does not touch the base file. It regenerates patch files.

3. You edit and save:

   * The buffer diff is computed against the current on-disk base file.
   * The unified diff is written to the active patch file (or all associated patch files if no active one is selected).
   * The buffer is marked clean (Emacs believes it was saved).
   * The base file on disk is left untouched.

4. You need to update upstream (for example because the game updated its Lua scripts). You switch to rebase mode:

   * The buffer is replaced with the true on-disk base file.
   * Saves now write to disk normally.
   * After each save, modpatch automatically regenerates the patch file(s) so they still apply cleanly.
   * When done, you exit rebase mode; modpatch reconstructs the desired text from the latest patch file(s) on disk and puts you back in patch-authoring mode.

5. You can maintain multiple patch variants for the same base file and interactively switch which variant you're authoring.

## Core data structures

Internally, `modpatch.el` keeps a hash table `modpatch--table`:

* Key: absolute path of a base file.
* Value: plist with

  * `:patches` → list of absolute patch file paths for that base file.
  * `:desired` → last known desired text for that base file.

Each buffer visiting a base file also has buffer-local variables:

* `modpatch--base-file`: the absolute path to the base file on disk.
* `modpatch--active-patch-file`: the currently selected patch variant for this buffer (or nil if not narrowed to a single variant).
* `modpatch--rebase-mode-p`: non-nil if we are currently editing the real base file on disk (rebase mode).

## Enabling modpatch-mode automatically

`modpatch.el` installs a `find-file-hook` (`modpatch-maybe-activate`).

When you open a file `X`:

* If `X` does not have an entry in `modpatch--table`, nothing special happens; you just visit the file normally.
* If `X` does have an entry:

  * The base file on disk is read.
  * The associated patch file(s) are applied to produce the desired text. If we are tracking a currently active patch variant, that variant is used. Otherwise all known patches are applied in order.
  * The buffer contents are replaced with the desired text.
  * The buffer is marked unmodified.
  * `modpatch-mode` is turned on in that buffer.

From that point on, you are in patch-authoring mode for that file.

## Patch-authoring mode (normal mode)

This is the default mode when `modpatch-mode` is on and we are not in rebase mode.

In this mode:

* The buffer contents are the desired, modded version.
* The real file on disk is not shown.
* Saving does not write the buffer to disk. Instead, `modpatch.el` intercepts the save via `write-contents-functions`.

When you hit `C-x C-s`:

1. It reads the current on-disk content of the base file.
2. It diffs that against your buffer content, generating a unified diff.
3. It writes that diff to one or more patch files:

   * If `modpatch--active-patch-file` is non-nil, only that patch file is updated.
   * Otherwise, all patch files in `:patches` are updated.
4. It updates the `:desired` entry for this base file in `modpatch--table` to match the buffer content.
5. It marks the buffer clean (not modified).

The base file on disk is left unchanged.

Key bindings in this mode:

* `C-c m a`
  `modpatch-add-patch-target`
  Add a new patch file path for this base file. After calling this, saving will start updating that patch file.
* `C-c m p`
  `modpatch-select-active-patch`
  Choose which patch file you are currently authoring. This does three things:

  * Prompts you with completion over all known patch files for this base.
  * Applies only that selected patch file to the base file to compute what the buffer should look like.
  * Replaces the current buffer with that result and records that selected patch file in `modpatch--active-patch-file`.
* `C-c m r`
  `modpatch-enter-rebase-mode`
  Switch into rebase mode (described below).
* `C-c m R`
  `modpatch-exit-rebase-mode`
  Switch back to patch-authoring mode (when already in rebase mode).
* `C-c m s`
  `modpatch-save-associations`
  Persist the current association table (`modpatch--table`) to disk immediately.

### Multiple patch variants

A single base file can have multiple `.patch` files associated with it. Example:

* `/mods/Balance/Lua/testmodule.lua.patch`
* `/mods/Cheats/Lua/testmodule.lua.patch`

These represent different variants.

When in patch-authoring mode:

* Run `C-c m p` (`modpatch-select-active-patch`).
* You will be prompted to pick one of the known patch files.
* The buffer will be rebuilt to show what the base file would look like after applying only that chosen patch file.
* From now on, saving updates only that chosen patch file.
* The choice is remembered in `modpatch--active-patch-file` for this buffer.

## Rebase mode (editing the real file on disk)

Sometimes upstream changes and you need to reconcile your mod with a new version of the base file. This is what rebase mode is for.

Enter rebase mode with `C-c m r` (`modpatch-enter-rebase-mode`) while in patch-authoring mode.

When you enter rebase mode:

1. Your current buffer (which is showing the desired text) is committed to disk as the authoritative desired state:

   * The unified diff between the on-disk base file and your buffer is generated.
   * The associated patch file(s) are updated.
   * The association entry’s `:desired` is updated to match your buffer contents.
2. Then the buffer is replaced with the real on-disk content of the base file. You now see the true upstream file.
3. `modpatch--rebase-mode-p` is set to non-nil.
4. An `after-save-hook` is installed so that every time you save in this mode, `modpatch.el` regenerates the patch files against the new upstream content.

Effectively, you are now editing the base file directly. Saving (`C-x C-s`) writes to disk like normal.

After each save in rebase mode:

* `modpatch.el` computes the diff between the new base file text on disk and the last known desired text (the modded result you are targeting).
* That diff is written to the relevant patch file(s) so the patch stays valid against the updated base.

This is what keeps your mod patches from going stale when the upstream file changes.

When you are done rebasing, run `C-c m R` (`modpatch-exit-rebase-mode`).

When you exit rebase mode:

1. `modpatch.el` recomputes the modded view to show in the buffer. It does this by reading the latest patch file(s) off disk, applying them to the now-current base file, and rebuilding the desired text fresh. This ensures you see the newest patch output, not a cached copy.

   * If you have an active patch file (`modpatch--active-patch-file`), it prefers that one so you remain focused on the same variant you were editing.
2. The buffer is replaced with that recomputed modded view.
3. The `after-save-hook` is removed.
4. `modpatch--rebase-mode-p` is set back to nil.
5. You are back in patch-authoring mode, where saving again updates patch files instead of touching the base file on disk.

You can re-enter rebase mode at any time.

## Commands summary

All commands below assume `modpatch-mode` is active in the buffer.

* `modpatch-add-patch-target`
  Key: `C-c m a`
  Add a patch file to the association for this base file. You typically point it somewhere in your mod directory, e.g. `Mods/MyMod/Lua/testmodule.lua.patch`.

* `modpatch-select-active-patch`
  Key: `C-c m p`
  Choose which patch file is currently active in this buffer. The buffer is rebuilt from disk using only that patch, and further saves update only that patch.

* `modpatch-enter-rebase-mode`
  Key: `C-c m r`
  Switch to rebase mode:

  * Save your current desired modded result into patch form.
  * Show you the real upstream file.
  * From now on, saving writes upstream and auto-regenerates the patch file(s) to stay aligned.

* `modpatch-exit-rebase-mode`
  Key: `C-c m R`
  Leave rebase mode:

  * Reconstruct the modded view from the (now updated) base file plus the latest patch file(s).
  * Return to patch-authoring behavior.

* `modpatch-save-associations`
  Key: `C-c m s`
  Write the current association table (`modpatch--table`) to `modpatch-associations-file`. The file is Lisp-readable. You can version this file in git alongside your mods.

* `modpatch-load-associations`
  Manually reload the association table from `modpatch-associations-file`. You can call this after changing `modpatch-associations-file` to point Emacs at a different set of mod metadata.

## Typical workflow

1. Open `Assets/Lua/testmodule.lua`.
   If this file has never been patched before, Emacs opens it normally.

2. Run `M-x modpatch-mode`.
   The mode records the file path and sets up buffer-local state.
   Then run `C-c m a` (`modpatch-add-patch-target`) and give it a patch file path, for example:
   `Mods/MyBalanceMod/Lua/testmodule.lua.patch`

3. Edit the file in-place (you're seeing your desired version of the file).

4. `C-x C-s`.
   Instead of saving to `Assets/Lua/testmodule.lua`, modpatch:

   * Differs the current on-disk original vs your buffer.
   * Writes that diff as a unified patch to `Mods/MyBalanceMod/Lua/testmodule.lua.patch`.
   * Marks the buffer unmodified.

5. Later, upstream updates `Assets/Lua/testmodule.lua`.
   Run `C-c m r` to enter rebase mode.

   * Your current mod view is captured and patches updated.
   * Buffer now shows the new upstream version directly.
     Edit and save; each save rewrites your patch file so it still represents your desired changes relative to the new upstream file.

6. Run `C-c m R` to exit rebase mode.

   * Buffer is reconstructed by applying your latest patch file(s) to the updated upstream.
   * You are back in patch-authoring mode.

7. If you maintain multiple patch variants (for example a “balance” version and a “cheats” version), you can switch which variant you're editing via `C-c m p` (`modpatch-select-active-patch`). The buffer will be rebuilt using only that selected patch file, and future saves will update that specific patch file.

## Notes

* `diff` and `patch` must be available in your environment. `modpatch.el` shells out to:

  * `diff -u` to generate unified diffs
  * `patch` to apply them
    If you are on Windows, you can install these via MSYS2 or Git for Windows. Alternatively, you can rework `modpatch--generate-diff` and `modpatch--apply-patch` to use pure Emacs primitives (`diff-no-select`, `epatch-buffer`).

* Paths in the generated patch header are normalized so that patch applies cleanly regardless of the actual directory layout. Internally we rewrite the header lines (`---` / `+++`) to a neutral filename before running `patch`.

* `modpatch.el` never silently writes to the base file in patch-authoring mode. The only time it will write to the real base file is in rebase mode, which you enter explicitly with `C-c m r`.

* You can keep the association file (`modpatch-associations-file`) under version control in your mod project so that your patch relationships, desired texts, and active variants survive across sessions and machines.

This completes the feature set:

* Patch-authoring mode with redirected save.
* Rebase mode for upstream changes.
* Multiple patch variants per base file and interactive switching.
* Automatic reopening into the patched view.
* Persistence across sessions, with configurable storage and reload.
