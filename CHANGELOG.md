# Changelog

## Master (Unreleased)



### Breaking Changes

#### Changes for THING register
The built-in thing definition shipped by meow should be more close to what Emacs gives us.
So two previously added, complex things are removed. A helper function is added, so you can easily
register new thing with Emacs things, functions, syntax descriptions or regexp pairs.

- A helper function `meow-thing-register` is provided, check its document for usage.
- Thing `indent` and `extend` has been removed.
- Variable `meow-extend-syntax`(undocumented) has been removed.
- Add custom variable `meow-thing-selection-directions`.
- `meow-bounds-of-thing` will create a backward selection by default.

### Enhancements
* Remove paredit shims, no longer needed.
* [#110](https://github.com/meow-edit/meow/issues/110) Only disable hint overlay for modes in `meow-expand-exclude-mode-list`.
* Add custom variable `meow-motion-remap-prefix.`
* Remove `dash.el` and `s.el` from dependencies.
* Add more defaults to `meow-mode-state-list`.
* `meow-swap/sync-grab` will grab on current position, thus you can go
  back to previous position with `meow-pop-grab` later.

## 1.1.1 (2021-12-06)

### Enhancements
* Prevent user call `meow-mode` directly.

### Bugs fixed
* Fix for disabling `meow-global-mode`.

## 1.1.0 (2021-12-06)

### Features
* [#99](https://github.com/meow-edit/meow/pull/99) Add `meow-tutor`.

### Breaking Changes
* rename bmacro -> beacon.

### Enhancements
* Add `meow-expand-hint-counts`.
* Add more defaults to `meow-mode-state-list`.
* Improve color calculation in beacon state.
* Support change char in beacon state (as the fallback behaviour for change, by default).
* Support `expand char` selection in beacon state.
* Support `kill` in beacon state.
* Add thing `sentence`, default bound to `.`.

### Bugs fixed
* Fix expand for `meow-line`.
* Fix nils displayed in keypad popup.
* Fix C-S- and C-M-S- in keypad.
* Eval `meow-motion-overwrite-define-key` multiple times cause invalid remap.
* Set `undo-tree-enable-undo-in-region` for undo-tree automatically.

## 1.0.1 (2021-11-30)
### Bugs fixed
* `SPC SPC` doesn't work in motion state.

## 1.0.0 (2021-11-28)
Initial release.
