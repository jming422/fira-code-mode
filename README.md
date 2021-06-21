# fira-code-mode

[![MELPA](https://melpa.org/packages/fira-code-mode-badge.svg)](https://melpa.org/#/fira-code-mode)

[![screenshot](https://raw.githubusercontent.com/jming422/fira-code-mode/master/screenshots/screenshot2.png)](https://github.com/jming422/fira-code-mode/tree/master/screenshots)

This is a simple minor mode for Fira Code ligatures, built from [these instructions on the FiraCode repo](https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-prettify-symbols).

You don't need to use Fira Code as your main font in Emacs for this to work! Using this minor mode will give you just the ligatures from the Fira Code font; it won't alter your fonts or faces in any other way. (Of course, you're free to use Fira Code as your main font alongside this package - that's what I do!)

## Getting Started

1. Install `fira-code-mode`. (from MELPA: `M-x package-install RET fira-code-mode RET`)

2. Install the `Fira Code Symbol` font to your system by `M-x fira-code-mode-install-fonts RET`

   - Or you can install the font manually. Download the font [here](https://raw.githubusercontent.com/jming422/fira-code-mode/master/fonts/FiraCode-Regular-Symbol.otf)
     and follow the instructions [here](https://github.com/tonsky/FiraCode/wiki/Installing).
   - _Note:_ This `Fira Code Symbol` font is not the same as the real `Fira Code` font, and you need to install `Fira Code Symbol` whether you have regular `Fira Code` on your system or not.
   - Thanks to [@siegebell](https://github.com/siegebell) for creating this font.

3. Enable `fira-code-mode` in your config, either using `(global-fira-code-mode)` or by adding `fira-code-mode` to whichever hooks you like.

4. Done, enjoy the ligatures!

## Example Configs

With [use-package](https://github.com/jwiegley/use-package) (this is the config that I use personally):

```elisp
(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes
```

Feel free to remove or change the `:custom` and `:hook` values of course, but those are the ones I've found useful. Most of the ligatures I've disabled are purely preferential; some of them conflicted with other syntax styling for e.g. Clojure reader macros.

If you want to use the global minor mode with use-package:

```elisp
(use-package fira-code-mode
  :config (global-fira-code-mode))
```

An example config without use-package:

```elisp
(require 'fira-code-mode)
(custom-set-variable 'fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off

;; Enable fira-code-mode automatically for programming major modes
(add-hook 'prog-mode-hook 'fira-code-mode)
;; Or, you can use the global mode instead of adding a hook:
(global-fira-code-mode)
```

## Customization

- `fira-code-mode-disabled-ligatures`: Add a string to this list to prevent that string from being displayed with a ligature.
- `fira-code-mode-enable-hex-literal` (defaults to `t`): When non-nil, display the \"x\" in hex literals (like 0x12) with a ligature.
  - Note: Adding \"x\" to the list of disabled ligatures does not effect this option; see this option's docstring for more info.

## Troubleshooting Common Issues

### Ligatures are rendered using the wrong font (#16)

If you see unexpected symbols where ligatures should be, then it's likely that some other font on your system is being used to resolve the prettified symbols instead of Fira Code Symbol. You should be able to resolve this by:

1.  Ensuring you have the `Fira Code Symbol` font installed - see Getting Started step 2 above
2.  If you have `Fira Code Symbol` installed, but another font is still being used to display the ligatures, then add this function call to your config: `(fira-code-mode-set-font)`
    - With `use-package`, this should go in your `:config`. Otherwise, it can go anywhere after `(require 'fira-code-mode)`.

## Contributing

This is the first Emacs package that I've made, and I'm making it available in hopes that it will make your lives easier as it did mine. I welcome suggestions and contributions, but here are a couple things to be aware of:

- I'm quite new to writing Emacs Lisp, so please be nice! I know I have a lot to learn.
- I don't have a ton of time to spend on this, but I'll do my best to respond to issues and PRs quickly!
- If you've read this far, thank you! I'm very grateful for the chance to make something useful/interesting.
