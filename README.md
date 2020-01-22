fira-code-mode
===
### Minor mode for Fira Code ligatures using prettify-symbols.

This is a simple minor mode for Fira Code ligatures, built from these instructions: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-prettify-symbols

Requires installing the Fira Code Symbol font from here: https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632

Note that you aren't required to also use Fira Code as your main font in Emacs for this to work! Using this minor mode will give you just the ligatures from the Fira Code font; it won't alter your fonts or faces in any other way.


## Getting Started

1. First, you'll have to install the Fira Code Symbol font created by @siegebell posted to [this issue on the Fira Code repo](https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632) [(direct download link)](https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip).
  - This process varies from system to system, but Fira Code's repo provides [some good instructions](https://github.com/tonsky/FiraCode/wiki/Installing).

2. Next, clone this repo someplace. I keep mine in `~/.emacs.d/git-lisp/`, but it's completely up to you as to where you want to keep it.
```bash
$ git clone https://github.com/jming422/fira-code-mode
```

3. Enable `fira-code-mode` in your config. Here are some examples:
  - With [use-package](https://github.com/jwiegley/use-package) (this is the config that I use personally):
  ```elisp
  (use-package fira-code-mode
    :ensure nil
    :load-path "git-lisp/fira-code-mode"
    :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
    :hook prog-mode) ;; Enables fira-code-mode automatically for modes descended from prog-mode
  ```
  Feel free to remove or change the `:custom` and `:hook` values of course, but those are the ones I've found most useful. (Some of the ligatures I've disabled are purely preferences, some of them conflicted with syntax styling for e.g. Clojure reader macros)

  - Without use-package:
  ```elisp
  (require 'fira-code-mode "path/to/fira-code-mode/fira-code-mode.el")
  (custom-set-variable 'fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  (add-hook 'prog-mode-hook 'fira-code-mode) ;; Enables fira-code-mode automatically for modes descended from prog-mode
  ```
  Again, the last two lines are optional, but they're a reasonable starting point.

4. Enjoy the ligatures!


## Screenshots

Coming soon


## Contributing

This is the first Emacs package that I've made, and I'm making it available in hopes that it will make your lives easier as it did mine. I welcome suggestions and contributions with open arms, but here are a couple things you should be aware of:
- I'm quite new to writing Emacs Lisp, so please be nice! I have a lot to learn, but that means I need to be taught, not yelled at.
- I don't have a ton of time to spend on this, but I'll do my best to respond to issues and PRs quickly!
- I'm super grateful that would be interested in my work, so thank you for reading this far!
