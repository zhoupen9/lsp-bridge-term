lsp-bridge-term - lsp-bridge terminal support for Emacs
---
This package provides a subset of LSP implementations for terminal environment based on [lsp-bridge](https://github.com/manateelazycat/lsp-bridge). It was coded in a simply streight way. For better coding experience, use *lsp-brdige* in GUI environment instead.

# Features
Support display popup in terminal.
- Code completion
- Code action
- Documentation
- Signature
- Diagnostics

# Installation
## prerequisite
### lsp-bridge
Install [lsp-bridge](https://github.com/manateelazycat/lsp-bridge) and make sure it works on your machine before trying this package.

### emacs-popon
This package uses `emacs-popon` to display popup with `overlay` in terminal.
`emacs-popon` is not in `elpa/melpa` repository, you need to clone repository from codeberg.
```shell
git clone https://codeberg.org/akib/emacs-popon.git
```
and then add  to `load-path`.

### nerd-font
Download and install nerd fonts, and configure terminal to use one of nerd mono font.

## init script
### use-package
Add following configuration when use `use-package`.
```lisp
(unless (display-graphic-p)
  (use-package popon)
  (use-package lsp-bridge-term
    :init
    (add-hook
     'emacs-startup-hook
     (lambda ()
       (with-eval-after-load 'acm (require 'lsp-bridge-term))))))
```
# Keymap
Default key bindings

| Key   | Command                     | Description                                          |
|-------|-----------------------------|------------------------------------------------------|
| C-n   | lsp-bridge-term-select-next | select next candidate in menu or scroll doc up       |
| Down  | lsp-bridge-term-select-next | same as C-n                                          |
| C-p   | lsp-bridge-term-select-prev | select previous candidate in menu or scroll doc down |
| Up    | lsp-bridge-term-select-prev | same as C-p                                          |
| C-m   | lsp-bridge-term-complete    | select menu or close doc                             |
| Enter | lsp-bridge-term-complete    | same as C-m                                          |
| C-g   | lsp-bridge-term-cancel      | cancel any popup if present                          |

Use `lsp-bridge` key bindings to popup `documentation` and `code action`.
# Command
- `lsp-bridge-term-diagnostics-inline-toggle` toggle display diagnostics 

# Options
- `lsp-bridge-term-doc-line-max` maximum line length to display doc popup. When `window` space is insufficient, open doc in `other window`.
- `lsp-bridge-term-popup-min-height` default 5, minimum height to display popup.
- `lsp-bridge-term-popup-max-height` default 25, maximum height to display popup. When displaying content is more than this, popup only display content of this height, use `C-n` to scroll popup to see hiding content.
- `lsp-bridge-term-diagnostics-inline` defualt nil, when t, display diagnostic message in `overlay`.

# Customization
Customization not supported yet, which means nerd font icons, faces and etc and *hardcoded*, helps required.

# Known issues and TODOs
This package simply override `lsp-bridge-` functions to handle LSP messages from python end, instead of implementing `acm`.
- Features supported by `acm` may missing in terminal environment
- Performance may degrade in terminal environment
- Without `timer` and display popup directly when receiving LSP message brings *input lag*
- Varies backend not supported.

# Screenshots
![code completion](screenshots/code-completion-popup-menu.png "Code Completion")
![code action](screenshots/code-action-popup-menu.png "Code Action")
![signature](screenshots/signature-popup.png "Signature")
![documentation](screenshots/doc-popup.png "Documentation")
![diagnostic indicator](screenshots/diagnostic-indicator-overlay.png "Diagnostic Indicator")
![diagnostic message](screenshots/diagnostic-message-overlay.png "Diagnostic Message")
