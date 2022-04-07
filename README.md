# Tera Mode for Emacs

This is an Emacs mode for editing with the [Tera template language](https://tera.netlify.app).

[![License GPL3](https://img.shields.io/badge/license-GPL3-blue.svg)](https://github.com/svavs/tera-mode/blob/master/LICENSE.txt)

The mode is not offering complete support in special circumstances (nested code, custom macros, etc), but is ready for the most use cases.
The `tera-mode` is automatically enabled when opening a file having the `.tera` extension. Instead, when opening an HTML file containing Tera code, is possible to enable the `tera-mode` with `M-x tera-mode`.

## Quick Installation

This mode is designed having the Spacemacs configuration in mind, but should work without problems on vanilla Emacs.

Running `tera-mode` directly from sources is easy but requires a little preparation:

- `git clone https://github.com/svavs/tera-mode.git` into a
  suitable directory, e.g. `~/.spacemacs.d/tera-mode/` where `~`
  stands for your home directory.

`tera-mode` supports GNU Emacs version 25.1 or later.

### Spacemacs

To add this mode in Spacemacs, simply add the following code to the dotfile under the `dotspacemacs/user-config` section:

```el
  ;; Install custom mode for Tera template language
  (add-to-list 'load-path "~/.spacemacs.d/tera-mode")
  (require 'tera-mode)
```

### Doom emacs

To add tera-mode in Doom Emacs, add the following code to the dedicated files:

```el
  ;; package.el
  (package! tera-mode
    :recipe (:host github :repo "svavs/tera-mode"))
```

```el
  ;; config.el
  (require 'tera-mode)
```

After that, running `doom sync` will automatically download and enable the `tera mode`.

## Contributing

If you followed the above you are just a couple of steps away from
contributing to `tera-mode`.

This is the first time I wrote an Emacs mode, hence `tera-mode` is actively seeking contributions from users of `tera-mode`.
If you have some contributions, you can fork this repo and request a merge, it'll be really appreciated.
