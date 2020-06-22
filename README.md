Tera Mode for Emacs
======================

This is an Emacs mode for editing with the [Tera template language](https://tera.netlify.app).

[![License GPL3](https://img.shields.io/badge/license-GPL3-blue.svg)](https://github.com/svavs/tera-mode/blob/master/LICENSE.txt)


## Quick Installation

This mode is designed having the Spacemacs configuration in mind, but should work without problems on vanilla Emacs. 

Running `tera-mode` directly from sources is easy but requires a little preparation:

- `git clone https://github.com/svavs/tera-mode.git` into a
  suitable directory, e.g. `~/.spacemacs.d/tera-mode/` where `~`
  stands for your home directory.

To add this mode in Spacemacs, simply add this code to the dotfile under the `dotspacemacs/user-config` section:


```el
  ;; Install custom mode for Tera template language
  (add-to-list 'load-path "~/.spacemacs.d/tera-mode")
  (require 'tera-mode)
```

`tera-mode` supports GNU Emacs version 25.1 or later.


## Contributing

If you followed the above you are just a couple of steps away from
contributing to `tera-mode`.

`tera-mode` is actively seeking contributions from users of `tera`-mode`. 
For more information have a look at [the wiki page on contributing](https://github.com/svavs/tera-mode/wiki/Contributing).


