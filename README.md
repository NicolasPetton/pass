# pass

A major-mode to manage your
[password-store](http://passwordstore.org/) (pass) keychain.  The
keychian entries are displayed in a directory-like structure.

## Installing

Use [melpa](http://melpa.milkbox.net).


## Getting started

This library depends on `password-store.el`.

    M-x pass

The following keybindings are available:

- `i`: Insert a new entry (With a prefix argument, generate the password)
- `n`: Go to the next entry
- `p`: Go to the previous entry
- `M-n`: Go to the next directory
- `M-p`: Go to the previous directory
- `k`: Remove the entry at point
- `s`: Trigger iSearch
- `r`: Trigger iSearch (backward)
- `?`: Help
- `g`: Update the password-store buffer
- `RET` or `v`: Go to the entry at point
- `q`: Quit pass

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [LICENSE][]. Copyright (c) 2015 Nicolas Petton & Damien Cassou.


[CONTRIBUTING]: ./CONTRIBUTING.md
[LICENSE]: ./LICENSE
