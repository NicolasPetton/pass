# pass

A major-mode to manage your
[password-store](http://passwordstore.org/) (pass) keychain.  The
keychain entries are displayed in a directory-like structure.

Canonical repository: https://gitlab.petton.fr/nico/pass.

## Installing

Use [melpa](https://melpa.org/).


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

## Pass in Emacs

Users of this package may also be interested in functionality provided
by other Emacs packages dealing with pass:

- [password-store](https://git.zx2c4.com/password-store/tree/contrib/emacs/password-store.el): password store (pass) support;
- [auth-password-store](https://github.com/DamienCassou/auth-password-store): integrate Emacs' auth-source with password-store;
- [helm-pass](https://github.com/jabranham/helm-pass): helm interface for pass.

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [LICENSE][]. Copyright (c) 2015-2016 Nicolas Petton & Damien Cassou.


[CONTRIBUTING]: ./CONTRIBUTING.md
[LICENSE]: ./LICENSE
