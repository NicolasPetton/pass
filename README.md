# pass-mode

A major-mode to manage your password-store (pass) keychain.

All entries are displayed in a directory-like structure.

# Getting started

This library depends on `password-store.el`.

    M-x password-store

The following keybindings are available:

- `i`: Insert a new entry (With a prefix argument, generate the password)
- `n`: Go to the next entry
- `p`: Go to the previous entry
- `k`: Remove the entry at point
- `s`: Trigger iSearch
- `r`: Trigger iSearch (backward)
- `?`: Help
- `g`: Update the password-store buffer
- `RET` or `v`: Go to the entry at point
- `q`: Quit pass-mode
