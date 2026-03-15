## Local instructions

- Start by trying to evaluate some elisp once per session. When interacting with Emacs use `emacsclient --socket-name $HOME/.emacs.d/server/server`.
  - emacsclient --socket-name "$HOME/.emacs.d/server/server" -e '(message "hello from ai")'
- Always reload `init.el` after making changes (e.g., `M-x eval-buffer` or `load-file`).
- After reloading or when the user says something failed, inspect the `*Messages*` and `*Warnings*` buffer for errors and report any issues. Start by looking at 800 characters; if it's not enough, fetch more. Do this in a single elisp request using emacsclient.
- When asked to remove `elpa/`, restart Emacs afterward to test package installation.
- Do not tell the user to reload; perform the reload yourself.
- When you hit parse errors in Emacs Lisp, run `check-parens`. To locate unmatched parens, run `check-parens`, and use the point location it reports in the same buffer. The cursor will move to the error.
- After restarting Emacs, always run a simple elisp eval to confirm it responds.
- When adding or updating a package from a repo, look up the latest commit SHA and add a `t-package` entry with the host/provider, repo path, and pinned 7-character SHA so it installs on the next package run.
- When adding a new package, inspect its dependencies and list them.
- Prefer `:deps` for dependencies in `t-package` entries instead of separate `t-package` blocks.
- After adding a package definition, always run `M-x t-rescan-packages`. Never install queued packages yourself.
- To update a package: fetch the latest 7 char commit SHA, update the `t-package` entry, run `M-x t-rescan-packages`, and confirm the package shows as `outdated` (not `installed`).
