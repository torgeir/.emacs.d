# Emacs config (literate)

- Source of truth: `readme.org` in `DOOMDIR` (`/home/torgeir/.doom.d`).
- Tangle + reload: `emacsclient -e '(+literate/reload)'` (alias of `doom/reload`).
- If no server: `emacs --batch --eval "(require 'org)" --eval "(setq org-confirm-babel-evaluate nil)" --eval "(with-current-buffer (find-file-noselect \"/home/torgeir/.config/doom.d/readme.org\") (org-babel-tangle))"`.
- Always run `M-x check-parens` on `config.el` after tangling; do this before any restart.
- Restart: `emacsclient -e '(doom/restart)'` or `emacsclient -e '(doom/restart-and-restore)'`.
- When committing changes to `readme.org`, prefer partial staging (`git add -p readme.org`) and only commit the relevant hunks.

What to look for if things break:
- Unclosed `#+begin_src`/`#+end_src` blocks (they silently skip later tangles).
- Wrong `DOOMDIR` or stale `config.el`.
- `org-directory`/`org-agenda-files` drift from `~/Dropbox/org`.
- Failing `check-parens` on `config.el` (syntax error in tangled output).

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. You will NEVER run git push.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. This is MANDATORY:
   ```bash
   bd sync
   git status
   ```
5. **Clean up** - Clear stashes, prune branches
6. **Verify** - All changes committed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- NEVER run git push
- NEVER that leaves work stranded locally
