## .emacs.d

```
cd && git clone git@github.com:torgeir/.emacs.d.git
ln -s ~/.emacs.d/.emacs
```

for spacemacs, add

```
cd && git clone https://github.com/syl20bnr/spacemacs
ln -s ~/.emacs.d/.spacemacs
```

### installing emacs

### os x

```
brew tap d12frosted/emacs-plus
brew install emacs-plus --HEAD --with-natural-title-bar --without-spacemacs-icon
```

### ubuntu

```
sudo add-apt-repository -y ppa:ubuntu-elisp
sudo apt-get update
sudo apt-get install emacs-snapshot
```

### win

Install from cygwin or https://ftp.gnu.org/gnu/emacs/windows/. Win gnu emacs
seem to want files in `C:\Users\<user>\AppData\Roaming\`, and doesn't like
symlinks, so you could..

- clone the repo and copy `.emacs.d` to `C:\Users\<user>\AppData\Roaming\.emacs.d`
- clone spacemacs and copy `spacemacs`, copy it to `C:\Users\<user>\AppData\Roaming\spacmacs`
- copy `.emacs.d/.emacs` to `C:\Users\<user>\AppData\Roaming\.emacs`
- copy `.emacs.d/.spacemacs` to `C:\Users\<user>\AppData\Roaming\.spacemacs`

### emacsclient - terminal and gui

In a folder on your `PATH`, e.g. `~/bin`:

```
ln -s ~/.emacs.d/emacsclient ~/bin/e
ln -s ~/.emacs.d/emacsclient-terminal ~/bin/em
```

### deps

remember to byte compile ~/.emacs.d/site-lisp/t-doom-modeline/,
`(byte-recompile-file "~/.emacs.d/site-lisp/t-doom-modeline/t-doom-modeline.el"
t 0)`

these need to exist on `PATH`:

- git
- w3m
- eslint babel-eslint jsonlint eslint-plugin-react tern flow
- browser-sync

#### fonts 

install these fonts on the system

- `~/.emacs.d/Symbola.ttf`
- `~/.emacs.d/FiraCode_1.204.zip`





