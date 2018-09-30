## .emacs.d

```
cd && git clone git@github.com:torgeir/.emacs.d.git
ln -s ~/.emacs.d/.emacs
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

sudo apt-get install fonts-inconsolata
sudo fc-cache -fv
```

### win

Install from cygwin or https://ftp.gnu.org/gnu/emacs/windows/. Win gnu emacs
seem to want files in `C:\Users\<user>\AppData\Roaming\`, and doesn't like
symlinks, so you could..

- clone the repo and copy `.emacs.d` to `C:\Users\<user>\AppData\Roaming\.emacs.d`
- copy `.emacs.d/.emacs` to `C:\Users\<user>\AppData\Roaming\.emacs`

### emacsclient - terminal and gui

In a folder on your `PATH`, e.g. `~/bin`:

```
ln -s ~/.emacs.d/emacsclient ~/bin/e
ln -s ~/.emacs.d/emacsclient-terminal ~/bin/em
```

### deps

these need to exist on `PATH`:

- git
- w3m
- eslint babel-eslint jsonlint eslint-plugin-react tern flow-bin
- browser-sync

#### fonts

install these fonts on the system

- `~/.emacs.d/Symbola.ttf`
- `~/.emacs.d/FiraCode_1.204.zip`





