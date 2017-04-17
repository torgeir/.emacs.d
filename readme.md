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
brew install emacs --with-cocoa --wi th-ctags --with-gnutls --with-imagemagick@6 --with-librsvg
```

### ubuntu

```
sudo add-apt-repository -y ppa:ubuntu-elisp
sudo apt-get update
sudo apt-get install emacs-snapshot
```

### win

Install from cygwin or https://ftp.gnu.org/gnu/emacs/windows/. Win gnu emacs seem to want files in `C:\.emacs.d` or `C:\Users\<user>\AppData\Roaming\.emacs.d`.

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
- eslint babel-eslint jsonlint eslint-plugin-react tern flow
- browser-sync

install `~/.emacs.d/Symbola.ttf` on the system





