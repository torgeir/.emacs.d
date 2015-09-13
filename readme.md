## .emacs.d

```
cd && git clone https://github.com/torgeir/.emacs.d
```

### installing

### os x

```
brew install emacs --HEAD --use-git-head --with-srgb --with-cocoa --with-glib --with-librsvg --with-gnutls --with-ctags --with-imagemagick
```

### ubuntu

```
sudo add-apt-repository -y ppa:ubuntu-elisp
sudo apt-get update
sudo apt-get install emacs-snapshot
```

### win

Install from cygwin or https://ftp.gnu.org/gnu/emacs/windows/. Win gnu emacs seem to want files in `C:\.emacs.d` or `C:\Users\<user>\AppData\Roaming\.emacs.d`.

### deps

these need to exist on `PATH`:

- git
- w3m
- browser-sync





