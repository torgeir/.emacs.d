## .emacs.d

```
cd && git clone https://github.com/torgeir/.emacs.d
```

### installing

### os x

```
brew install emacs --with-cocoa --with-glib --with-librsvg --with-mailutils --with-gnutls --with-ctags --with-imagemagick
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

In a folder on your `PATH`:

```
ln -s emacsclient e
ln -s emacsclient-terminal em
```

### deps

these need to exist on `PATH`:

- git
- w3m
- browser-sync





