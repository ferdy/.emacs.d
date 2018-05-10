# .emacs.d

This is the Emacs configuration I use everyday.

It requires an Emacs version built from development sources, and it is finely
honed to work with:

- [Haskell](https://github.com/commercialhaskell/intero)
- [Clojure and ClojureScript](https://github.com/clojure-emacs/cider)
- [Git](https://magit.vc/)
- [LaTeX](https://www.gnu.org/software/auctex/)
- [Org mode](https://orgmode.org/)

There is a reasonable setup for other programming languages and utilities, just
have a look inside the `lisp` directory if you want to know more.

## First steps

This is how I build and install Emacs on Ubuntu. If you are using a different
operative system, you need to understand how to install the required
dependencies. Otherwise just run:
```console
$ sudo apt-get build-dep emacs24
```
Once ready:

- clone Emacs trunk:
```console
$ git clone git://git.savannah.gnu.org/emacs.git
```
- build Emacs trunk:
```console
$ cd emacs
$ ./autogen.sh
$ ./autogen.sh git
$ ./configure --host=x86_64-debian-linux-gnu
$ make
$ sudo make install
$ make clean
```
- clone this repo to your home directory:
```console
$ cd
$ git clone git@github.com:manuel-uberti/.emacs.d.git
```
- install the dictionaries for the spell-checker:
```console
$ cd .emacs.d
$ sudo mkdir -p /usr/share/hunspell
$ sudo cp etc/dictionaries/* /usr/share/hunspell
```
- run Emacs

The first time you run Emacs the packages I use will be automatically installed
and set up thanks to [use-package](https://github.com/jwiegley/use-package).

## Updates
This configuration tracks latest Emacs developments, so I highly recommend you
update and build your sources once a week.

You can use [Magit](https://github.com/magit/magit); `magit-status` is bound
to <kbd>C-c v v</kbd>. Or you can do it with the command line:
```console
$ cd emacs
$ git pull
```
Now you can build Emacs:
```console
$ ./configure --host=x86_64-debian-linux-gnu
$ make
$ sudo make install
$ make clean
```
I would also recommend you regularly upgrade every package installed. You can
easily do it with [Paradox](https://github.com/Bruce-Connor/paradox), which is
bound to <kbd>C-c a p</kbd>.

If sources do not build correctly, or you find errors while using the latest
commit, you can still revert to a working commit and re-build:
```console
$ git reset --hard <commit>
$ ./configure --host=x86_64-debian-linux-gnu
$ make
$ sudo make install
$ make clean
```
With the help of tools such as [Magit](https://github.com/magit/magit),
[Paradox](https://github.com/Bruce-Connor/paradox) and your preferred shell,
maintenance is simple.
