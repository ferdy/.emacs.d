# .emacs.d

This is the Emacs configuration I use everyday.

It requires an Emacs version built from development sources, and it is finely
honed to work with:

- [Clojure and ClojureScript](https://github.com/clojure-emacs/cider)
- [Git](https://magit.vc/)
- [Haskell](https://github.com/commercialhaskell/intero)
- [LaTeX](https://www.gnu.org/software/auctex/)
- [Org mode](https://orgmode.org/)

There is a reasonable setup for other programming languages and utilities, just
have a look inside the `lisp` directory if you want to know more.

This configuration relies on external tools to complement Emacs
capabilities. I use [Ansible
Playbooks](https://github.com/manuel-uberti/playbooks) to set up my systems, but
your mileage may vary.

## First steps

This is how I build and install Emacs on Ubuntu 18.04. If you are using a
different operating system, you need to understand how to install the required
dependencies. Otherwise just run:

```console
$ sudo apt-get build-dep emacs25
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
$ ./configure --host=x86_64-debian-linux-gnu --with-modules
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

Upon starting [use-package](https://github.com/jwiegley/use-package) will fetch,
install, and configure the packages I use.

## Updates
This configuration tracks the latest Emacs developments, so I highly recommend
you update and build your sources frequently.

You can use [Magit](https://github.com/magit/magit); `magit-status` is bound
to <kbd>C-c v v</kbd>.

I would also recommend you regularly upgrade every package installed. You can do
it with [Paradox](https://github.com/Bruce-Connor/paradox), which is bound to
<kbd>C-c a p</kbd>.

If sources do not build correctly, or you find errors while using the latest
commit, you can still revert to a working commit with `git reset --hard
<commit>` and re-build.

With the help of tools such as [Magit](https://github.com/magit/magit),
[Paradox](https://github.com/Bruce-Connor/paradox) and your preferred shell,
maintenance is simple.
