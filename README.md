#.emacs.d

This is the Emacs configuration I use everyday.

##Notes
I use Emacs on [Debian Jessie](https://www.debian.org) (amd64), so this
configuration targets **Debian-based** systems. However, it has been tested on a
wide range of GNU/Linux distributions.

The `esetup` script helps to create the required environment. It installs all
the necessary tools automatically, provided the user has `sudo` privileges. The
script must be run *before* starting Emacs for the first time.

This configuration requires an Emacs version built from development sources. It
does not work with older versions.

##Setup
On your **Debian-based** machine:

- clone Emacs trunk:
```console
$ git clone git://git.savannah.gnu.org/emacs.git
```
- build Emacs trunk:
```console
$ cd emacs
$ sudo apt-get build-dep emacs24
$ ./autogen.sh
$ ./autogen.sh git
$ ./configure
$ make
$ sudo make install
$ make clean
```
- clone this repo to your home directory:
```console
$ cd
$ git clone https://github.com/manuel-uberti/.emacs.d
```
- run ```esetup```:
```console
$ cd .emacs.d
$ chmod +x esetup
$ ./esetup
```
- run Emacs

The first time you run Emacs, every package configured in `init.el` and in every
file in the `lisp` directory will be automatically installed and configured.

##Updates
This configuration tracks latest Emacs developments. If you intend to use it, I
highly recommend you update and build your sources at least once a week.

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
maintenance is fairly simple.

##Acknowledgements
This configuration would not have been possible without the work of and the
inspiration from these people:
- [Bozhidar Batsov](https://github.com/bbatsov)
- [Joe Brock](https://github.com/DebianJoe)
- [Sacha Chua](https://github.com/sachac)
- [Wilfred Hughes](https://github.com/Wilfred)
- [Oleh Krehel](https://github.com/abo-abo)
- [Artur Malabarba](https://github.com/Malabarba)
- [Kaushal Modi](https://github.com/kaushalmodi)
- [Mickey Petersen](https://github.com/mickeynp)
- [Steve Purcell](https://github.com/purcell)
- [Vasilij Schneidermann](https://github.com/wasamasa)
- [Magnar Sveen](https://github.com/magnars)
- [John Wiegley](https://github.com/jwiegley)
- [Sebastian Wiesner](https://github.com/lunaryorn)

##License
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with GNU
Emacs; see the file COPYING. If not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
