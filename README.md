#.emacs.d

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [.emacs.d](#emacsd)
    - [Preface](#preface)
    - [Setup](#setup)
    - [Updates](#updates)
    - [Acknowledgements](#acknowledgements)
    - [License](#license)

<!-- markdown-toc end -->

##Preface
This is the Emacs configuration I use everyday.

It requires Emacs built from development sources to work. I regularly update my
sources from here:

```console
git://git.savannah.gnu.org/emacs.git
```

I use Emacs on [LinuxBBQ](http://linuxbbq.org/). The ```esetup``` script helps
to create the right environment *before* starting Emacs with this configuration
for the first time. The script only works with **Debian-based** systems.

This configuration uses
[use-package](https://github.com/jwiegley/use-package) and relies heavily upon
[Helm](https://github.com/emacs-helm/helm). I mainly work with **LaTeX**,
**Clojure** and **Org-mode**.

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

##Updates
This configuration tracks latest Emacs developments. If you intend to use it, I
highly recommend you update and build your sources at least once a week.

You can use [Magit](https://github.com/magit/magit); ```magit-status``` is bound
to ```C-c v v```. Or you can do it with the command line:
```console
$ cd emacs
$ git pull
```
Now you can build Emacs:
```console
$ ./configure
$ make
$ sudo make install
$ make clean
```
I would also recommend you regularly upgrade every package installed. You can
easily do it with [Paradox](https://github.com/Bruce-Connor/paradox), which is
bound to ```C-c a p```.

If sources do not build correctly, or you find errors while using the latest
commit, you can still revert to a working commit and re-build:
```console
$ git reset --hard <commit>
$ ./configure
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
- [Mickey Petersen](https://github.com/mickeynp)
- [Sebastian Wiesner](https://github.com/lunaryorn)
- [Artur Malabarba](https://github.com/Malabarba)
- [Sacha Chua](https://github.com/sachac)
- [John Wiegley](https://github.com/jwiegley)
- [Bozhidar Batsov](https://github.com/bbatsov)
- [Magnar Sveen](https://github.com/magnars)
- [Steve Purcell](https://github.com/purcell)
- [Oleh Krehel](https://github.com/abo-abo)
- [Joe Brock](https://github.com/DebianJoe)
- [Wilfred Hughes](https://github.com/Wilfred)

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
