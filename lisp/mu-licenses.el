;;; mu-licenses.el --- Generate custom licenses for headers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel.uberti@inventati.org>
;; Keywords: convenience

;;; Commentary:

;; This file store useful skeletons for insert licenses.

;;; Code:

(define-skeleton mu-insert-mit/x11
  "Insert the MIT/X11 license into the current buffer."
  nil
  @ "Copyright (c) " (format-time-string "%Y")
    "  " user-full-name " <" user-mail-address ">

"
    @
    "Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
"
    @
    '(pcase-let ((`(,end ,mid ,start . _) skeleton-positions))
       ;; We use markers to keep the positions updated after commenting
       (let ((mid (copy-marker mid t))
             (end (copy-marker end t)))
         (comment-region start end)
         (fill-region mid end)))
    "\n"-)

(define-skeleton mu-insert-apache2
  "Insert Apache 2 license header into the current buffer."
  nil
  @ "Copyright (c) " (format-time-string "%Y")
    "  " user-full-name " <" user-mail-address ">

"
    @
    "Licensed under the Apache License, Version 2.0 (the
\"License\"); you may not use this file except in compliance with
the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an \"AS
IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
express or implied.  See the License for the specific language
governing permissions and limitations under the License.
"
    @
    '(pcase-let ((`(,end ,mid ,start . _) skeleton-positions))
       ;; We use markers to keep the positions updated after commenting
       (let ((mid (copy-marker mid t))
             (end (copy-marker end t)))
         (comment-region start end)
         (fill-region mid end)))
    "\n"-)

(define-skeleton mu-insert-gpl3
  "Insert GPL 3 license header into the current buffer."
  nil
  @ "Copyright (C) " (format-time-string "%Y")
    "  " user-full-name " <" user-mail-address ">

"
    @
    "This program is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see
<http://www.gnu.org/licenses/>.
"
    @
    '(pcase-let ((`(,end ,mid ,start . _) skeleton-positions))
       ;; We use markers to keep the positions updated after commenting
       (let ((mid (copy-marker mid t))
             (end (copy-marker end t)))
         (comment-region start end)
         (fill-region mid end)))
    "\n"-)

(provide 'mu-licenses)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-licenses ends here
