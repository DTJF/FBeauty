Welcome to project *FBeauty*, a [FreeBASIC](http://www.freebasic.net/)
source code beautyfier. It can get started

- from the command line or

- as a Geany IDE custom command

and then

- parses source code,

- identifies the keywords and

- formats them in consistent letter cases (UPPER CASE, lower case,
  CamelCase or custom).

Comments and string literals stay unchanged. The package contains the
source code of the tool, to be compiled with the FreeBASIC compiler.

Find further information at the forum page
http://www.freebasic.net/forum/viewtopic.php?p=146522.


Licence
=======

Copyright &copy; 2011-2015 by DTJF (Thomas{ At ]Freiherr{ at }gmx[ dOt ]net)

The source code of this bundle is free software; you can redistribute
it and/or modify it under the terms of the GNU General Public License
version 3 as published by the Free Software Foundation.

The program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with this package; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110- 1301, USA. For
further details please refer to:
http://www.gnu.org/licenses/gpl-3.0.html


Description
===========

The programm acts like a filter, identyfying FreeBasic keywords in the
input stream (StdIn) and by default replaces them by their upper-case
equivalents in the output stream (StdOut). All other words and
characters get passed unchanged. The filter neither works in comments
nor in strings.


Installation
============

The program is designed for use in Geany, an FreeBasic-IDE for LINUX
and win. As a compiled binary it can be added to Geany as a `sent text`
custom command. Just go to menu `Edit -> Format -> Send text -> Set
Custom Commands` and click in that dialog on `Add` to get a new text
entry. Then type the command for calling the FBeauty binary. From now
on, the current selection can get send to FBeauty and the filtered
output of FBeauty will be used to replace the current selection.


Usage
=====

FBeauty can also be used at the command line by reading the input from
a file. E. g. on LINUX type `./FBeauty < Input.txt > Output.bas` to
read the context form file Input.txt and write the filtered output to
file Output.bas.

The filter features of FBeauty.bas may get customized in the source
code for personal purposes.

Since version 0.0.2 different keyword cases can be choosen by command
line options:

| Option | Result                                          |
| -----: | :---------------------------------------------- |
|        | upper case keywords (default)                   |
|     -l | lower case keywords                             |
|     -c | capitalized keywords                            |
|     -i | individual keywords (as defined in source code) |


Further Information
===================

- [Forum (en)](http://www.freebasic.net/forum/viewtopic.php?p=146522)
- [Former download page (de)](http://www.freebasic-portal.de/downloads/ides-fuer-freebasic/fbeauty-bas-160.html)
