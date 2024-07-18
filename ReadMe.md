Welcome to project *FBeauty*, a [FreeBASIC](http://www.freebasic.net/)
source code beautyfier. It's designed to get started

- from the command line or

- as a Geany IDE custom command

in order to

- parse source code,

- identify the FreeBASIC keywords and

- formats them in consistent letter cases (UPPER CASE, lower case,
  CamelCase as defined in source).

Comments and string literals stay unchanged. The package contains the
source code of the tool, to be compiled with the FreeBASIC compiler.

Find further information at the forum page
http://www.freebasic.net/forum/viewtopic.php?p=146522.


Licence
=======

Copyright &copy; 2011-2024 by DTJF (Thomas{ At ]Freiherr{ at }gmx[ dOt ]net)

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

The programm acts like a filter, picking FreeBASIC keywords from the
input stream (StdIn) and (by default) replacing them by their upper-case
equivalents in the output stream (StdOut). All other words and
characters passing unchanged. The filter neither works in comments
nor in strings.


Installation
============

The program is designed for use in Geany, an FreeBASIC-IDE for LINUX
and win. As a compiled binary it can be added to Geany as a `sent text`
custom command. Therefor choose to menu `Edit -> Format -> Send text ->
Set Custom Commands` and click in that dialog on `Add` to get a new
text entry. Then type the command for executing the FBeauty binary.
From now on, the current selection can get send to FBeauty and the
filtered output of FBeauty will be used to replace the current
selection.


Further Usage
=============

FBeauty can also be used at the command line by reading the input from
a file. Ie. on LINUX type `./FBeauty < Input.txt > Output.bas` to
read the context form file Input.txt and write the filtered output to
file Output.bas.


Customization
=============

In order to customize the filter features, adapt the entries in
`RETURN Cases(w, @"XxXx")` in the source code for personal purposes.

Since version 0.0.2 different keyword cases can be choosen by command
line options:

| Option | Result                                          |
| -----: | :---------------------------------------------- |
|        | upper case keywords (default)                   |
|     -l | lower case keywords                             |
|     -c | capitalized keywords                            |


Special compiling
=================

By default FB dialect keywords are not supported, unless you set the
preprocessors define `DIALECTS`, either by uncommenting the source code
line `'#DEFINE DIALECTS`, or at the compiler command line like

~~~{txt}
fbc -w all -d DIALECTS FBeauty.bas
~~~

The preprocessors define `THISFIX` makes it handle the keywort `THIS`
correctly, when followed by a dot operator:

~~~{txt}
fbc -w all -d THISFIX FBeauty.bas
~~~

\note This slows down the handling of all words starting with a `T`
      character. Since the syntax `THIS.` is not neccessary, this
      feature is implemented as an option.


Further Information
===================

- [Forum (en)](http://www.freebasic.net/forum/viewtopic.php?p=146522)
- [Former download page (de)](http://www.freebasic-portal.de/downloads/ides-fuer-freebasic/fbeauty-bas-160.html)
