# WebParser #

This repository can be added in the src/ directory of any of the TRIPS systems
available on github (at time of writing, these are [bob](../bob),
[cabot](../cabot), [cogent](../cogent), [drum](../drum), and [step](../step))
in order to replicate the websites for those systems' parsers ([indexed
here](http://trips.ihmc.us/parser/). This is a manually-updated mirror of the
`src/WebParser/` directory in the TRIPS CVS repository.

The main documentation for the CVS version is in [README.txt](README.txt). The
quick start guide there talks about getting stuff from CVS, which you probably
don't have access to. But you can get the code in the right places with the
following commands (using the TRIPS cabot system as an example):

    git clone https://github.com/wdebeaum/cabot.git
    cd cabot/src/
    git clone https://github.com/wdebeaum/WebParser.git

All of the TRIPS systems that have git mirrors already have the
`$TRIPS_BASE/src/config/Graphviz/` directory, and the code to load `WebParser`
from `system.lisp`, so you don't need to worry about those parts. And there are
other ways to do this, using symlinks, or git submodules, but I think this is
the easiest.

After this you can follow the instructions for configuring and installing the
TRIPS system you selected, and resume the instructions from README.txt at "And
install files to the web directory." If you already installed TRIPS before
adding WebParser, you should instead resume at "Then you must remake the lisp
image..."

## Licensing ##

The files in the `exslt/` subdirectory are from the [EXSLT website](http://exslt.org), which is [on github](https://github.com/exslt/exslt.github.io) with an MIT license (see [exslt/LICENSE](exslt/LICENSE).

The rest of the code uses the same license as TRIPS, [GPL 2+](http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) (see [LICENSE.txt](LICENSE.txt), and the notice below).

TRIPS WebParser
Copyright (C) 2017  Institute for Human & Machine Cognition

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
