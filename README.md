MUSIC V
=================

This is Max Mathew's MUSIC V synthesis program, prepared by
Bill Schottstaed for gfortran, with additions and fixes by Victor Lazzarini.

Building
------

To build MUSIC V you will need to install the gfortran compiler,
which you can find at http://gcc.gnu.org/wiki/GFortran

With this installed,

```
$ cd src
$ make
```

and your version MUSIC V will be built. 

Running
--------

MUSIC V is based on a 3-pass set of commands (which take no arguments):

1. pass1  takes a score file named 'score' and produce 'pass1.data'
2. pass2  takes a 'pass1.data' file and produces 'pass2.data'
3. pass3  takes a 'pass2.data' file and produces 'snd.raw'

'snd.raw' is a mono, 44.1KHz, 32-bit float with your system's endianess.

A script is included for convenience, to which you can pass any
score and output file names:

$ ./music5.sh myscore output.raw


Known Issues
=============

The FLT unit generator is missing. 

