MUSIC V
=================

This is Max Mathew's MUSIC V synthesis program, prepared by
Bill Schottstaed for gfortran, with further 
additions and fixes by Victor Lazzarini.

Building
------

To build MUSIC V you will need to install a Fortran compiller. The
gfortran compiler, which you can find at
http://gcc.gnu.org/wiki/GFortran, has been tested and the sources
are kept up to date with it. You willl also need CMake and a C
compiler installation.

With these installed,

```
$ mkdir build
$ cmake ..
$ make
```

will build MUSIC V

Installing
--------

Installation is controlled by the CMake option
`CMAKE_INSTALL_PREFIX`. This is set by default to `/usr/local`.

The command

```
make install
```

will install the software to `${CMAKE_INSTALL_PREFIX}/bin`. Depending
on the local admin permissions might be need to perform installation.


Running
--------

MUSIC V is based on a 3-pass set of commands (which take no arguments):

1. pass1  takes a score file named 'score' and produce 'pass1.data'
2. pass2  takes a 'pass1.data' file and produces 'pass2.data'
3. pass3  takes a 'pass2.data' file and produces 'snd.raw'

'snd.raw' is a mono, 44.1KHz, 32-bit float with your system's
endianess. A utility, `raw2wav` is provided to convert this to a
RIFF-Wave format file.

The `music5` command is also included. This is a driver program that
calls the three passes and produces a RIFF Wave format
soundfile. 

```
music5 <scorefile> <soundfile>
```

Since this program calls the other commands, it is
important that these are all found in your system `PATH`. Normally,
this is guaranteed by installation, but it is also possible to prepend this
environment variable to the command if that is not the case. With
all programs in the current directory we can di.

```
PATH=.:$PATH music5 <scorefile> <soundfile>
```


Known Issues
------

- The FLT unit generator is missing. 
- PLF routines cannot always be directly added to pass1.f because the read1
  routine has been disposed of. They may need to be integrated in the main
  program.


