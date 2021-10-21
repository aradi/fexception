**********
FException
**********

This repository offers examples for possible exception handling in Fortran.
The folder `<examples>`_ contains ``*_fxy.f90`` files with the suggested
syntax for different scenarios. Each of the files has a corresponding ``*.f90``
file with a working implementation in Fortran 2008. This demonstrates, that
the features proposed here are possible in Fortran already, the proposed
new syntax only serves the purpose to spare a significant amount of boiler
plate code.

Building
========

In order to build the Fortran examples, use the usual CMake workflow::

  mkdir build
  cd build
  cmake ..
  make

The example executables will be in the ``./examples`` folder in the build
directory.


Related projects
================

The `ErrorFx library <https://github.com/aradi/errorfx>`_ (which this
project emerged from) contains a working implementation of the presented
concepts using `Fypp <https://github.com/aradi/fypp>`_ macros. Since it is
preprocessor based, it has a few limitations (e.g. completness of the
exception catching can only be checked at run time, not at compile time), but
it already offers out of the box exception handling without the need for
writing a lot of boiler plate code.

If you are interested in the discussion about the presented concepts
(and their possible inclusion in a future Fortran standard), join us
in the corresponding issue at the `J3 Fortran github site
<https://github.com/j3-fortran/fortran_proposals/issues/236>`_.


License
=======

The content of this repository is licensed under the `CC-BY 4.0
<https://creativecommons.org/licenses/by/4.0/>`_ license.
