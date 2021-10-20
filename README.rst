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


License
=======

The content of this repository is licensed under the `CC-BY 4.0
<https://creativecommons.org/licenses/by/4.0/>`_ license.
