#!/bin/bash

#  Based on conda-forge recipe for scipy
export LIBRARY_PATH="${PREFIX}/lib"
export C_INCLUDE_PATH="${PREFIX}/include"
export CPLUS_INCLUDE_PATH="${PREFIX}/include"

# Depending on our platform, shared libraries end with either .so or .dylib
if [[ `uname` == 'Darwin' ]]; then
    # conda compilers need the MacOSX10.9 SDK and need to be told where they are
    # https://www.anaconda.com/utilizing-the-new-compilers-in-anaconda-distribution-5/
    # Other uses may need to update this if they install MacOSX10.9.sdk somewhere else
    export CONDA_BUILD_SYSROOT=/opt/MacOSX10.9.sdk
else
    unset LDFLAGS
fi

$PYTHON -m pip install . --no-deps -vv
