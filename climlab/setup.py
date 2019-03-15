#!/usr/bin/env python
from __future__ import division, print_function
import os


def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration('climlab', parent_package, top_path)
    config.add_subpackage('convection')
    config.add_subpackage('domain')
    config.add_subpackage('dynamics')
    config.add_subpackage('model')
    config.add_subpackage('process')
    config.add_subpackage('radiation')
    config.add_subpackage('solar')
    config.add_subpackage('surface')
    config.add_subpackage('tests')
    config.add_subpackage('utils')
    #config.make_config_py() # installs __config__.py
    return config

if __name__ == '__main__':
    print('This is the wrong setup.py file to run')
