#!/usr/bin/env python
from __future__ import division, print_function
import os


def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration('radiation', parent_package, top_path)
    config.add_subpackage('rrtm')
    config.add_subpackage('cam3')
    config.add_data_files(os.path.join('data','ozone','README'))
    return config

if __name__ == '__main__':
    print('This is the wrong setup.py file to run')
