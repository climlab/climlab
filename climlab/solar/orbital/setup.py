from __future__ import division, print_function
import os


def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration(package_name='orbital', parent_name=parent_package, top_path=top_path)
    config.add_data_files(os.path.join('data','README'))
    return config

if __name__ == '__main__':
    print('This is the wrong setup.py file to run')
