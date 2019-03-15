from __future__ import division, print_function


def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration(package_name='tests', parent_name=parent_package, top_path=top_path)
    #config.make_config_py() # installs __config__.py
    return config

if __name__ == '__main__':
    print('This is the wrong setup.py file to run')
