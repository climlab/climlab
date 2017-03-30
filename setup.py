import os, sys

VERSION = '0.5.2'

def readme():
    with open('README.rst') as f:
        return f.read()

def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration

    config = Configuration(None, parent_package, top_path)
    config.set_options(ignore_setup_xxx_py=True,
                       assume_default_configuration=True,
                       delegate_options_to_subpackages=True,
                       quiet=True)

    config.add_subpackage('climlab')

    #config.get_version('numpy/version.py') # sets config.version
    return config


if __name__ == '__main__':
    #  Set up climlab with call to setuptools
    #from setuptools import setup
    from numpy.distutils.core import setup
    #from climlab import __version__
    __version__ = VERSION
    setup(name='climlab',
          version=__version__,
          description='Package for process-oriented climate modeling',
          long_description=readme(),
          classifiers=[
            'License :: OSI Approved :: MIT License',
            'Programming Language :: Python :: 2.7',
            'Intended Audience :: Education',
            'Intended Audience :: Science/Research',
            'Topic :: Scientific/Engineering :: Atmospheric Science',
          ],
          keywords='climate modeling modelling model ebm radiation radiative-convective earth',
          url='http://github.com/brian-rose/climlab',
          author='Brian E. J. Rose',
          author_email='brose@albany.edu',
          license='MIT',
          configuration=configuration,
          )
