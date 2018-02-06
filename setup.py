import os, sys
import textwrap

VERSION = '0.6.2'

# BEFORE importing setuptools, remove MANIFEST. Otherwise it may not be
# properly updated when the contents of directories change (true for distutils,
# not sure about setuptools).
if os.path.exists('MANIFEST'):
    os.remove('MANIFEST')

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

def setup_package():
    __version__ = VERSION
    metadata = dict(
          name='climlab',
          version=__version__,
          description='Package for process-oriented climate modeling',
          long_description=readme(),
          classifiers=[
            'License :: OSI Approved :: MIT License',
            'Programming Language :: Python',
            'Intended Audience :: Education',
            'Intended Audience :: Science/Research',
            'Topic :: Scientific/Engineering :: Atmospheric Science',
          ],
          keywords='climate modeling modelling model ebm radiation radiative-convective earth',
          url='http://github.com/brian-rose/climlab',
          author='Brian E. J. Rose',
          author_email='brose@albany.edu',
          license='MIT',
    )
    # if "--force" in sys.argv:
    #     run_build = True
    # else:
    #     # Raise errors for unsupported commands, improve help output, etc.
    #     run_build = parse_setuppy_commands()
    run_build = True

    # This import is here because it needs to be done before importing setup()
    # from numpy.distutils, but after the MANIFEST removing and sdist import
    # higher up in this file.
    from setuptools import setup

    if run_build:
        from numpy.distutils.core import setup
        metadata['configuration'] = configuration
    setup(**metadata)

if __name__ == '__main__':
    setup_package()
