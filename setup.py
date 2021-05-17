from __future__ import print_function
import os

VERSION = '0.7.12'

# BEFORE importing setuptools, remove MANIFEST. Otherwise it may not be
# properly updated when the contents of directories change (true for distutils,
# not sure about setuptools).
if os.path.exists('MANIFEST'):
    os.remove('MANIFEST')

def readme():
    with open('README.rst') as f:
        return f.read()

# Patch the Fortran compiler not to optimize certain sources
def patch_fortran():
    # This should work for all subclasses of FCompiler
    try:
        from numpy.distutils import fcompiler
    except ImportError:
        import sys
        print("\nPlease install numpy before installing climlab\n", file=sys.stderr)
        sys.exit(-1)

    def monkeypatched_spawn(old_spawn):
        def spawn(self, cmd, *args, **kw):
            for arg in cmd:
                if os.path.basename(arg) in (
                    'rrtmg_sw_k_g.f90',
                    'rrtmg_lw_k_g.f90',
                ):
                    try:
                        cmd.append('-O0')
                    except ValueError:
                        pass
                    break

            return old_spawn(self, cmd, *args, **kw)
        return spawn
    fcompiler.FCompiler.spawn = monkeypatched_spawn(fcompiler.FCompiler.spawn)

def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration

    config = Configuration(None, parent_package, top_path)
    config.set_options(ignore_setup_xxx_py=True,
                       assume_default_configuration=True,
                       delegate_options_to_subpackages=True,
                       quiet=True)
    config.add_subpackage('climlab')
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
          setup_requires=['numpy'],
          install_requires=['numpy','xarray','attrdict','scipy'],
          license='MIT',
    )
    run_build = True

    # This import is here because it needs to be done before importing setup()
    # from numpy.distutils, but after the MANIFEST removing and sdist import
    # higher up in this file.
    from setuptools import setup

    patch_fortran()

    if run_build:
        from numpy.distutils.core import setup
        metadata['configuration'] = configuration
    setup(**metadata)

if __name__ == '__main__':
    setup_package()
