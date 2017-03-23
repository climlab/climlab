import os, sys
from numpy.distutils import fcompiler
from numpy.distutils.fcompiler import CompilerNotFound


def readme():
    with open('README.rst') as f:
        return f.read()

#  Run the manual f2py build script
try:
    from numpy.distutils import fcompiler
    compiler = fcompiler.get_default_fcompiler()
    import build_fortran_extensions
except CompilerNotFound:
    print 'Cannot find a Fortran compiler, not building Fortran components!'

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
    from climlab import __version__
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
        #   packages=[
        #     'climlab',
        #     'climlab.convection',
        #     'climlab.domain',
        #     'climlab.dynamics',
        #     'climlab.model',
        #     'climlab.process',
        #     'climlab.radiation',
        #     'climlab.radiation.cam3',
        #     'climlab.radiation.rrtm',
        #     'climlab.radiation.rrtm._rrtmg_sw',
        #     'climlab.radiation.rrtm._rrtmg_lw',
        #     'climlab.solar',
        #     'climlab.surface',
        #     'climlab.tests',
        #     'climlab.utils',
        #   ],
          install_requires=[
              'numpy',
              'scipy',
              'netcdf4',
              'pytest'
          ],
          package_data={
            'climlab': [os.path.join('data','ozone','apeozone_cam3_5_54.nc')],
            'climlab.solar': ['orbit91'],
            'climlab.radiation.cam3': [
                        os.path.join('data','abs_ems_factors_fastvx.c030508.nc'),
                        '_cam3.so',
                                      ],
            #'climlab.radiation.rrtm._rrtmg_sw': ['_rrtmg_sw.so'], # make sure compiled .so files are included
            #'climlab.radiation.rrtm._rrtmg_lw': ['_rrtmg_lw.so'],
            },
          setup_requires=['pytest-runner'],
          tests_require=['pytest'],
          include_package_data=True,
          zip_safe=False,
          configuration=configuration,
          )
