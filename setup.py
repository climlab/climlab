#from setuptools import setup, find_packages
#import os, sys, subprocess, string
#from numpy.distutils import fcompiler
#from setuptools.extension import Extension
#  or use the numpy versions?
#from numpy.distutils.core import setup
#from numpy.distutils.extension import Extension
import os, sys

# path = os.path.join('climlab','radiation','src','cam3')
# sourcelistfile = os.path.join(path,'sources_in_order_of_compilation')
# sourcelist = [line.rstrip() for line in open(sourcelistfile)]
# sourcelist.append('_cam3.pyf')
# ext1 = Extension(name = '_cam3',
#                  sources = sourcelist,
#                  f2py_options = '--fcompiler={} --noopt'.format(fcompiler.get_default_fcompiler()))
#
#
# path = os.path.join('climlab','radiation','src','rrtm','rrtmg_lw_v4.85')
# sourcelistfile = os.path.join(path,'sources_in_order_of_compilation')
# sourcelist = [line.rstrip() for line in open(sourcelistfile)]
# sourcelist.append('_rrtmg_lw.pyf')
# print sourcelist
# ext2 = Extension(name = '_rrtmg_lw',
#                  sources = sourcelist)
#
# #ext2 = Extension(name = 'fib2',
# #                sources = ['fib2.pyf', 'fib1.f'])
#
#

def readme():
    with open('README.rst') as f:
        return f.read()

#import build_fortran_extensions

def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration

    config = Configuration(None, parent_package, top_path)
    config.set_options(ignore_setup_xxx_py=True,
                       assume_default_configuration=True,
                       delegate_options_to_subpackages=True,
                       quiet=False)

    config.add_subpackage('climlab')

    #config.get_version('numpy/version.py') # sets config.version

    return config



if __name__ == '__main__':
    # from setuptools import setup
    # import climlab
    # metadata = dict(
    #     name='climlab',
    #     version=climlab.__version__,
    #     description='Package for process-oriented climate modeling',
    #     long_description=readme(),
    #     classifiers=[
    #         'Development Status :: 3 - Alpha',
    #         'License :: OSI Approved :: MIT License',
    #         'Programming Language :: Python :: 2.7',
    #         'Intended Audience :: Education',
    #         'Intended Audience :: Science/Research',
    #         'Topic :: Scientific/Engineering :: Atmospheric Science',
    #                 ],
    #     keywords='climate modeling modelling model ebm radiation radiative-convective earth',
    #     url='http://github.com/brian-rose/climlab',
    #     author='Brian E. J. Rose',
    #     author_email='brose@albany.edu',
    #     license='MIT',
    #     install_requires=[
    #           'numpy',
    #           'scipy',
    #           'netcdf4',
    #           'pytest'
    #                     ],
    #       #package_data={'climlab': ['data/apeozone_cam3_5_54.nc'],
    #     #                'climlab.solar': ['orbit91'],},
    #       setup_requires=['pytest-runner'],
    #       tests_require=['pytest'],
    #       include_package_data=True,
    #       zip_safe=False,
    #
    #                 )
    # if "--force" in sys.argv:
    #     run_build = True
    # else:
    #     # Raise errors for unsupported commands, improve help output, etc.
    #     #run_build = parse_setuppy_commands()
    #     run_build=True
    #
    # from setuptools import setup
    # if run_build:
    #     from numpy.distutils.core import setup
    #     metadata['configuration'] = configuration
    #
    # setup(**metadata)

    #  Set up climlab with call to setuptools
    from setuptools import setup
    from climlab import __version__
    setup(name='climlab',
          version=__version__,
          description='Package for process-oriented climate modeling',
          long_description=readme(),
          classifiers=[
            'Development Status :: 3 - Alpha',
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
          #packages=find_packages(),
          #packages = ['climlab'],
          packages=[
            'climlab',
            'climlab.convection',
            'climlab.domain',
            'climlab.dynamics',
            'climlab.model',
            'climlab.process',
            'climlab.radiation',
            'climlab.radiation.cam3',
            'climlab.radiation.rrtm',
            'climlab.radiation.rrtm._rrtmg_sw',
            'climlab.radiation.rrtm._rrtmg_lw',
            'climlab.solar',
            'climlab.surface',
            'climlab.tests',
            'climlab.utils',
          ],
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
            'climlab.radiation.rrtm._rrtmg_sw': ['_rrtmg_sw.so'],
            'climlab.radiation.rrtm._rrtmg_lw': ['_rrtmg_lw.so'],
            # 'climlab.radiation': [
            #     'src/cam3/sources_in_order_of_compilation',
            #     'src/cam3/sources_signature_file',
            #     'src/cam3/Driver.f90',
            #     'src/cam3/.f2py_f2cmap',
            #     'src/cam3/src/*.F90',
            #     'src/cam3/src/*.h']
            },
          setup_requires=['pytest-runner'],
          tests_require=['pytest'],
          include_package_data=True,
          zip_safe=False,
          #ext_modules = [ext2],
          )
