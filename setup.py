from setuptools import setup, find_packages

def readme():
    with open('README.rst') as f:
        return f.read()

import climlab

setup(name='climlab',
      version=climlab.__version__,
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
      packages=[
        'climlab',
        'climlab.convection',
        'climlab.domain',
        'climlab.dynamics',
        'climlab.model',
        'climlab.process',
        'climlab.radiation',
        'climlab.solar',
        'climlab.surface',
        'climlab.utils',
      ],
      install_requires=[
          'numpy',
          'scipy',
          'netCDF4',
      ],
      package_data={
        'climlab': ['data/cam3rad/abs_ems_factors_fastvx.c030508.nc'],
        'climlab.solar': ['orbit91'],
        'climlab.radiation': [
            'src/cam3/sources_in_order_of_compilation',
            'src/cam3/sources_signature_file',
            'src/cam3/Driver.f90',
            'src/cam3/.f2py_f2cmap',
            'src/cam3/src/*.F90',
            'src/cam3/src/*.h'
        ]},
      include_package_data=True,
      zip_safe=False)
