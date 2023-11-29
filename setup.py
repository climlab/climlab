import setuptools, os

VERSION = '0.8.2'

# BEFORE importing setuptools, remove MANIFEST. Otherwise it may not be
# properly updated when the contents of directories change (true for distutils,
# not sure about setuptools).
if os.path.exists('MANIFEST'):
    os.remove('MANIFEST')

def readme():
    with open('README.rst') as f:
        return f.read()

setuptools.setup(
    name='climlab',
    version=VERSION,
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
    url='http://github.com/climlab/climlab',
    author='Brian E. J. Rose',
    author_email='brose@albany.edu',
    setup_requires=['numpy'],
    install_requires=['numpy','xarray','scipy'],
    license='MIT',
    packages=setuptools.find_packages(),
)
