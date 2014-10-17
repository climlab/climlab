from setuptools import setup

def readme():
    with open('README') as f:
        return f.read()

setup(name='climlab',
      version='0.1',
      description='Package for simple climate modeling',
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
      packages=['climlab'],
      install_requires=[
          'numpy',
          'scipy',
      ],
      include_package_data=True,
      zip_safe=False)
