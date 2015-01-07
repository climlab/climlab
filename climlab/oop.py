# -*- coding: utf-8 -*-
"""
Created on Wed Jan  7 16:20:45 2015

@author: Brian
"""

# trying out some ideas for a truly object-oriented modeling framework

# create a grid parent class
# create a state variable parent class
# state variables are always referenced to a grid
# create a model parent class.
# a model is nothing more than a collection of state variables,
# with methods to operate on the state variables
#   DO I WANT TO ALLOW THINGS OTHER THAN TIME-DEPENDENT MODELS?

# Actually I'm going to try to do this ON TOP OF IRIS
#  which already implements all of the grid stuff.
#  Basically each state variable will be an iris cube

import numpy as np
import iris
from climlab import legendre

# This creates a latitude axis at 1 degree, evenly spaced.
#   note... you can specify arbitarily complex bounds on each cell.
#  But here we are making them all 1 degree wide.
latpoints = np.arange(-89.5,89.5,1)
lat = iris.coords.DimCoord(latpoints, standard_name='latitude', units='degree_north')
lat.guess_bounds(bound_position=0.5)

#  Now we create a cube with temperature data on that latitude axis
temppoints = 15. - 30.*legendre.P2(np.sin(np.deg2rad(latpoints)))
temp = iris.cube.Cube(data=temppoints, standard_name='air_temperature', 
        units='degree_Celsius', dim_coords_and_dims=[(lat, 0)])

temp2 = iris.cube.Cube(data=temppoints-4., standard_name='air_temperature', 
        units='degree_Celsius', dim_coords_and_dims=[(lat, 0)])

# But this is wonky... get an error trying to do something simple:
temp3 = temp - temp2
# it complains about differing coordinates.

# I STILL FUCKING HATE IRIS. I want to like it.


#  GO BACK TO DOING IT MYSELF.
# make a very simple coordinate class and grid class