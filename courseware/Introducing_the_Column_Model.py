# ---
# jupyter:
#   jupytext:
#     formats: ipynb,md:myst,py:percent
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.8.0
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

# %% [markdown]
# # Introducing the Column Model

# %% [markdown]
# ## About this exercise:

# %% [markdown]
# There are two goals for this (hopefully straightforward) exercise:
#
# 1. To introduce you to working with files and modules in Python
# 2. To introduce the column radiation model
#
# We will be working with this same model code for the next few weeks.
#
# Your job is to go through the whole exercise. There are 3 questions clearly marked below for you to answer. Hand in ONLY your answers to those questions.

# %% [markdown]
# #### Please DO NOT just copy and paste the examples from here. Type them in yourself! This is MUCH better way to learn what you are doing.

# %% [markdown]
# ### Answers are due on Tuesday February 25, 2014

# %% [markdown]
# ## About Python modules:

# %% [markdown]
# Every time we use the import statement in Python, the interpreter loads instructions from a file, usually with the extension .py at the end of the file name.
#
# For example if we typed 'import foo' the interpreter would try to open and read a file called 'foo.py'
#
# This file would contain the same types of Python code that you have been entering manually on the command line, e.g. definitions of variables and functions.
#
# The real beauty of the import statement is that it makes the Python language very modular. With one 'import' we can have access to all kinds of useful code.
#
# To then access variables and functions in a module, we usually type the name of the module, followed by period (.), followed by the variable or function name. You've already seen a bit of this with the netCDF4 module you used in the last homework. You'll practice these skills again here.

# %%
import numpy as np
import climlab
from climlab import constants as const

# %% [markdown]
# ## Creating a new instance of the Column Model

# %% [markdown]
# To begin working with the model, we just need to tell Python to create a new object called a `mycolumn` that implements a grey radiation model.
#
# Try this:

# %%
mycolumn = climlab.GreyRadiationModel( num_lev=2 )

# %% [markdown]
# You have just created a new column object. What is that? Let's take a look:

# %%
print(mycolumn)

# %% [markdown]
# Try just typing mycolumn. and then hitting the tab key. You should see a list pop up. Everything on that list is the name of some object that is contained within mycolumn.
#
# "Object" is a very general concept. Everything in Python that has a name is an object. This includes variables (numbers, character strings, etc.), but also functions.
#
# Our object called mycolumn contains a bunch of data, and it also contains some functions that operate on those data.
#
# Try this:

# %%
print(mycolumn.state)

# %% [markdown]
# We will use the column object to model temperature in a column. What you just did above was to look at the temperatures currently stored in mycolumn, for the surface (a single number), and the atmosphere.
#
# When you created mycolumn a few lines ago, you specifically asked for a column with 2 levels in the atmosphere.
#
# We can see where those levels are in the atmosphere as follows:

# %%
#  Three ways to get this:
print(mycolumn.lev)
print(mycolumn.Tatm.domain.axes['lev'].points)
print(mycolumn.domains['Tatm'].axes['lev'].points)

# %% [markdown]
# which is actually the pressure (in mb or hPa) at which we are specifying the temperature -- in this case essentially the lower and upper troposphere.

# %% [markdown]
# ### So what does this code actually do?

# %% [markdown]
# It calculates the warming and cooling of the air and the surface based on the grey radition approximation that we have looked at in class.
#
# I encourage you to look through the code. Try typing this:

# %% [raw]
# edit ColumnModel

# %% [markdown]
# You should find that the code is now displayed in your editor window. Have a look, but obviously don't fret about not understanding it at this point.
#
# Notice, though, that there are lots of comments sprinkled throughout the code. Comments in Python begin with # and are just words to help us understand the code. Comments are never executed by the Python interpreter.
#
# Convince yourself of this by typing something like this:

# %%
# this is not valid Python code

# %% [markdown]
# You might want to try typing the same thing without the # in front.

# %% [markdown]
# ### OK but what does this code actually do?

# %% [markdown]
# It calculates the warming and cooling of the air and the surface based on the grey radition approximation that we have looked at in class.
#
# Try this:

# %%
mycolumn.compute_diagnostics()
print(mycolumn.LW_absorbed_sfc)
print(mycolumn.LW_absorbed_atm)

# %% [markdown]
# What you just did was to call a function Longwave_Heating() that calculates how much longwave radiation is emitted up and down from each layer, and how much is absorbed by each layer (and the surface).
#
# You then printed out some quantities that were calculated and stored by the function, which are actually the heating rates at each level in W / m$^2$
#
# Now try this:

# %%
print(mycolumn.SW_absorbed_sfc)
print(mycolumn.SW_absorbed_atm)

# %% [markdown]
# Hopefully this makes sense. Our code also calculates the energy absorbed due to shortwave (solar) radiation. The atmosphere is transparent to solar radiation in this model, so the absorption all occurs at the surface.
#
# Look in the code file to find where the function Shortwave_Heating() is defined. It's really just calculating
# $(1-\alpha) Q$ based on some default parameter values for $\alpha$ and $Q$. We'll think about changing the defaults later.

# %% [markdown]
# ### Energy (im)balance in the column

# %% [markdown]
# To get the total energy sources and sinks at each point, we just need to add up the shortwave and longwave terms:

# %%
print(mycolumn.LW_absorbed_atm + mycolumn.SW_absorbed_atm)
print(mycolumn.LW_absorbed_sfc + mycolumn.SW_absorbed_sfc)

# %% [markdown]
# Evidently this column is NOT in energy balance! The surface is gaining energy at a rate 33 W / m$^2$, the lower atmosphere is losing energy at 116 W / m$^2$, and the upper atmosphere is gaining nearly 90 W / m$^2$.

# %% [markdown]
# ### OK so what?

# %% [markdown]
# The code will not just calculate the energy imbalance, but also change the temperatures in response to the imbalance. It does this by time-stepping, just like we did with the zero-dimensional model in the first homework.

# %%
mycolumn.step_forward()
print(mycolumn.state)

# %% [markdown]
# Here you just called a function `step_forward()` that computes the energy imbalance as you just did above, and then uses that imbalance to adjust the temperatures.

# %% [markdown]
# ## HOMEWORK QUESTION 1

# %% [markdown]
# Have the temperatures gone up or down at the surface and at each level from where they started? Why?
#
# (This is an easy question, not a trick question)

# %% [markdown]
# ## Timestepping to equilibrium

# %% [markdown]
# Just like we did with the zero-dimensional model in the first homework, we will use loops to time-step the model towards equilibrium. Try this:

# %%
for n in range(10):
    mycolumn.step_forward()
    print(mycolumn.Ts)

# %% [markdown]
# This little loop just repeated the call to Step_Forward 10 times, and printed out the surface temperature after each time step.
#
# Notice that the temperature is changing each time. That means we are not at equilibrium. Try it again!

# %%
for n in range(10):
    mycolumn.step_forward()
    print(mycolumn.Ts)

# %% [markdown]
# Still changing, but not by as much.
#
# Here's a trick:

# %%
mycolumn.integrate_years(0.3)
print(mycolumn.Ts)

# %% [markdown]
# What you just did was to loop through the time-stepping procedure 100 times!
#
# Look at the code and find the function Step_Forward(). Do you see how the code creates a loop?
#
# In this case the function Step_Forward() takes an optional input argument which is the number of iterations through the loop. This number defaults to 1 if we don't specify it, which is what happened above!
#
# Has the model reached equilibrium yet? We can always keep on time-stepping and see if anything changes:

# %%
mycolumn.integrate_years(0.3)
print(mycolumn.Ts)

# %% [markdown]
# The changes are now minimal, and it is close to equilibrium.
#
# Let's look at the whole column temperature:

# %%
for key, item in mycolumn.state.items():
    print(key, item)

# %%
mycolumn.diagnostics

# %% [markdown]
# ## HOMEWORK QUESTION 2

# %% [markdown]
# Compare the temperatures you found here (after time-stepping to equilibrium) with the radiative equilibrium temperatures we derived in class for this same model. Do they agree?

# %% [markdown]
# ### Greenhouse warming in the 2-layer model

# %% [markdown]
# Now that our column is in equilibrium, let's look at the Outgoing Longwave Radiation. The model keeps track of this for us:

# %%
print(mycolumn.OLR)

# %% [markdown]
# This should hopefully be almost exactly equal to the shortwave absorption:

# %%
print(mycolumn.ASR)

# %% [markdown]
# Now you are going to do a "global warming" experiment, like we started in class.
#
# The following will increase the emissivity / absorptivity of each layer by 10%, which is analagous to an increase in greenhouse gases in the atmosphere:

# %%
#  Make an exact clone of the model
column2 = climlab.process_like(mycolumn)
absorptivity = column2.subprocess['LW'].absorptivity
print(absorptivity)
column2.subprocess['LW'].absorptivity *= 1.1 
print(column2.subprocess['LW'].absorptivity)

# %% [markdown]
# Let's now re-calculate the longwave radiation with this new value of eps:

# %%
column2.compute_diagnostics()

# %% [markdown]
# ## HOMEWORK QUESTION 3

# %% [markdown]
# Find the new value of OLR after this change. Is it larger or smaller than it was before we added greenhouse gases? What do you think should happen to the surface temperature as a result? Why?

# %%
print(column2.OLR)

# %%
column2.diagnostics

# %%
