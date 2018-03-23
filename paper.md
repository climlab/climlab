---
title: 'CLIMLAB: a Python toolkit for interactive, process-oriented climate modeling'
tags:
- climate
- climate
authors:
- name: Brian E. J. Rose
  orcid: 0000-0002-9961-3821
  affiliation: 1 # (Multiple affiliations must be quoted)
date: 22 March 2018
bibliography: paper.bib
---

# Summary

CLIMLAB is an open-ended engine for interactive, process-oriented climate modeling for use in education and research. It is motivated by the need for simpler tools and more reproducible workflows with which to "fill in the gaps" between blackboard-level theory and the results of comprehensive climate models. With CLIMLAB you can interactively mix and match physical model components, or combine simpler process models together into a more comprehensive model. CLIMLAB is used in the classroom (undergraduate and graduate) to put models in the hands of students, and emphasize a hierarchical, process-oriented approach to understanding the key emergent properties of the climate system. CLIMLAB is equally a tool for climate research, where the same needs exist for more robust, process- based understanding and reproducible computational results [@Held:2005fk; @Jeevanjee:2017a].

CLIMLAB defines a base Python class called `Process`. This generalized model operator contains, at a minimum:
- a dictionary of state variables, each with a well-defined spatial domain
- a list of required input fields
- methods to compute tendencies (rates of change) of its state variables given inputs and current state.

A `Process` object can also contain an arbitrarily complex tree of subprocesses (each also some sub-class of `Process`). Tendencies are then computed by iterating through the subprocess tree and summing up contributions from each member. Using this object-oriented approach, every climate process (radiative, dynamical, physical, turbulent, convective, chemical, etc.) can be simulated as a stand-alone model given appropriate input, or as a component of a more complex model.

CLIMLAB has out-of-the-box support and documented examples for
- Several atmospheric radiation codes including the widely used [RRTMG model](http://rtweb.aer.com/rrtm_frame.html) [@Mlawer:1997a;@Clough:2005a]
- Convection models including the [Emanuel moist convection scheme](https://emanuel.mit.edu/problem-convective-moistening) [@EmanueL:1991a; @EmanueL:1999a]
- Diffusion solvers for energy balance models
- Insolation for arbitrary orbital parameters [e.g. as used by @Rose:2017a]
- Boundary layer turbulence

CLIMLAB allows the user to assemble arbitrary combinations of the above on 1D, 2D, or 3D spatial domains. It is also an open framework allowing arbitrary pieces of useful climate model code to be wrapped into new `Process` classes.

The user-facing parts of CLIMLAB are written in Python and well-suited to interactive command line use. CLIMLAB is agnostic about the underlying numerics; several process modules use compiled Fortran code. Automated build services are used to provide binaries for most common platforms, vastly simplifying the deployment of scientific software to non-specialist students and researchers.

In addition to the documentation, a large collection of example classroom usage can be found in the Jupyter notebook collection at <https://github.com/brian-rose/ClimateModeling_courseware>.


# Links

- Source repository: <https://github.com/brian-rose/climlab>
- Documentation: <http://climlab.readthedocs.io/en/latest/>


# Acknowledgement

Development of CLIMLAB is partially supported by the National Science Foundation under award AGS-1455071 to Brian Rose. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.


# References
