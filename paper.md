---
title: 'CLIMLAB: a Python toolkit for interactive, process-oriented climate modeling'
tags:
- climate
authors:
- name: Brian E. J. Rose
  orcid: 0000-0002-9961-3821
  affiliation: 1 # (Multiple affiliations must be quoted)
date: 22 March 2018
bibliography: paper.bib
---

# Summary

CLIMLAB is an open-ended software engine for interactive, process-oriented climate modeling for use in education and research. It is motivated by the need for simpler tools and more reproducible workflows with which to "fill in the gaps" between blackboard-level theory and the results of comprehensive climate models. With CLIMLAB you can interactively mix and match physical model components, or combine simpler process models together into a more comprehensive model. CLIMLAB is used in the classroom to put models in the hands of students (undergraduate and graduate), and emphasize a hierarchical, process-oriented approach to understanding the key emergent properties of the climate system. CLIMLAB is equally a tool for climate research, where the same needs exist for more robust, process- based understanding and reproducible computational results [@Held:2005fk], [@Jeevanjee:2017a]. \

CLIMLAB defines a base Python class called `Process`, a generalized model operator that contains, at a minimum:
- a dictionary of state variables, each with a well-defined spatial domain
- a list of required input fields
- methods to compute tendencies (rates of change) of its state variables.
A `Process` object can also contain an arbitrarily complex tree of subprocesses (each also some sub-class of `Process`). Tendencies are then computed by iterating through the subprocess tree and summing up contributions from each member. Using this object-oriented approach, every climate process (radiative, dynamical, physical, turbulent, convective, chemical, etc.) can be simulated as a stand-alone
model given appropriate input, or as a component of a more complex model.

The user-facing parts of CLIMAB are written in Python and well-suited to interactive command line use. CLIMAB is agnostic about the underlying numerics. Several existing process modules use compiled Fortran code.

CLIMLAB has out-of-the-box support and documented examples for

- Radiative and radiative-convective column models, with various radiation schemes:
    - [RRTMG](http://rtweb.aer.com/rrtm_frame.html) (a widely used radiative transfer code)
    - CAM3  (from the NCAR GCM)
    - Grey Gas
    - Simplified band-averaged models (4 bands each in longwave and shortwave)
- Emanuel moist convection scheme
- Hard convective adjustment
- Diffusive energy balance models
- Seasonal and steady-state models
- Arbitrary combinations of the above, for example:
    - 2D latitude-pressure models with radiation, horizontal diffusion, and fixed relative humidity
- Orbital / insolation calculations
- Boundary layer sensible and latent heat fluxes


Development of CLIMLAB is partially supported by the National Science Foundation under award AGS-1455071 to Brian Rose. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

- A summary describing the high-level functionality and purpose of the software
for a diverse, non-specialist audience
- A clear statement of need that illustrates the purpose of the software
- A list of key references including a link to the software archive
- Mentions (if applicable) of any ongoing research projects using the software
or recent scholarly publications enabled by it

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.


# References
