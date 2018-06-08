'''
This package contains ready-made models that can be run "off-the-shelf".

    :Example:

        .. code-block:: python

            import climlab
            #  create a 1D Energy Balance Model
            mymodel = climlab.EBM()
            #  see what you just created
            print(mymodel)
            #  run the model
            mymodel.integrate_years(2.)
            #  display the current state
            mymodel.state
            #  see what diagnostics have been computed
            mymodel.diagnostics.keys()

These modules are fully functional and tested.
However users are encouraged to build their own models
by explicitly creating individual processes and coupling together
as subprocesses of a parent process.

See the documentation for the RRTMG scheme for an example of building a
radiative-convective column model from individual components.
'''
