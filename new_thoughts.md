The design of `climlab` top-level classes needs to be first and foremost *modular* and *agnostic* about specific implementation of the computational details. Otherwise the API is going to keep changing with each modification.

The guiding principle:

**Keep the user interface / API as cleanly separated from the numerics as possible!**

I don't think it really makes sense to sub-class `xray.Dataset` for the top-level Process.

The advantage of doing that was
- automatic axis like `process.lat`
- arithmetic on processes
- simple i/o to `netcdf`

But really... doing `process.lat` is trivial to implement as an object property

What we really want to do is have an object attribute
`process.state`
that itself is a dictionary of state variables

We will use `xray.Dataset` to represent this dictionary
But as much as possible, design the rest of the API to be agnostic about the details!

- arithmetic on processes is not useful. Only arithmetic on dictionaries of state or other variables.

- would be very simple to have a method to produces a dictionary or dataset object containing all variables (state plus diagnostics) that would make for easy i/o using `xray` code.

We also definitely want to be able to do things like `process.Ts` to access specific variables (whether state or otherwise)
This will require some extra code to access members of the state and diagnostic dictionaries as attributes of the process object. Same with grid variables like process.lat and process.lev

So... essentials for the API design are
- accessing dictionary of state variables through `process.state`
- same for diagnostic variables through `process.diagnostic`
- same for input and boundary conditions through `process.input`
- same for parameters (essentially just scalar input) through `process.param`
- `process.tendencies()` (formerly known as `process.compute()`) is a method that computes complete tendencies (d/dt) for all state variables, including the process itself as well as all subprocesses. It will return a dictionary or `xray.Dataset` object with same members as `process.state`
- `process.step_forward()` actually updates the state variables with the current tendencies.
- `process.integrate_years()` etc will automate the time-stepping and computation of time-average diagnostics.
- `process.subprocess` is a dictionary of named subprocesses. Each member is another instance of `climlab.Process`
- All variables (including state variables) are referenced to a grid. This will be handled by implementing each variable as an `xray.DataArray` object. But the `climlab` API should be as agnostic about this as possible.

Also try to be a little more deliberate and consistent about public vs. private methods. Use `process._method()` for anything that is not a well-thought-out generic method that should be part of the public API.

*As much as possible* avoid `set_stuff()` type methods. Changing a parameter or setting or state variable should be intuitive and easy from the command line with `process.attribute = value` logic. Wherever necessary use `property` declarations to enable this.

I need to think through the time-stepping logic more carefully though. Need a robust system that, at the top-most level, is agnostic about the numerical scheme. But also flexible about order of operations. I think each `process` object will need a well-defined map of sequence of calculations to get the tendencies of all its subprocesses. 
