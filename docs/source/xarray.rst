.. highlight:: rst

Integration with ``xarray``
===========================

xarray_ is a powerful Python package for geospatial data analysis.
It provides ``DataArray`` and ``Dataset`` structures for self-describing gridded data.

For the convenience of xarray users, climlab provides tools for automatic translation
of the native ``Field`` object to xarray format.

Additionally, as of climlab v0.7.1, the insolation and orbital functions have been updated
with an xarray-compatible interface:

- ``climlab.solar.orbital.OrbitalTable`` returns an ``xarray.Dataset`` object with orbital data.
- ``climlab.solar.insolation.daily_insolation`` accepts input in labeled ``xarray.DataArray`` format and return the same.

:Example 1:

    Create a single column radiation model and view air temperature as ``xarray.DataArray``::

        >>> import climlab
        >>> state = climlab.column_state(num_lev=20)
        >>> model = climlab.radiation.RRTMG(state=state)

        >>> # display a single variable as xarray.DataArray
        >>> model.Tatm.to_xarray()
        <xarray.DataArray (lev: 20)>
        array([ 200.      ,  204.105263,  208.210526,  212.315789,  216.421053,
                220.526316,  224.631579,  228.736842,  232.842105,  236.947368,
                241.052632,  245.157895,  249.263158,  253.368421,  257.473684,
                261.578947,  265.684211,  269.789474,  273.894737,  278.      ])
        Coordinates:
          * lev      (lev) float64 25.0 75.0 125.0 175.0 225.0 275.0 325.0 375.0 ...

:Example 2:

    Display the entire model state dictionary as ``xarray.Dataset``::

        >>> model.to_xarray()
        <xarray.Dataset>
        Dimensions:       (depth: 1, depth_bounds: 2, lev: 20, lev_bounds: 21)
        Coordinates:
          * depth         (depth) float64 0.5
          * depth_bounds  (depth_bounds) float64 0.0 1.0
          * lev           (lev) float64 25.0 75.0 125.0 175.0 225.0 275.0 325.0 ...
          * lev_bounds    (lev_bounds) float64 0.0 50.0 100.0 150.0 200.0 250.0 ...
        Data variables:
            Ts            (depth) float64 288.0
            Tatm          (lev) float64 200.0 204.1 208.2 212.3 216.4 220.5 224.6 ...

:Example 3:

    Combine model state and diagnostics into a single ``xarray.Dataset``::

        >>> # take a single timestep to populate the diagnostic variables
        >>> model.step_forward()

        >>> # Now look at the full output in xarray format
        >>> model.to_xarray(diagnostics=True)
        <xarray.Dataset>
        Dimensions:           (depth: 1, depth_bounds: 2, lev: 20, lev_bounds: 21)
        Coordinates:
          * depth             (depth) float64 0.5
          * depth_bounds      (depth_bounds) float64 0.0 1.0
          * lev               (lev) float64 25.0 75.0 125.0 175.0 225.0 275.0 325.0 ...
          * lev_bounds        (lev_bounds) float64 0.0 50.0 100.0 150.0 200.0 250.0 ...
        Data variables:
            Ts                (depth) float64 288.7
            Tatm              (lev) float64 201.3 204.0 208.0 212.0 216.1 220.2 ...
            ASR               (depth) float64 240.0
            ASRcld            (depth) float64 0.0
            ASRclr            (depth) float64 240.0
            LW_flux_down      (lev_bounds) float64 0.0 12.63 19.47 26.07 32.92 40.1 ...
            LW_flux_down_clr  (lev_bounds) float64 0.0 12.63 19.47 26.07 32.92 40.1 ...
            LW_flux_net       (lev_bounds) float64 240.1 231.2 227.6 224.1 220.5 ...
            LW_flux_net_clr   (lev_bounds) float64 240.1 231.2 227.6 224.1 220.5 ...
            LW_flux_up        (lev_bounds) float64 240.1 243.9 247.1 250.2 253.4 ...
            LW_flux_up_clr    (lev_bounds) float64 240.1 243.9 247.1 250.2 253.4 ...
            LW_sfc            (depth) float64 128.9
            LW_sfc_clr        (depth) float64 128.9
            OLR               (depth) float64 240.1
            OLRcld            (depth) float64 0.0
            OLRclr            (depth) float64 240.1
            SW_flux_down      (lev_bounds) float64 341.3 323.1 318.0 313.5 309.5 ...
            SW_flux_down_clr  (lev_bounds) float64 341.3 323.1 318.0 313.5 309.5 ...
            SW_flux_net       (lev_bounds) float64 240.0 223.3 220.2 217.9 215.9 ...
            SW_flux_net_clr   (lev_bounds) float64 240.0 223.3 220.2 217.9 215.9 ...
            SW_flux_up        (lev_bounds) float64 101.3 99.88 97.77 95.64 93.57 ...
            SW_flux_up_clr    (lev_bounds) float64 101.3 99.88 97.77 95.64 93.57 ...
            SW_sfc            (depth) float64 163.8
            SW_sfc_clr        (depth) float64 163.8
            TdotLW            (lev) float64 -1.502 -0.6148 -0.5813 -0.6173 -0.6426 ...
            TdotLW_clr        (lev) float64 -1.502 -0.6148 -0.5813 -0.6173 -0.6426 ...
            TdotSW            (lev) float64 2.821 0.5123 0.3936 0.3368 0.3174 0.3299 ...
            TdotSW_clr        (lev) float64 2.821 0.5123 0.3936 0.3368 0.3174 0.3299 ...

:Example 4:

    Use the ``climlab.to_xarray()`` method to convert the ``timeave`` dictionary
    to ``xarray.Dataset``::

        >>> # integrate forward one year and automatically store time averages
        >>> model.integrate_years(1)
        Integrating for 365 steps, 365.2422 days, or 1 years.
        Total elapsed time is 0.9993368783782377 years.

        >>> # Now look at model.timeave dictionary in xarray format
        >>> climlab.to_xarray(model.timeave)
        <xarray.Dataset>
        Dimensions:           (depth: 1, depth_bounds: 2, lev: 20, lev_bounds: 21)
        Coordinates:
          * depth             (depth) float64 0.5
          * depth_bounds      (depth_bounds) float64 0.0 1.0
          * lev               (lev) float64 25.0 75.0 125.0 175.0 225.0 275.0 325.0 ...
          * lev_bounds        (lev_bounds) float64 0.0 50.0 100.0 150.0 200.0 250.0 ...
        Data variables:
            Ts                (depth) float64 296.9
            Tatm              (lev) float64 217.1 203.1 200.8 200.4 201.7 204.2 ...
            ASR               (depth) float64 240.1
            ASRcld            (depth) float64 0.0
            ASRclr            (depth) float64 240.1
            LW_flux_down      (lev_bounds) float64 0.0 16.55 20.24 24.12 28.15 32.57 ...
            LW_flux_down_clr  (lev_bounds) float64 0.0 16.55 20.24 24.12 28.15 32.57 ...
            LW_flux_net       (lev_bounds) float64 243.0 226.5 223.4 221.0 218.8 ...
            LW_flux_net_clr   (lev_bounds) float64 243.0 226.5 223.4 221.0 218.8 ...
            LW_flux_up        (lev_bounds) float64 243.0 243.0 243.7 245.1 246.9 ...
            LW_flux_up_clr    (lev_bounds) float64 243.0 243.0 243.7 245.1 246.9 ...
            LW_sfc            (depth) float64 162.5
            LW_sfc_clr        (depth) float64 162.5
            OLR               (depth) float64 243.0
            OLRcld            (depth) float64 0.0
            OLRclr            (depth) float64 243.0
            SW_flux_down      (lev_bounds) float64 341.3 323.1 317.9 313.5 309.5 ...
            SW_flux_down_clr  (lev_bounds) float64 341.3 323.1 317.9 313.5 309.5 ...
            SW_flux_net       (lev_bounds) float64 240.1 223.3 220.3 217.9 216.0 ...
            SW_flux_net_clr   (lev_bounds) float64 240.1 223.3 220.3 217.9 216.0 ...
            SW_flux_up        (lev_bounds) float64 101.2 99.81 97.69 95.56 93.5 ...
            SW_flux_up_clr    (lev_bounds) float64 101.2 99.81 97.69 95.56 93.5 ...
            SW_sfc            (depth) float64 163.7
            SW_sfc_clr        (depth) float64 163.7
            TdotLW            (lev) float64 -2.789 -0.5133 -0.4154 -0.3732 -0.3626 ...
            TdotLW_clr        (lev) float64 -2.789 -0.5133 -0.4154 -0.3732 -0.3626 ...
            TdotSW            (lev) float64 2.836 0.5078 0.3898 0.3332 0.3138 0.3267 ...
            TdotSW_clr        (lev) float64 2.836 0.5078 0.3898 0.3332 0.3138 0.3267 ...


.. _xarray: http://xarray.pydata.org/en/stable/
