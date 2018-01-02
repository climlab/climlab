.. highlight:: rst

Integration with `xarray`
=========================

xarray_ is a powerful Python package for geospatial data analysis.
It provides ``DataArray`` and ``Dataset`` structures for self-describing gridded data.

For the convenience of xarray users, climlab provides tools for automatic translation
of the native ``Field`` object to xarray format.


:Example:

    Create a single column radiation model and view as ``xarray`` object::

        >>> import climlab
        >>> state = climlab.column_state(num_lev=20)
        >>> model = climlab.radiation.RRTMG(state=state)

        >>> # display model state as xarray:
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
