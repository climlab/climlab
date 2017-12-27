from __future__ import division
from builtins import str
from builtins import object
import xarray as xr


def Field_to_xarray(Field):
    '''Convert a climlab.Field object to xarray.DataArray'''
    dom = Field.domain
    coords = {}; dims = []
    for name in dom.axes:
        dims.append(name)
        coords[name] = dom.axes[name].points
    #  Might need to reorder the data
    da = xr.DataArray(Field.transpose([dom.axis_index[name] for name in dims]),
                      dims=dims, coords=coords)
    for name in dom.axes:
        try:
            da[name].attrs['units'] = dom.axes[name].units
        except:
            pass
    return da

def state_to_xarray(state):
    '''Convert a climlab state or diagnostic variable dictionary to xarray.Dataset'''
    ds = xr.Dataset()
    for name, Field in state.items():
        ds[name] = Field_to_xarray(Field)
        dom = Field.domain
        for axname, ax in dom.axes.items():
            bounds_name = axname + '_bounds'
            ds.coords[bounds_name] = xr.DataArray(ax.bounds, dims=[bounds_name],
                                coords={bounds_name:ax.bounds})
            try:
                ds[bounds_name].attrs['units'] = ax.units
            except:
                pass
    return ds
