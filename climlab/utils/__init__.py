'''A collection of general purpose utility functions.'''

# Root URL for the climlab data repository
_datapath_http = "http://www.atmos.albany.edu/facstaff/brose/resources/climlab_data/"
# Alternate path that should also work
_threddspath_http = "http://thredds.atmos.albany.edu:8080/thredds/fileServer/CLIMLAB/"


def _make_dict(arg, argtype):
    if arg is None:
        return {}
    elif isinstance(arg, dict):
        return arg
    elif isinstance(arg, argtype):
        return {'default': arg}
    else:
        raise ValueError('Problem with input type')


class ProcNameWarning(UserWarning):
    pass

