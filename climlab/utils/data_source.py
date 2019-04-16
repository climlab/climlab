from __future__ import division, print_function


def load_data_source(local_path,
               remote_source_list,
               open_method,
               open_method_kwargs=dict(),
               remote_kwargs=dict(),
               verbose=True):
    '''Flexible data retreiver to download and cache the data files locally.

    Usage example (this makes a local copy of the ozone data file):

    :Example:

        .. code-block:: python

            from climlab.utils.data_source import load_data_source
            from xarray import open_dataset
            ozonename = 'apeozone_cam3_5_54.nc'
            ozonepath = 'http://thredds.atmos.albany.edu:8080/thredds/fileServer/CLIMLAB/ozone/' + ozonename
            data, path = load_data_source(local_path=ozonename,
                                          remote_source_list=[ozonepath],
                                          open_method=open_dataset)
            print(data)


    The order of operations is

    1. Try to read the data directly from ``local_path``
    2. If the file doesn't exist then iterate through ``remote_source_list``.
       Try to download and save the file to ``local_path`` using http request
       If that works then open the data from ``local_path``.
    3. As a last resort, try to read the data remotely from URLs in ``remote_source_list``

    In all cases the file is opened and read by the user-supplied ``open_method``
    (e.g. ``xarray.open_dataset``), with additional keyword arguments supplied
    as a dictionary through ``open_method_kwargs``.
    These are passed straight through to ``open_method``.

    Additional keyword arguments in ``remote_kwargs``
    are only passed to ``open_method`` in option 3 above
    (remote access, e.g. through OpenDAP)

    Quiet all output by passing ``verbose=False``.

    Returns:

    - ``data`` is the data object returned by the successful call to ``open_method``
    - ``path`` is the path that resulted in a successful call to ``open_method``.
    '''
    try:
        path = local_path
        data = open_method(path, **open_method_kwargs)
        if verbose:
            print('Opened data from {}'.format(path))
    except FileNotFoundError:
        #  First try to load from remote sources and cache the file locally
        for source in remote_source_list:
            try:
                response = _download_and_cache(source, local_path)
                data = open_method(local_path, **open_method_kwargs)
                if verbose:
                    print('Data retrieved from {} and saved locally.'.format(source))
                break
            except Exception:
                continue
        else:
            # as a final resort, try opening the source remotely
            for source in remote_source_list:
                path = source
                try:
                    data = open_method(path, **open_method_kwargs, **remote_kwargs)
                    if verbose:
                        print('Opened data remotely from {}'.format(source))
                    break
                except Exception:
                    continue
            else:
                raise Exception('All data access methods have failed.')
    finally:
        return data, path

def _download_and_cache(source, local_path):
    import urllib3
    connection_pool = urllib3.PoolManager()
    resp = connection_pool.request('GET', source)
    if resp.status == 200:  # successful http request
        with open(local_path, 'wb') as file:
            file.write(resp.data)
    return resp
