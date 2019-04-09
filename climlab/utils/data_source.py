from __future__ import division, print_function
try:
    from urllib.request import urlretrieve  # Python 3
except ImportError:
    from urllib import urlretrieve  # Python 2


def load_data_source(local_path,
               remote_source_list,
               open_method,
               open_method_kwargs=dict(),
               verbose=True):
    try:
        path = local_path
        data = open_method(path, **open_method_kwargs)
        if verbose:
            print('Opened data from {}'.format(path))
    except FileNotFoundError:
        #  First try to load from remote sources and cache the file locally
        for source in remote_source_list:
            try:
                urlretrieve(source, local_path)
                path = local_path
                data = open_method(path, **open_method_kwargs)
                if verbose:
                    print('Data retrieved from {} and saved locally.'.format(source))
                break
            except Exception:
                pass
        else:
            # as a final resort, try opening the source remotely
            for source in remote_source_list:
                path = source
                data = open_method(path, **open_method_kwargs)
                if verbose:
                    print('Opened data remotely from {}'.format(source))
    finally:
        return data, path
