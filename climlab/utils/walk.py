def walk_processes(top, topname='top', topdown=True, ignoreFlag=False):
    """Generator for recursive tree of climlab processes
    
    Starts walking from climlab process ``top`` and generates a complete 
    list of all processes and sub-processes that are managed from ``top`` process.
    ``level`` indicades the rank of specific process in the process hierarchy:
    
    .. note::    
    
        * level 0: ``top`` process
            * level 1: sub-processes of ``top`` process
                * level 2: sub-sub-processes of ``top`` process (=subprocesses of level 1 processes)
              
              

    The method is based on os.walk().

    
    
    :param process top:     top process from where walking should start
    :param str topname:     name of top process
    :param bool topdown:    whether geneterate *process_types* in regular or 
                            in reverse order.
                            Set to ``True`` by default.    
    :param bool ignoreFlag: whether ``topdown`` flag should be ignored or not
    :returns: name (str), proc (process), level (int)

    
    :Example:
    
        .. code::
            
            import climlab
            
            model = climlab.EBM()
            processes = []
            for name, proc, top_proc in walk_processes(model):
                processes.append(proc)
    
    """
    if not ignoreFlag:
        flag = topdown
    else:
        flag = True
    proc = top
    level = 0
    if flag:
        yield topname, proc, level
    if len(proc.subprocess) > 0:  # there are sub-processes
        level += 1
        for name, subproc in proc.subprocess.iteritems():
            for name2, subproc2, level2 in walk_processes(subproc,
                                                          topname=name,
                                                          topdown=subproc.topdown,
                                                          ignoreFlag=ignoreFlag):
                yield name2, subproc2, level+level2
    if not flag:
        yield topname, proc, level


def process_tree(top, name='top'):
    """Creates a string representation of the process tree for process top.

    This method uses the :func:`walk_processes` method to create the process tree.
    
    :param process top:     top process for which process tree string should be 
                            created
    :param str name:        name of top process
    :returns:               string representation of the process tree
    :rtype:                 str
    
    :Example:
    
        .. code::
            
            import climlab
            from climlab.utils.walk import process_tree            
            
            model = climlab.EBM()
            proc_tree = process_tree(model, name='model')
            
            print proc_tree
                
    """
    str1 = ''
    for name, proc, level in walk_processes(top, name, ignoreFlag=True):
        indent = ' ' * 3 * (level)
        str1 += ('{}{}: {}\n'.format(indent, name, type(proc)))
    return str1
