def walk_processes(top, topname='top', topdown=True, ignoreFlag=False):
    """Generator for recursive tree of climlab processes
    Usage:
        processes = []
        for name, proc, top_proc in walk_processes(model):
            processes.append(proc)
        
        Generates a complete list of all processes and sub-processes
    
    based on os.walk()    """
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
    '''Create a string representation of the process tree for process top.'''
    str1 = ''
    for name, proc, level in walk_processes(top, name, ignoreFlag=True):
        indent = ' ' * 3 * (level)
        str1 += ('{}{}: {}\n'.format(indent, name, type(proc)))
    return str1
