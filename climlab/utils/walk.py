def walk_processes(top, topdown=True):
    """Generator for recursive tree of climlab processes
    Usage:
        processes = []
        for name, proc in walk_processes(model):
            processes.append(proc)
        
        Generates a complete list of all processes and sub-processes
    
    based on os.walk()    """
    proc = top
    if 'name' not in locals():
        name = 'top'
    if topdown:
        yield name, proc
    if len(proc.subprocess) > 0:  # there are sub-processes
        for name, subproc in proc.subprocess.iteritems():
            for name2, subproc2 in walk_processes(subproc, topdown=subproc.topdown):
                yield name2, subproc2
    if not topdown:
        yield name, proc
