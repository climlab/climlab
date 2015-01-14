def walk_processes(top, topdown=True):
    """Generator for recursive tree of climlab processes
    Usage:
        processes = []
        for proc in walk_processes(model):
            processes.append(proc)
        
        Generates a complete list of all processes and sub-processes
    
    based on os.walk()    """
    proc = top
    if topdown:
        yield proc
    if len(proc.subprocess) > 0:  # there are sub-processes
        for subproc in proc.subprocess.values():
            for subproc2 in walk_processes(subproc, topdown):
                yield subproc2
    if not topdown:
        yield proc
