def walk_processes(top, name='top', topdown=True):
    """Generator for recursive tree of climlab processes
    Usage:
        processes = []
        for name, proc in walk_processes(model):
            processes.append(proc)
        
        Generates a complete list of all processes and sub-processes
    
    based on os.walk()    """
    proc = top
    #if 'name' not in locals():
    #    name = 'top'
    if topdown:
        yield name, proc
    if len(proc.subprocess) > 0:  # there are sub-processes
        for name, subproc in proc.subprocess.iteritems():
            for name2, subproc2 in walk_processes(subproc, name=name, topdown=subproc.topdown):
                yield name2, subproc2
    if not topdown:
        yield name, proc


#  HERE IS SOME EXAMPLE CODE I FOUND ONLINE TO DISPLAY A DIRECTORY TREE
import os

def list_files(startpath):
    for root, dirs, files in os.walk(startpath):
        level = root.replace(startpath, '').count(os.sep)
        indent = ' ' * 4 * (level)
        print('{}{}/'.format(indent, os.path.basename(root)))
        subindent = ' ' * 4 * (level + 1)
        for f in files:
            print('{}{}'.format(subindent, f))
#
#def display_process_tree(top, name='top'):
#    for name, proc in walk_processes(top, name):
#        level = 