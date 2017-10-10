from __future__ import division

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



    :param top:             top process from where walking should start
    :type top:              :class:`~climlab.process.process.Process`
    :param str topname:     name of top process [default: 'top']
    :param bool topdown:    whether geneterate *process_types* in regular or
                            in reverse order [default: True]
    :param bool ignoreFlag: whether ``topdown`` flag should be ignored or not
                            [default: False]
    :returns: name (str), proc (process), level (int)


    :Example:

        ::

            >>> import climlab
            >>> from climlab.utils import walk

            >>> model = climlab.EBM()

            >>> for name, proc, top_proc in walk.walk_processes(model):
            ...     print name
            ...
            top
            diffusion
            LW
            iceline
            cold_albedo
            warm_albedo
            albedo
            insolation

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
        for name, subproc in proc.subprocess.items():
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

    :param  top:            top process for which process tree string should be
                            created
    :type top:              :class:`~climlab.process.process.Process`
    :param str name:        name of top process
    :returns:               string representation of the process tree
    :rtype:                 str

    :Example:

        ::

            >>> import climlab
            >>> from climlab.utils import walk

            >>> model = climlab.EBM()
            >>> proc_tree_str = walk.process_tree(model, name='model')

            >>> print proc_tree_str
            model: <class 'climlab.model.ebm.EBM'>
               diffusion: <class 'climlab.dynamics.diffusion.MeridionalDiffusion'>
               LW: <class 'climlab.radiation.AplusBT.AplusBT'>
               albedo: <class 'climlab.surface.albedo.StepFunctionAlbedo'>
                  iceline: <class 'climlab.surface.albedo.Iceline'>
                  cold_albedo: <class 'climlab.surface.albedo.ConstantAlbedo'>
                  warm_albedo: <class 'climlab.surface.albedo.P2Albedo'>
               insolation: <class 'climlab.radiation.insolation.P2Insolation'>

    """
    str1 = ''
    for name, proc, level in walk_processes(top, name, ignoreFlag=True):
        indent = ' ' * 3 * (level)
        str1 += ('{}{}: {}\n'.format(indent, name, type(proc)))
    return str1
