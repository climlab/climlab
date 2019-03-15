from __future__ import division
import climlab


def to_xarray(model):
    model.to_xarray()
    model.to_xarray(diagnostics=True)
    if hasattr(model, 'timeave'):
        climlab.to_xarray(model.timeave)
    # We should be able to render all tendencies as xarray
    for name, proc, top_proc in climlab.utils.walk.walk_processes(model, topdown=False):
        climlab.to_xarray(proc.tendencies)
