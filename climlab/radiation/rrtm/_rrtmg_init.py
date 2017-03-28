#   NOT CURRENTLY USING THIS PYTHON-BASED INITIALIZATION!!!
import os
import netCDF4 as nc
from climlab import constants as const


def init_lw(fort, cp=const.cp):
    '''Initialization of the RRTMG_LW fortran module.'''
    read_lw_abs_data(fort)
    fort.climlab_rrtmg_lw_ini(cp)

def init_sw(fort, cp=const.cp):
    '''Initialization of the RRTMG_SW fortran module.'''
    read_sw_abs_data(fort)
    fort.climlab_rrtmg_sw_ini(cp)

def name(bandNumber):
    '''Just creates a string `rrlw_kgxx` where `xx` is a zero-padded version
    of the input number, e.g. `rrlw_kg01` '''
    index = '{:02}'.format(bandNumber)
    thisstr = 'rrlw_kg'
    name = thisstr + index
    return name

def nameSW(bandNumber):
    '''Just creates a string `rrsw_kgxx` where `xx` is
    the input number plus 15, e.g. `rrsw_kg16` for `bandNumber=1`'''
    index = '{:02}'.format(bandNumber+15)
    thisstr = 'rrsw_kg'
    name = thisstr + index
    return name


def read_lw_abs_data(fort):
    '''
    Python code to read RRTM Lw absorption data from netcdf file
    and store it in appropriate fortran modules
    This code replicates the functionality in
    `CliMT/src/radiation/rrtm/src/rrtmg_lw/gcm_model/src/rrtmg_lw_read_nc.f90`
    but removes dependency on compiled netcdf libraries.
    input argument: fortran object _rrtm_radiation_fortran
    '''
    here = os.path.dirname( __file__ )
    ncfilepath = os.path.join(here, '_rrtmg_lw', 'rrtmg_lw_v4.85', 'gcm_model', 'data', 'rrtmg_lw.nc')
    data = nc.Dataset(ncfilepath)
    # Following code in `CliMT/src/radiation/rrtm/src/rrtmg_lw/gcm_model/src/rrtmg_lw_read_nc.f90`
    gPointSetNumber = 1
    numGPoints = 16
    # (actually this number is set in each separately from a constant in
    #  corresponding module, e.g. rrlw_kg.rrlw_kg01.no1 etc. But they are all just
    #  integer 16)

    #  Some dimension constants
    #  For some reason the cases got messed up
    ncpar = fort.rrlw_ncpar
    keylower = ncpar.keylower
    keyupper = ncpar.keyupper
    Tdiff = ncpar.tdiff
    ps = ncpar.ps
    plower = ncpar.plower
    pupper = ncpar.pupper
    Tself = ncpar.tself
    Tforeign = ncpar.tforeign
    T = ncpar.t
    Tplanck = ncpar.tplanck
    band = ncpar.band
    GPoint = ncpar.gpoint
    GPointSet = ncpar.gpointset

    #  Some arrays are all the same size: selfrefo and forrefo
    #  We will loop through each band here and set these two field
    #  (unlike in RRTM code)
    for bandNumber in range(1,17):
        mod = getattr(fort, name(bandNumber))
        mod.selfrefo = data.variables['H20SelfAbsorptionCoefficients'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tself].T
        mod.forrefo = data.variables['H20ForeignAbsorptionCoefficients'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tforeign].T

    ####  All other fields we will set 'by hand' to mirror how it is done in
    ####  the RRTM code file `rrtmg_lw_read_nc.f90`

    # have to switch from fortran indexing to python indexing ... 1 becomes 0.
    #  Also need to transpose each array to get the right order of dimensions

    # subroutine `lw_kgb01`
    bandNumber = 1
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, 0].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T
    #	!Get absorber index for N2
    ab = ncpar.getabsorberindex('N2')
    mod.kao_mn2 = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T
    mod.kbo_mn2 = data.variables['AbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T

    # subroutine `lw_kgb02`
    bandNumber = 2
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, 0].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T

    # subroutine `lw_kgb03`
    bandNumber = 3
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, :keyupper, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, :keyupper].T
    #	!Get absorber index for N2
    #  (comment in the RRTM code is wrong, it's actually N2O)
    ab = ncpar.getabsorberindex('N2O')
    mod.kao_mn2o = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, :keylower].T
    mod.kbo_mn2o = data.variables['AbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, :keyupper].T

    # subroutine `lw_kgb04`
    bandNumber = 4
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, :keyupper, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, :keyupper].T

    # subroutine `lw_kgb05`
    bandNumber = 5
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, :keyupper, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, :keyupper].T
    #	!Get absorber index for O3
    ab = ncpar.getabsorberindex('O3')
    mod.kao_mo3 = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, :keylower].T
    #	!Get absorber index for CCL4
    ab = ncpar.getabsorberindex('CCL4')
    mod.ccl4o = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, 0, 0].T

    # subroutine `lw_kgb06`
    bandNumber = 6
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, 0].T
    #	!Get absorber index for CO2
    ab = ncpar.getabsorberindex('CO2')
    mod.kao_mco2 = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T
    #	!Get absorber index for CFC11
    ab = ncpar.getabsorberindex('CFC11')
    mod.cfc11adjo = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, 0, 0].T
    #	!Get absorber index for CFC12
    ab = ncpar.getabsorberindex('CFC12')
    mod.cfc12o = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, 0, 0].T

    # subroutine `lw_kgb07`
    bandNumber = 7
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T
    #	!Get absorber index for CO2
    ab = ncpar.getabsorberindex('CO2')
    mod.kao_mco2 = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, :keylower].T
    mod.kbo_mco2 = data.variables['AbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T

    # subroutine `lw_kgb08`
    bandNumber = 8
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, 0].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T
    #	!Get absorber index for O3
    ab = ncpar.getabsorberindex('O3')
    mod.kao_mo3 = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T
    #	!Get absorber index for CO2
    ab = ncpar.getabsorberindex('CO2')
    mod.kao_mco2 = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T
    mod.kbo_mco2 = data.variables['AbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T
    #	!Get absorber index for N2O
    ab = ncpar.getabsorberindex('N2O')
    mod.kao_mn2o = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T
    mod.kbo_mn2o = data.variables['AbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T
    #	!Get absorber index for CFC12
    ab = ncpar.getabsorberindex('CFC12')
    mod.cfc12o = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, 0, 0].T
    #	!Get absorber index for CFC22
    ab = ncpar.getabsorberindex('CFC22')
    mod.cfc22adjo = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, 0, 0].T

    # subroutine `lw_kgb09`
    bandNumber = 9
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T
    #	!Get absorber index for N2O
    ab = ncpar.getabsorberindex('N2O')
    mod.kao_mn2o = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, :keylower].T
    mod.kbo_mn2o = data.variables['AbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T

    # subroutine `lw_kgb10`
    bandNumber = 10
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, 0].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T

    # subroutine `lw_kgb11`
    bandNumber = 11
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, 0].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T
    #	!Get absorber index for O2
    mod.kao_mo2 = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T
    mod.kbo_mo2 = data.variables['AbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T

    # subroutine `lw_kgb12`
    bandNumber = 12
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T

    # subroutine `lw_kgb13`
    bandNumber = 13
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    #	!Get absorber index for O3
    ab = ncpar.getabsorberindex('O3')
    mod.kbo_mo3 = data.variables['AbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, 0].T
    #	!Get absorber index for CO2
    ab = ncpar.getabsorberindex('CO2')
    mod.kao_mco2 = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, :keylower].T
    #	!Get absorber index for CO
    ab = ncpar.getabsorberindex('CO')
    mod.kao_mco = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, :keylower].T

    # subroutine `lw_kgb14`
    bandNumber = 14
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, 0].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T

    # subroutine `lw_kgb15`
    bandNumber = 15
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    #	!Get absorber index for N2
    ab = ncpar.getabsorberindex('N2')
    mod.kao_mn2 = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, :T, :keylower].T

    #  subroutine `lw_kgb16`
    bandNumber = 16
    mod = getattr(fort, name(bandNumber))
    mod.fracrefao = data.variables['PlanckFractionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.fracrefbo = data.variables['PlanckFractionUpperAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T


def read_sw_abs_data(fort):
    '''
    Python code to read RRTM SW absorption data from netcdf file
    and store it in appropriate fortran modules
    This code replicates the functionality in
    `CliMT/src/radiation/rrtm/src/rrtmg_sw/gcm_model/src/rrtmg_sw_read_nc.f90`
    but removes dependency on compiled netcdf libraries.
    input argument: fortran object _rrtm_radiation_fortran
    '''
    here = os.path.dirname( __file__ )
    ncfilepath = os.path.join(here, '_rrtmg_sw', 'rrtmg_sw_v4.0', 'gcm_model', 'data', 'rrtmg_sw.nc')
    data = nc.Dataset(ncfilepath)
    gPointSetNumber = 1
    numGPoints = 16

    #  Some dimension constants
    #  For some reason the cases got messed up
    ncpar = fort.rrsw_ncpar
    keylower = ncpar.keylower
    keyupper = ncpar.keyupper
    Tdiff = ncpar.tdiff
    ps = ncpar.ps
    plower = ncpar.plower
    pupper = ncpar.pupper
    Tself = ncpar.tself
    Tforeignlower = ncpar.tforeignlower
    Tforeignupper = ncpar.tforeignupper
    T = ncpar.t
    band = ncpar.band
    GPoint = ncpar.gpoint
    GPointSet = ncpar.gpointset

    # subroutine `sw_kgb16`
    bandNumber = 1
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.rayl = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, 0].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T
    mod.selfrefo = data.variables['H2OSelfAbsorptionCoefficients'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tself].T
    mod.forrefo = data.variables['H2OForeignAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tforeignlower].T

    # subroutine `sw_kgb17`
    bandNumber = 2
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionUpperAtmos'][gPointSetNumber-1, bandNumber-1, :keyupper, :numGPoints].T
    mod.rayl = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, 0].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, :keyupper].T
    mod.selfrefo = data.variables['H2OSelfAbsorptionCoefficients'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tself].T
    mod.forrefo[0:3,:] = data.variables['H2OForeignAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tforeignlower].T
    mod.forrefo[3,:] = data.variables['H2OForeignAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, 1].T

    # subroutine `sw_kgb18`
    bandNumber = 3
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.rayl = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, 0].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T
    mod.selfrefo = data.variables['H2OSelfAbsorptionCoefficients'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tself].T
    mod.forrefo = data.variables['H2OForeignAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tforeignlower].T

    # subroutine `sw_kgb19`
    bandNumber = 4
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.rayl = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, 0].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T
    mod.selfrefo = data.variables['H2OSelfAbsorptionCoefficients'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tself].T
    mod.forrefo = data.variables['H2OForeignAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tforeignlower].T

    # subroutine `sw_kgb20`
    bandNumber = 5
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.rayl = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, 0].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, 0].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T
    mod.selfrefo = data.variables['H2OSelfAbsorptionCoefficients'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tself].T
    mod.forrefo[0:3,:] = data.variables['H2OForeignAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tforeignlower].T
    mod.forrefo[3,:] = data.variables['H2OForeignAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, 1].T
	#  !Get absorber index for CH4
    ab = ncpar.getabsorberindex('CH4')
    mod.absch4o = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, 0, 0].T

    # subroutine `sw_kgb21`
    bandNumber = 6
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.rayl = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, 0].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, :keyupper].T
    mod.selfrefo = data.variables['H2OSelfAbsorptionCoefficients'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tself].T
    mod.forrefo[0:3,:] = data.variables['H2OForeignAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tforeignlower].T
    mod.forrefo[3,:] = data.variables['H2OForeignAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, 1].T

    # subroutine `sw_kgb22`
    bandNumber = 7
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.rayl = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, 0].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T
    mod.selfrefo = data.variables['H2OSelfAbsorptionCoefficients'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tself].T
    mod.forrefo = data.variables['H2OForeignAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tforeignlower].T

    # subroutine `sw_kgb23`
    bandNumber = 8
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.raylo = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, 0].T
    mod.selfrefo = data.variables['H2OSelfAbsorptionCoefficients'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tself].T
    mod.forrefo = data.variables['H2OForeignAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tforeignlower].T

    # subroutine `sw_kgb24`
    bandNumber = 9
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.raylao = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :keylower, :numGPoints].T
    mod.raylbo = data.variables['RayleighExtinctionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T
    mod.selfrefo = data.variables['H2OSelfAbsorptionCoefficients'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tself].T
    mod.forrefo = data.variables['H2OForeignAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tforeignlower].T
    #	!Get absorber index for O3
    ab = ncpar.getabsorberindex('O3')
    mod.abso3ao = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, 0, 0].T
    mod.abso3bo = data.variables['AbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, 0, 0].T

    # subroutine `sw_kgb25`
    bandNumber = 10
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.raylo = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, 0].T
    #	!Get absorber index for O3
    ab = ncpar.getabsorberindex('O3')
    mod.abso3ao = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, 0, 0].T
    mod.abso3bo = data.variables['AbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, 0, 0].T

    # subroutine `sw_kgb26`
    bandNumber = 11
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.raylo = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T

    # subroutine `sw_kgb27`
    bandNumber = 12
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.raylo = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, 0].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T

    # subroutine `sw_kgb28`
    bandNumber = 13
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionUpperAtmos'][gPointSetNumber-1, bandNumber-1, :keyupper, :numGPoints].T
    mod.rayl = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, 0].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, :keylower].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, :keyupper].T

    # subroutine `sw_kgb29`
    bandNumber = 14
    mod = getattr(fort, nameSW(bandNumber))
    mod.sfluxrefo = data.variables['SolarSourceFunctionLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, :numGPoints].T
    mod.rayl = data.variables['RayleighExtinctionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, 0, 0].T
    mod.kao = data.variables['KeySpeciesAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :plower, :Tdiff, 0].T
    mod.kbo = data.variables['KeySpeciesAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :pupper, :Tdiff, 0].T
    mod.selfrefo = data.variables['H2OSelfAbsorptionCoefficients'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tself].T
    mod.forrefo[0:3,:] = data.variables['H2OForeignAbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, :Tforeignlower].T
    mod.forrefo[3,:] = data.variables['H2OForeignAbsorptionCoefficientsUpperAtmos'][gPointSetNumber-1, bandNumber-1, :numGPoints, 1].T
    #	!Get absorber index for H2O
    ab = ncpar.getabsorberindex('H2O')
    mod.absh2oo = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, 0, 0].T
    #	!Get absorber index for CO2
    ab = ncpar.getabsorberindex('CO2')
    mod.absco2o = data.variables['AbsorptionCoefficientsLowerAtmos'][gPointSetNumber-1, bandNumber-1, ab-1, :numGPoints, 0, 0].T
