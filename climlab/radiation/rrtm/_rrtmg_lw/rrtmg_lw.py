import _rrtmg_lw
import f90wrap.runtime
import logging

class Parrrtm(f90wrap.runtime.FortranModule):
    """
    Module parrrtm
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 lines 2-110
    
    """
    @property
    def mxlay(self):
        """
        Element mxlay ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 31
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__mxlay()
    
    @property
    def mg(self):
        """
        Element mg ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 32
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__mg()
    
    @property
    def nbndlw(self):
        """
        Element nbndlw ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 33
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__nbndlw()
    
    @property
    def maxxsec(self):
        """
        Element maxxsec ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 34
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__maxxsec()
    
    @property
    def mxmol(self):
        """
        Element mxmol ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 35
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__mxmol()
    
    @property
    def maxinpx(self):
        """
        Element maxinpx ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 36
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__maxinpx()
    
    @property
    def nmol(self):
        """
        Element nmol ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 37
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__nmol()
    
    @property
    def ngptlw(self):
        """
        Element ngptlw ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 39
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngptlw()
    
    @property
    def ng1(self):
        """
        Element ng1 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng1()
    
    @property
    def ng2(self):
        """
        Element ng2 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 45
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng2()
    
    @property
    def ng3(self):
        """
        Element ng3 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 46
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng3()
    
    @property
    def ng4(self):
        """
        Element ng4 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 47
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng4()
    
    @property
    def ng5(self):
        """
        Element ng5 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 48
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng5()
    
    @property
    def ng6(self):
        """
        Element ng6 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 49
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng6()
    
    @property
    def ng7(self):
        """
        Element ng7 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 50
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng7()
    
    @property
    def ng8(self):
        """
        Element ng8 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 51
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng8()
    
    @property
    def ng9(self):
        """
        Element ng9 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 52
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng9()
    
    @property
    def ng10(self):
        """
        Element ng10 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 53
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng10()
    
    @property
    def ng11(self):
        """
        Element ng11 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 54
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng11()
    
    @property
    def ng12(self):
        """
        Element ng12 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 55
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng12()
    
    @property
    def ng13(self):
        """
        Element ng13 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 56
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng13()
    
    @property
    def ng14(self):
        """
        Element ng14 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 57
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng14()
    
    @property
    def ng15(self):
        """
        Element ng15 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 58
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng15()
    
    @property
    def ng16(self):
        """
        Element ng16 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 59
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ng16()
    
    @property
    def ngs1(self):
        """
        Element ngs1 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 61
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs1()
    
    @property
    def ngs2(self):
        """
        Element ngs2 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 62
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs2()
    
    @property
    def ngs3(self):
        """
        Element ngs3 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 63
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs3()
    
    @property
    def ngs4(self):
        """
        Element ngs4 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 64
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs4()
    
    @property
    def ngs5(self):
        """
        Element ngs5 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 65
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs5()
    
    @property
    def ngs6(self):
        """
        Element ngs6 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 66
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs6()
    
    @property
    def ngs7(self):
        """
        Element ngs7 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 67
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs7()
    
    @property
    def ngs8(self):
        """
        Element ngs8 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 68
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs8()
    
    @property
    def ngs9(self):
        """
        Element ngs9 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 69
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs9()
    
    @property
    def ngs10(self):
        """
        Element ngs10 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 70
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs10()
    
    @property
    def ngs11(self):
        """
        Element ngs11 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 71
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs11()
    
    @property
    def ngs12(self):
        """
        Element ngs12 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 72
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs12()
    
    @property
    def ngs13(self):
        """
        Element ngs13 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 73
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs13()
    
    @property
    def ngs14(self):
        """
        Element ngs14 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 74
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs14()
    
    @property
    def ngs15(self):
        """
        Element ngs15 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 line 75
        
        """
        return _rrtmg_lw.f90wrap_parrrtm__get__ngs15()
    
    def __str__(self):
        ret = ['<parrrtm>{\n']
        ret.append('    mxlay : ')
        ret.append(repr(self.mxlay))
        ret.append(',\n    mg : ')
        ret.append(repr(self.mg))
        ret.append(',\n    nbndlw : ')
        ret.append(repr(self.nbndlw))
        ret.append(',\n    maxxsec : ')
        ret.append(repr(self.maxxsec))
        ret.append(',\n    mxmol : ')
        ret.append(repr(self.mxmol))
        ret.append(',\n    maxinpx : ')
        ret.append(repr(self.maxinpx))
        ret.append(',\n    nmol : ')
        ret.append(repr(self.nmol))
        ret.append(',\n    ngptlw : ')
        ret.append(repr(self.ngptlw))
        ret.append(',\n    ng1 : ')
        ret.append(repr(self.ng1))
        ret.append(',\n    ng2 : ')
        ret.append(repr(self.ng2))
        ret.append(',\n    ng3 : ')
        ret.append(repr(self.ng3))
        ret.append(',\n    ng4 : ')
        ret.append(repr(self.ng4))
        ret.append(',\n    ng5 : ')
        ret.append(repr(self.ng5))
        ret.append(',\n    ng6 : ')
        ret.append(repr(self.ng6))
        ret.append(',\n    ng7 : ')
        ret.append(repr(self.ng7))
        ret.append(',\n    ng8 : ')
        ret.append(repr(self.ng8))
        ret.append(',\n    ng9 : ')
        ret.append(repr(self.ng9))
        ret.append(',\n    ng10 : ')
        ret.append(repr(self.ng10))
        ret.append(',\n    ng11 : ')
        ret.append(repr(self.ng11))
        ret.append(',\n    ng12 : ')
        ret.append(repr(self.ng12))
        ret.append(',\n    ng13 : ')
        ret.append(repr(self.ng13))
        ret.append(',\n    ng14 : ')
        ret.append(repr(self.ng14))
        ret.append(',\n    ng15 : ')
        ret.append(repr(self.ng15))
        ret.append(',\n    ng16 : ')
        ret.append(repr(self.ng16))
        ret.append(',\n    ngs1 : ')
        ret.append(repr(self.ngs1))
        ret.append(',\n    ngs2 : ')
        ret.append(repr(self.ngs2))
        ret.append(',\n    ngs3 : ')
        ret.append(repr(self.ngs3))
        ret.append(',\n    ngs4 : ')
        ret.append(repr(self.ngs4))
        ret.append(',\n    ngs5 : ')
        ret.append(repr(self.ngs5))
        ret.append(',\n    ngs6 : ')
        ret.append(repr(self.ngs6))
        ret.append(',\n    ngs7 : ')
        ret.append(repr(self.ngs7))
        ret.append(',\n    ngs8 : ')
        ret.append(repr(self.ngs8))
        ret.append(',\n    ngs9 : ')
        ret.append(repr(self.ngs9))
        ret.append(',\n    ngs10 : ')
        ret.append(repr(self.ngs10))
        ret.append(',\n    ngs11 : ')
        ret.append(repr(self.ngs11))
        ret.append(',\n    ngs12 : ')
        ret.append(repr(self.ngs12))
        ret.append(',\n    ngs13 : ')
        ret.append(repr(self.ngs13))
        ret.append(',\n    ngs14 : ')
        ret.append(repr(self.ngs14))
        ret.append(',\n    ngs15 : ')
        ret.append(repr(self.ngs15))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

parrrtm = Parrrtm()

class Rrlw_Ncpar(f90wrap.runtime.FortranModule):
    """
    Module rrlw_ncpar
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 lines 1-64
    
    """
    @staticmethod
    def getabsorberindex(absorbername):
        """
        absorberindex = getabsorberindex(absorbername)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 lines 48-64
        
        Parameters
        ----------
        absorbername : str
        
        Returns
        -------
        absorberindex : int
        
        """
        absorberindex = _rrtmg_lw.f90wrap_getabsorberindex(absorbername=absorbername)
        return absorberindex
    
    @property
    def cpdair(self):
        """
        Element cpdair ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 7
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__cpdair()
    
    @property
    def maxabsorbernamelength(self):
        """
        Element maxabsorbernamelength ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 13
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__maxabsorbernamelength()
    
    @property
    def absorber(self):
        """
        Element absorber ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 13
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__absorber()
    
    @property
    def status(self):
        """
        Element status ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 29
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_ncpar__array__status(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            status = self._arrays[array_handle]
        else:
            status = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_ncpar__array__status)
            self._arrays[array_handle] = status
        return status
    
    @status.setter
    def status(self, status):
        self.status[...] = status
    
    @property
    def i(self):
        """
        Element i ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 30
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__i()
    
    @i.setter
    def i(self, i):
        _rrtmg_lw.f90wrap_rrlw_ncpar__set__i(i)
    
    @property
    def keylower(self):
        """
        Element keylower ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__keylower()
    
    @property
    def keyupper(self):
        """
        Element keyupper ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__keyupper()
    
    @property
    def tdiff(self):
        """
        Element tdiff ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__tdiff()
    
    @property
    def ps(self):
        """
        Element ps ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__ps()
    
    @property
    def plower(self):
        """
        Element plower ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__plower()
    
    @property
    def pupper(self):
        """
        Element pupper ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__pupper()
    
    @property
    def tself(self):
        """
        Element tself ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__tself()
    
    @property
    def tforeign(self):
        """
        Element tforeign ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__tforeign()
    
    @property
    def pforeign(self):
        """
        Element pforeign ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__pforeign()
    
    @property
    def t(self):
        """
        Element t ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__t()
    
    @property
    def tplanck(self):
        """
        Element tplanck ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__tplanck()
    
    @property
    def band(self):
        """
        Element band ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__band()
    
    @property
    def gpoint(self):
        """
        Element gpoint ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__gpoint()
    
    @property
    def gpointset(self):
        """
        Element gpointset ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90 line 44
        
        """
        return _rrtmg_lw.f90wrap_rrlw_ncpar__get__gpointset()
    
    def __str__(self):
        ret = ['<rrlw_ncpar>{\n']
        ret.append('    cpdair : ')
        ret.append(repr(self.cpdair))
        ret.append(',\n    maxabsorbernamelength : ')
        ret.append(repr(self.maxabsorbernamelength))
        ret.append(',\n    absorber : ')
        ret.append(repr(self.absorber))
        ret.append(',\n    status : ')
        ret.append(repr(self.status))
        ret.append(',\n    i : ')
        ret.append(repr(self.i))
        ret.append(',\n    keylower : ')
        ret.append(repr(self.keylower))
        ret.append(',\n    keyupper : ')
        ret.append(repr(self.keyupper))
        ret.append(',\n    tdiff : ')
        ret.append(repr(self.tdiff))
        ret.append(',\n    ps : ')
        ret.append(repr(self.ps))
        ret.append(',\n    plower : ')
        ret.append(repr(self.plower))
        ret.append(',\n    pupper : ')
        ret.append(repr(self.pupper))
        ret.append(',\n    tself : ')
        ret.append(repr(self.tself))
        ret.append(',\n    tforeign : ')
        ret.append(repr(self.tforeign))
        ret.append(',\n    pforeign : ')
        ret.append(repr(self.pforeign))
        ret.append(',\n    t : ')
        ret.append(repr(self.t))
        ret.append(',\n    tplanck : ')
        ret.append(repr(self.tplanck))
        ret.append(',\n    band : ')
        ret.append(repr(self.band))
        ret.append(',\n    gpoint : ')
        ret.append(repr(self.gpoint))
        ret.append(',\n    gpointset : ')
        ret.append(repr(self.gpointset))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_ncpar = Rrlw_Ncpar()

class Mcica_Subcol_Gen_Lw(f90wrap.runtime.FortranModule):
    """
    Module mcica_subcol_gen_lw
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 lines 7-563
    
    """
    @staticmethod
    def mcica_subcol_lw(iplon, ncol, nlay, icld, permuteseed, irng, play, cldfrac, \
        ciwp, clwp, rei, rel, tauc, cldfmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, \
        taucmcl):
        """
        mcica_subcol_lw(iplon, ncol, nlay, icld, permuteseed, irng, play, cldfrac, ciwp, \
            clwp, rei, rel, tauc, cldfmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 lines 50-155
        
        Parameters
        ----------
        iplon : int
        ncol : int
        nlay : int
        icld : int
        permuteseed : int
        irng : int
        play : float array
        cldfrac : float array
        ciwp : float array
        clwp : float array
        rei : float array
        rel : float array
        tauc : float array
        cldfmcl : float array
        ciwpmcl : float array
        clwpmcl : float array
        reicmcl : float array
        relqmcl : float array
        taucmcl : float array
        
        """
        _rrtmg_lw.f90wrap_mcica_subcol_lw(iplon=iplon, ncol=ncol, nlay=nlay, icld=icld, \
            permuteseed=permuteseed, irng=irng, play=play, cldfrac=cldfrac, ciwp=ciwp, \
            clwp=clwp, rei=rei, rel=rel, tauc=tauc, cldfmcl=cldfmcl, ciwpmcl=ciwpmcl, \
            clwpmcl=clwpmcl, reicmcl=reicmcl, relqmcl=relqmcl, taucmcl=taucmcl)
    
    @staticmethod
    def generate_stochastic_clouds(ncol, nlay, nsubcol, icld, irng, pmid, cld, clwp, \
        ciwp, tauc, cld_stoch, clwp_stoch, ciwp_stoch, tauc_stoch, changeseed=None):
        """
        generate_stochastic_clouds(ncol, nlay, nsubcol, icld, irng, pmid, cld, clwp, \
            ciwp, tauc, cld_stoch, clwp_stoch, ciwp_stoch, tauc_stoch[, changeseed])
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 lines 159-523
        
        Parameters
        ----------
        ncol : int
        nlay : int
        nsubcol : int
        icld : int
        irng : int
        pmid : float array
        cld : float array
        clwp : float array
        ciwp : float array
        tauc : float array
        cld_stoch : float array
        clwp_stoch : float array
        ciwp_stoch : float array
        tauc_stoch : float array
        changeseed : int
        
        \
            -------------------------------------------------------------------------------------------------
        \
            ----------------------------------------------------------------------------------------------------------------
         ---------------------
         Contact: Cecile Hannay (hannay@ucar.edu)
         Original code: Based on Raisanen et al., QJRMS, 2004.
         Modifications: Generalized for use with RRTMG and added Mersenne Twister as the \
             default
           random number generator, which can be changed to the optional kissvec random \
               number generator
           with flag 'irng'. Some extra functionality has been commented or removed.
           Michael J. Iacono, AER, Inc., February 2007
         Given a profile of cloud fraction, cloud water and cloud ice, we produce a set \
             of subcolumns.
         Each layer within each subcolumn is homogeneous, with cloud fraction equal to \
             zero or one
         and uniform cloud liquid and cloud ice concentration.
         The ensemble as a whole reproduces the probability function of cloud liquid and \
             ice within each layer
         and obeys an overlap assumption in the vertical.
         Overlap assumption:
          The cloud are consistent with 4 overlap assumptions: random, maximum, \
              maximum-random and exponential.
          The default option is maximum-random (option 3)
          The options are: 1=random overlap, 2=max/random, 3=maximum overlap, \
              4=exponential overlap
          This is set with the variable "overlap"
        mji - Exponential overlap option (overlap=4) has been deactivated in this \
            version
          The exponential overlap uses also a length scale, Zo. (real, parameter :: Zo = \
              2500. )
         Seed:
          If the stochastic cloud generator is called several times during the same \
              timestep,
          one should change the seed between the call to insure that the subcolumns are \
              different.
          This is done by changing the argument 'changeSeed'
          For example, if one wants to create a set of columns for the shortwave and \
              another set for the longwave ,
          use 'changeSeed = 1' for the first call and'changeSeed = 2' for the second call
         PDF assumption:
          We can use arbitrary complicated PDFS.
          In the present version, we produce homogeneuous clouds (the simplest case).
          Future developments include using the PDF scheme of Ben Johnson.
         History file:
          Option to add diagnostics variables in the history file. (using FINCL in the \
              namelist)
          nsubcol = number of subcolumns
          overlap = overlap type (1-3)
          Zo = length scale
          CLOUD_S = mean of the subcolumn cloud fraction ('_S" means Stochastic)
          CLDLIQ_S = mean of the subcolumn cloud water
          CLDICE_S = mean of the subcolumn cloud ice
         Note:
           Here: we force that the cloud condensate to be consistent with the cloud \
               fraction
           i.e we only have cloud condensate when the cell is cloudy.
           In CAM: The cloud condensate and the cloud fraction are obtained from 2 \
               different equations
           and the 2 quantities can be inconsistent (i.e. CAM can produce cloud fraction
           without cloud condensate or the opposite).
        \
            ---------------------------------------------------------------------------------------------------------------
        """
        _rrtmg_lw.f90wrap_generate_stochastic_clouds(ncol=ncol, nlay=nlay, \
            nsubcol=nsubcol, icld=icld, irng=irng, pmid=pmid, cld=cld, clwp=clwp, \
            ciwp=ciwp, tauc=tauc, cld_stoch=cld_stoch, clwp_stoch=clwp_stoch, \
            ciwp_stoch=ciwp_stoch, tauc_stoch=tauc_stoch, changeseed=changeseed)
    
    @staticmethod
    def kissvec(seed1, seed2, seed3, seed4, ran_arr):
        """
        kissvec(seed1, seed2, seed3, seed4, ran_arr)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 lines 530-562
        
        Parameters
        ----------
        seed1 : int array
        seed2 : int array
        seed3 : int array
        seed4 : int array
        ran_arr : float array
        
        \
            --------------------------------------------------------------------------------------------------
         public domain code
         made available from http://www.fortran.com/
         downloaded by pjr on 03/16/04 for NCAR CAM
         converted to vector form, functions inlined by pjr,mvr on 05/10/2004
         The  KISS (Keep It Simple Stupid) random number generator. Combines:
         (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
         (2) A 3-shift shift-register generator, period 2^32-1,
         (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
          Overall period>2^123;
        """
        _rrtmg_lw.f90wrap_kissvec(seed1=seed1, seed2=seed2, seed3=seed3, seed4=seed4, \
            ran_arr=ran_arr)
    
    _dt_array_initialisers = []
    

mcica_subcol_gen_lw = Mcica_Subcol_Gen_Lw()

class Rrtmg_Lw_Init(f90wrap.runtime.FortranModule):
    """
    Module rrtmg_lw_init
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 6-2657
    
    """
    @staticmethod
    def rrtmg_lw_ini(cpdair):
        """
        rrtmg_lw_ini(cpdair)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 28-176
        
        Parameters
        ----------
        cpdair : float
        
        """
        _rrtmg_lw.f90wrap_rrtmg_lw_ini(cpdair=cpdair)
    
    @staticmethod
    def lwdatinit(cpdair):
        """
        lwdatinit(cpdair)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 179-282
        
        Parameters
        ----------
        cpdair : float
        
        """
        _rrtmg_lw.f90wrap_lwdatinit(cpdair=cpdair)
    
    @staticmethod
    def lwcmbdat():
        """
        lwcmbdat()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 285-364
        
        
        """
        _rrtmg_lw.f90wrap_lwcmbdat()
    
    @staticmethod
    def cmbgb1():
        """
        cmbgb1()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 367-476
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb1()
    
    @staticmethod
    def cmbgb2():
        """
        cmbgb2()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 479-559
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb2()
    
    @staticmethod
    def cmbgb3():
        """
        cmbgb3()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 562-689
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb3()
    
    @staticmethod
    def cmbgb4():
        """
        cmbgb4()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 692-788
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb4()
    
    @staticmethod
    def cmbgb5():
        """
        cmbgb5()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 791-914
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb5()
    
    @staticmethod
    def cmbgb6():
        """
        cmbgb6()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 917-1003
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb6()
    
    @staticmethod
    def cmbgb7():
        """
        cmbgb7()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1006-1127
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb7()
    
    @staticmethod
    def cmbgb8():
        """
        cmbgb8()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1130-1246
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb8()
    
    @staticmethod
    def cmbgb9():
        """
        cmbgb9()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1249-1371
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb9()
    
    @staticmethod
    def cmbgb10():
        """
        cmbgb10()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1374-1458
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb10()
    
    @staticmethod
    def cmbgb11():
        """
        cmbgb11()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1461-1561
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb11()
    
    @staticmethod
    def cmbgb12():
        """
        cmbgb12()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1564-1633
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb12()
    
    @staticmethod
    def cmbgb13():
        """
        cmbgb13()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1636-1746
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb13()
    
    @staticmethod
    def cmbgb14():
        """
        cmbgb14()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1749-1833
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb14()
    
    @staticmethod
    def cmbgb15():
        """
        cmbgb15()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1836-1920
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb15()
    
    @staticmethod
    def cmbgb16():
        """
        cmbgb16()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1923-2016
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb16()
    
    @staticmethod
    def lwcldpr():
        """
        lwcldpr()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 2019-2657
        
        
        """
        _rrtmg_lw.f90wrap_lwcldpr()
    
    _dt_array_initialisers = []
    

rrtmg_lw_init = Rrtmg_Lw_Init()

class Rrtmg_Lw_Rad(f90wrap.runtime.FortranModule):
    """
    Module rrtmg_lw_rad
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90 lines 7-906
    
    """
    @staticmethod
    def rrtmg_lw(ncol, nlay, icld, idrv, play, plev, tlay, tlev, tsfc, h2ovmr, \
        o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, \
        emis, inflglw, iceflglw, liqflglw, cldfmcl, taucmcl, ciwpmcl, clwpmcl, \
        reicmcl, relqmcl, tauaer, uflx, dflx, hr, uflxc, dflxc, hrc, duflx_dt=None, \
        duflxc_dt=None):
        """
        rrtmg_lw(ncol, nlay, icld, idrv, play, plev, tlay, tlev, tsfc, h2ovmr, o3vmr, \
            co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis, \
            inflglw, iceflglw, liqflglw, cldfmcl, taucmcl, ciwpmcl, clwpmcl, reicmcl, \
            relqmcl, tauaer, uflx, dflx, hr, uflxc, dflxc, hrc[, duflx_dt, duflxc_dt])
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90 lines 90-577
        
        Parameters
        ----------
        ncol : int
        nlay : int
        icld : int
        idrv : int
        play : float array
        plev : float array
        tlay : float array
        tlev : float array
        tsfc : float array
        h2ovmr : float array
        o3vmr : float array
        co2vmr : float array
        ch4vmr : float array
        n2ovmr : float array
        o2vmr : float array
        cfc11vmr : float array
        cfc12vmr : float array
        cfc22vmr : float array
        ccl4vmr : float array
        emis : float array
        inflglw : int
        iceflglw : int
        liqflglw : int
        cldfmcl : float array
        taucmcl : float array
        ciwpmcl : float array
        clwpmcl : float array
        reicmcl : float array
        relqmcl : float array
        tauaer : float array
        uflx : float array
        dflx : float array
        hr : float array
        uflxc : float array
        dflxc : float array
        hrc : float array
        duflx_dt : float array
        duflxc_dt : float array
        
        """
        _rrtmg_lw.f90wrap_rrtmg_lw(ncol=ncol, nlay=nlay, icld=icld, idrv=idrv, \
            play=play, plev=plev, tlay=tlay, tlev=tlev, tsfc=tsfc, h2ovmr=h2ovmr, \
            o3vmr=o3vmr, co2vmr=co2vmr, ch4vmr=ch4vmr, n2ovmr=n2ovmr, o2vmr=o2vmr, \
            cfc11vmr=cfc11vmr, cfc12vmr=cfc12vmr, cfc22vmr=cfc22vmr, ccl4vmr=ccl4vmr, \
            emis=emis, inflglw=inflglw, iceflglw=iceflglw, liqflglw=liqflglw, \
            cldfmcl=cldfmcl, taucmcl=taucmcl, ciwpmcl=ciwpmcl, clwpmcl=clwpmcl, \
            reicmcl=reicmcl, relqmcl=relqmcl, tauaer=tauaer, uflx=uflx, dflx=dflx, \
            hr=hr, uflxc=uflxc, dflxc=dflxc, hrc=hrc, duflx_dt=duflx_dt, \
            duflxc_dt=duflxc_dt)
    
    @staticmethod
    def inatm(iplon, nlay, icld, iaer, play, plev, tlay, tlev, tsfc, h2ovmr, o3vmr, \
        co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis, \
        inflglw, iceflglw, liqflglw, cldfmcl, taucmcl, ciwpmcl, clwpmcl, reicmcl, \
        relqmcl, tauaer, pavel, pz, tavel, tz, semiss, coldry, wkl, wbrodl, wx, \
        cldfmc, taucmc, ciwpmc, clwpmc, reicmc, relqmc, taua):
        """
        nlayers, tbound, pwvcm, inflag, iceflag, liqflag = inatm(iplon, nlay, icld, \
            iaer, play, plev, tlay, tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, \
            o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, \
            liqflglw, cldfmcl, taucmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, tauaer, \
            pavel, pz, tavel, tz, semiss, coldry, wkl, wbrodl, wx, cldfmc, taucmc, \
            ciwpmc, clwpmc, reicmc, relqmc, taua)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90 lines 587-906
        
        Parameters
        ----------
        iplon : int
        nlay : int
        icld : int
        iaer : int
        play : float array
        plev : float array
        tlay : float array
        tlev : float array
        tsfc : float array
        h2ovmr : float array
        o3vmr : float array
        co2vmr : float array
        ch4vmr : float array
        n2ovmr : float array
        o2vmr : float array
        cfc11vmr : float array
        cfc12vmr : float array
        cfc22vmr : float array
        ccl4vmr : float array
        emis : float array
        inflglw : int
        iceflglw : int
        liqflglw : int
        cldfmcl : float array
        taucmcl : float array
        ciwpmcl : float array
        clwpmcl : float array
        reicmcl : float array
        relqmcl : float array
        tauaer : float array
        pavel : float array
        pz : float array
        tavel : float array
        tz : float array
        semiss : float array
        coldry : float array
        wkl : float array
        wbrodl : float array
        wx : float array
        cldfmc : float array
        taucmc : float array
        ciwpmc : float array
        clwpmc : float array
        reicmc : float array
        relqmc : float array
        taua : float array
        
        Returns
        -------
        nlayers : int
        tbound : float
        pwvcm : float
        inflag : int
        iceflag : int
        liqflag : int
        
        """
        nlayers, tbound, pwvcm, inflag, iceflag, liqflag = \
            _rrtmg_lw.f90wrap_inatm(iplon=iplon, nlay=nlay, icld=icld, iaer=iaer, \
            play=play, plev=plev, tlay=tlay, tlev=tlev, tsfc=tsfc, h2ovmr=h2ovmr, \
            o3vmr=o3vmr, co2vmr=co2vmr, ch4vmr=ch4vmr, n2ovmr=n2ovmr, o2vmr=o2vmr, \
            cfc11vmr=cfc11vmr, cfc12vmr=cfc12vmr, cfc22vmr=cfc22vmr, ccl4vmr=ccl4vmr, \
            emis=emis, inflglw=inflglw, iceflglw=iceflglw, liqflglw=liqflglw, \
            cldfmcl=cldfmcl, taucmcl=taucmcl, ciwpmcl=ciwpmcl, clwpmcl=clwpmcl, \
            reicmcl=reicmcl, relqmcl=relqmcl, tauaer=tauaer, pavel=pavel, pz=pz, \
            tavel=tavel, tz=tz, semiss=semiss, coldry=coldry, wkl=wkl, wbrodl=wbrodl, \
            wx=wx, cldfmc=cldfmc, taucmc=taucmc, ciwpmc=ciwpmc, clwpmc=clwpmc, \
            reicmc=reicmc, relqmc=relqmc, taua=taua)
        return nlayers, tbound, pwvcm, inflag, iceflag, liqflag
    
    _dt_array_initialisers = []
    

rrtmg_lw_rad = Rrtmg_Lw_Rad()

