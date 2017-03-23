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

class Rrlw_Kg01(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg01
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 lines 1-72
    
    """
    @property
    def no1(self):
        """
        Element no1 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 29
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg01__get__no1()
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def kao_mn2(self):
        """
        Element kao_mn2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__kao_mn2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mn2 = self._arrays[array_handle]
        else:
            kao_mn2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__kao_mn2)
            self._arrays[array_handle] = kao_mn2
        return kao_mn2
    
    @kao_mn2.setter
    def kao_mn2(self, kao_mn2):
        self.kao_mn2[...] = kao_mn2
    
    @property
    def kbo_mn2(self):
        """
        Element kbo_mn2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__kbo_mn2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo_mn2 = self._arrays[array_handle]
        else:
            kbo_mn2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__kbo_mn2)
            self._arrays[array_handle] = kbo_mn2
        return kbo_mn2
    
    @kbo_mn2.setter
    def kbo_mn2(self, kbo_mn2):
        self.kbo_mn2[...] = kbo_mn2
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng1(self):
        """
        Element ng1 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 60
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg01__get__ng1()
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 62
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 62
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 63
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 63
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def ka_mn2(self):
        """
        Element ka_mn2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__ka_mn2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mn2 = self._arrays[array_handle]
        else:
            ka_mn2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__ka_mn2)
            self._arrays[array_handle] = ka_mn2
        return ka_mn2
    
    @ka_mn2.setter
    def ka_mn2(self, ka_mn2):
        self.ka_mn2[...] = ka_mn2
    
    @property
    def kb_mn2(self):
        """
        Element kb_mn2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__kb_mn2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb_mn2 = self._arrays[array_handle]
        else:
            kb_mn2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__kb_mn2)
            self._arrays[array_handle] = kb_mn2
        return kb_mn2
    
    @kb_mn2.setter
    def kb_mn2(self, kb_mn2):
        self.kb_mn2[...] = kb_mn2
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg01__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg01__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg01>{\n']
        ret.append('    no1 : ')
        ret.append(repr(self.no1))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    kao_mn2 : ')
        ret.append(repr(self.kao_mn2))
        ret.append(',\n    kbo_mn2 : ')
        ret.append(repr(self.kbo_mn2))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng1 : ')
        ret.append(repr(self.ng1))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    ka_mn2 : ')
        ret.append(repr(self.ka_mn2))
        ret.append(',\n    kb_mn2 : ')
        ret.append(repr(self.kb_mn2))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg01 = Rrlw_Kg01()

class Rrlw_Kg02(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg02
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 lines 1-68
    
    """
    @property
    def no2(self):
        """
        Element no2 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 27
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg02__get__no2()
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 29
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 29
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 30
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng2(self):
        """
        Element ng2 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 57
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg02__get__ng2()
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 59
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 59
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 60
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 60
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 61
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 61
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 62
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 62
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def refparam(self):
        """
        Element refparam ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg02__array__refparam(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            refparam = self._arrays[array_handle]
        else:
            refparam = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg02__array__refparam)
            self._arrays[array_handle] = refparam
        return refparam
    
    @refparam.setter
    def refparam(self, refparam):
        self.refparam[...] = refparam
    
    def __str__(self):
        ret = ['<rrlw_kg02>{\n']
        ret.append('    no2 : ')
        ret.append(repr(self.no2))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng2 : ')
        ret.append(repr(self.ng2))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append(',\n    refparam : ')
        ret.append(repr(self.refparam))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg02 = Rrlw_Kg02()

class Rrlw_Kg03(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg03
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 lines 1-73
    
    """
    @property
    def no3(self):
        """
        Element no3 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 29
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg03__get__no3()
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def kao_mn2o(self):
        """
        Element kao_mn2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__kao_mn2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mn2o = self._arrays[array_handle]
        else:
            kao_mn2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__kao_mn2o)
            self._arrays[array_handle] = kao_mn2o
        return kao_mn2o
    
    @kao_mn2o.setter
    def kao_mn2o(self, kao_mn2o):
        self.kao_mn2o[...] = kao_mn2o
    
    @property
    def kbo_mn2o(self):
        """
        Element kbo_mn2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__kbo_mn2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo_mn2o = self._arrays[array_handle]
        else:
            kbo_mn2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__kbo_mn2o)
            self._arrays[array_handle] = kbo_mn2o
        return kbo_mn2o
    
    @kbo_mn2o.setter
    def kbo_mn2o(self, kbo_mn2o):
        self.kbo_mn2o[...] = kbo_mn2o
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng3(self):
        """
        Element ng3 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 62
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg03__get__ng3()
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def ka_mn2o(self):
        """
        Element ka_mn2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__ka_mn2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mn2o = self._arrays[array_handle]
        else:
            ka_mn2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__ka_mn2o)
            self._arrays[array_handle] = ka_mn2o
        return ka_mn2o
    
    @ka_mn2o.setter
    def ka_mn2o(self, ka_mn2o):
        self.ka_mn2o[...] = ka_mn2o
    
    @property
    def kb_mn2o(self):
        """
        Element kb_mn2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__kb_mn2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb_mn2o = self._arrays[array_handle]
        else:
            kb_mn2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__kb_mn2o)
            self._arrays[array_handle] = kb_mn2o
        return kb_mn2o
    
    @kb_mn2o.setter
    def kb_mn2o(self, kb_mn2o):
        self.kb_mn2o[...] = kb_mn2o
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg03__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg03__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg03>{\n']
        ret.append('    no3 : ')
        ret.append(repr(self.no3))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    kao_mn2o : ')
        ret.append(repr(self.kao_mn2o))
        ret.append(',\n    kbo_mn2o : ')
        ret.append(repr(self.kbo_mn2o))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng3 : ')
        ret.append(repr(self.ng3))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    ka_mn2o : ')
        ret.append(repr(self.ka_mn2o))
        ret.append(',\n    kb_mn2o : ')
        ret.append(repr(self.kb_mn2o))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg03 = Rrlw_Kg03()

class Rrlw_Kg04(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg04
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 lines 1-62
    
    """
    @property
    def no4(self):
        """
        Element no4 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 27
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg04__get__no4()
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 29
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 29
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 30
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng4(self):
        """
        Element ng4 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 55
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg04__get__ng4()
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 57
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 57
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 58
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 58
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 59
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 59
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 60
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90 line 60
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg04__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg04__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg04>{\n']
        ret.append('    no4 : ')
        ret.append(repr(self.no4))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng4 : ')
        ret.append(repr(self.ng4))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg04 = Rrlw_Kg04()

class Rrlw_Kg05(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg05
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 lines 1-74
    
    """
    @property
    def no5(self):
        """
        Element no5 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 29
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg05__get__no5()
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def kao_mo3(self):
        """
        Element kao_mo3 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__kao_mo3(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mo3 = self._arrays[array_handle]
        else:
            kao_mo3 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__kao_mo3)
            self._arrays[array_handle] = kao_mo3
        return kao_mo3
    
    @kao_mo3.setter
    def kao_mo3(self, kao_mo3):
        self.kao_mo3[...] = kao_mo3
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ccl4o(self):
        """
        Element ccl4o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__ccl4o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ccl4o = self._arrays[array_handle]
        else:
            ccl4o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__ccl4o)
            self._arrays[array_handle] = ccl4o
        return ccl4o
    
    @ccl4o.setter
    def ccl4o(self, ccl4o):
        self.ccl4o[...] = ccl4o
    
    @property
    def ng5(self):
        """
        Element ng5 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 63
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg05__get__ng5()
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def ka_mo3(self):
        """
        Element ka_mo3 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__ka_mo3(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mo3 = self._arrays[array_handle]
        else:
            ka_mo3 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__ka_mo3)
            self._arrays[array_handle] = ka_mo3
        return ka_mo3
    
    @ka_mo3.setter
    def ka_mo3(self, ka_mo3):
        self.ka_mo3[...] = ka_mo3
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def ccl4(self):
        """
        Element ccl4 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg05__array__ccl4(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ccl4 = self._arrays[array_handle]
        else:
            ccl4 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg05__array__ccl4)
            self._arrays[array_handle] = ccl4
        return ccl4
    
    @ccl4.setter
    def ccl4(self, ccl4):
        self.ccl4[...] = ccl4
    
    def __str__(self):
        ret = ['<rrlw_kg05>{\n']
        ret.append('    no5 : ')
        ret.append(repr(self.no5))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    kao_mo3 : ')
        ret.append(repr(self.kao_mo3))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ccl4o : ')
        ret.append(repr(self.ccl4o))
        ret.append(',\n    ng5 : ')
        ret.append(repr(self.ng5))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    ka_mo3 : ')
        ret.append(repr(self.ka_mo3))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append(',\n    ccl4 : ')
        ret.append(repr(self.ccl4))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg05 = Rrlw_Kg05()

class Rrlw_Kg06(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg06
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 lines 1-72
    
    """
    @property
    def no6(self):
        """
        Element no6 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 28
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg06__get__no6()
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 30
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kao_mco2(self):
        """
        Element kao_mco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__kao_mco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mco2 = self._arrays[array_handle]
        else:
            kao_mco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__kao_mco2)
            self._arrays[array_handle] = kao_mco2
        return kao_mco2
    
    @kao_mco2.setter
    def kao_mco2(self, kao_mco2):
        self.kao_mco2[...] = kao_mco2
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def cfc11adjo(self):
        """
        Element cfc11adjo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__cfc11adjo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            cfc11adjo = self._arrays[array_handle]
        else:
            cfc11adjo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__cfc11adjo)
            self._arrays[array_handle] = cfc11adjo
        return cfc11adjo
    
    @cfc11adjo.setter
    def cfc11adjo(self, cfc11adjo):
        self.cfc11adjo[...] = cfc11adjo
    
    @property
    def cfc12o(self):
        """
        Element cfc12o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__cfc12o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            cfc12o = self._arrays[array_handle]
        else:
            cfc12o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__cfc12o)
            self._arrays[array_handle] = cfc12o
        return cfc12o
    
    @cfc12o.setter
    def cfc12o(self, cfc12o):
        self.cfc12o[...] = cfc12o
    
    @property
    def ng6(self):
        """
        Element ng6 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 61
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg06__get__ng6()
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 63
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def ka_mco2(self):
        """
        Element ka_mco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__ka_mco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mco2 = self._arrays[array_handle]
        else:
            ka_mco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__ka_mco2)
            self._arrays[array_handle] = ka_mco2
        return ka_mco2
    
    @ka_mco2.setter
    def ka_mco2(self, ka_mco2):
        self.ka_mco2[...] = ka_mco2
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def cfc11adj(self):
        """
        Element cfc11adj ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__cfc11adj(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            cfc11adj = self._arrays[array_handle]
        else:
            cfc11adj = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__cfc11adj)
            self._arrays[array_handle] = cfc11adj
        return cfc11adj
    
    @cfc11adj.setter
    def cfc11adj(self, cfc11adj):
        self.cfc11adj[...] = cfc11adj
    
    @property
    def cfc12(self):
        """
        Element cfc12 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg06__array__cfc12(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            cfc12 = self._arrays[array_handle]
        else:
            cfc12 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg06__array__cfc12)
            self._arrays[array_handle] = cfc12
        return cfc12
    
    @cfc12.setter
    def cfc12(self, cfc12):
        self.cfc12[...] = cfc12
    
    def __str__(self):
        ret = ['<rrlw_kg06>{\n']
        ret.append('    no6 : ')
        ret.append(repr(self.no6))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kao_mco2 : ')
        ret.append(repr(self.kao_mco2))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    cfc11adjo : ')
        ret.append(repr(self.cfc11adjo))
        ret.append(',\n    cfc12o : ')
        ret.append(repr(self.cfc12o))
        ret.append(',\n    ng6 : ')
        ret.append(repr(self.ng6))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    ka_mco2 : ')
        ret.append(repr(self.ka_mco2))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append(',\n    cfc11adj : ')
        ret.append(repr(self.cfc11adj))
        ret.append(',\n    cfc12 : ')
        ret.append(repr(self.cfc12))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg06 = Rrlw_Kg06()

class Rrlw_Kg07(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg07
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 lines 1-74
    
    """
    @property
    def no7(self):
        """
        Element no7 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 29
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg07__get__no7()
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def kao_mco2(self):
        """
        Element kao_mco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__kao_mco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mco2 = self._arrays[array_handle]
        else:
            kao_mco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__kao_mco2)
            self._arrays[array_handle] = kao_mco2
        return kao_mco2
    
    @kao_mco2.setter
    def kao_mco2(self, kao_mco2):
        self.kao_mco2[...] = kao_mco2
    
    @property
    def kbo_mco2(self):
        """
        Element kbo_mco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__kbo_mco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo_mco2 = self._arrays[array_handle]
        else:
            kbo_mco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__kbo_mco2)
            self._arrays[array_handle] = kbo_mco2
        return kbo_mco2
    
    @kbo_mco2.setter
    def kbo_mco2(self, kbo_mco2):
        self.kbo_mco2[...] = kbo_mco2
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng7(self):
        """
        Element ng7 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 63
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg07__get__ng7()
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def ka_mco2(self):
        """
        Element ka_mco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__ka_mco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mco2 = self._arrays[array_handle]
        else:
            ka_mco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__ka_mco2)
            self._arrays[array_handle] = ka_mco2
        return ka_mco2
    
    @ka_mco2.setter
    def ka_mco2(self, ka_mco2):
        self.ka_mco2[...] = ka_mco2
    
    @property
    def kb_mco2(self):
        """
        Element kb_mco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__kb_mco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb_mco2 = self._arrays[array_handle]
        else:
            kb_mco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__kb_mco2)
            self._arrays[array_handle] = kb_mco2
        return kb_mco2
    
    @kb_mco2.setter
    def kb_mco2(self, kb_mco2):
        self.kb_mco2[...] = kb_mco2
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90 line 72
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg07__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg07__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg07>{\n']
        ret.append('    no7 : ')
        ret.append(repr(self.no7))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    kao_mco2 : ')
        ret.append(repr(self.kao_mco2))
        ret.append(',\n    kbo_mco2 : ')
        ret.append(repr(self.kbo_mco2))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng7 : ')
        ret.append(repr(self.ng7))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    ka_mco2 : ')
        ret.append(repr(self.ka_mco2))
        ret.append(',\n    kb_mco2 : ')
        ret.append(repr(self.kb_mco2))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg07 = Rrlw_Kg07()

class Rrlw_Kg08(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg08
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 lines 1-98
    
    """
    @property
    def no8(self):
        """
        Element no8 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 34
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg08__get__no8()
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def cfc12o(self):
        """
        Element cfc12o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__cfc12o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            cfc12o = self._arrays[array_handle]
        else:
            cfc12o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__cfc12o)
            self._arrays[array_handle] = cfc12o
        return cfc12o
    
    @cfc12o.setter
    def cfc12o(self, cfc12o):
        self.cfc12o[...] = cfc12o
    
    @property
    def cfc22adjo(self):
        """
        Element cfc22adjo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 39
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__cfc22adjo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            cfc22adjo = self._arrays[array_handle]
        else:
            cfc22adjo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__cfc22adjo)
            self._arrays[array_handle] = cfc22adjo
        return cfc22adjo
    
    @cfc22adjo.setter
    def cfc22adjo(self, cfc22adjo):
        self.cfc22adjo[...] = cfc22adjo
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 41
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kao_mco2(self):
        """
        Element kao_mco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 42
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__kao_mco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mco2 = self._arrays[array_handle]
        else:
            kao_mco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__kao_mco2)
            self._arrays[array_handle] = kao_mco2
        return kao_mco2
    
    @kao_mco2.setter
    def kao_mco2(self, kao_mco2):
        self.kao_mco2[...] = kao_mco2
    
    @property
    def kao_mn2o(self):
        """
        Element kao_mn2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 43
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__kao_mn2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mn2o = self._arrays[array_handle]
        else:
            kao_mn2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__kao_mn2o)
            self._arrays[array_handle] = kao_mn2o
        return kao_mn2o
    
    @kao_mn2o.setter
    def kao_mn2o(self, kao_mn2o):
        self.kao_mn2o[...] = kao_mn2o
    
    @property
    def kao_mo3(self):
        """
        Element kao_mo3 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 44
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__kao_mo3(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mo3 = self._arrays[array_handle]
        else:
            kao_mo3 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__kao_mo3)
            self._arrays[array_handle] = kao_mo3
        return kao_mo3
    
    @kao_mo3.setter
    def kao_mo3(self, kao_mo3):
        self.kao_mo3[...] = kao_mo3
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 45
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def kbo_mco2(self):
        """
        Element kbo_mco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 46
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__kbo_mco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo_mco2 = self._arrays[array_handle]
        else:
            kbo_mco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__kbo_mco2)
            self._arrays[array_handle] = kbo_mco2
        return kbo_mco2
    
    @kbo_mco2.setter
    def kbo_mco2(self, kbo_mco2):
        self.kbo_mco2[...] = kbo_mco2
    
    @property
    def kbo_mn2o(self):
        """
        Element kbo_mn2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 47
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__kbo_mn2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo_mn2o = self._arrays[array_handle]
        else:
            kbo_mn2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__kbo_mn2o)
            self._arrays[array_handle] = kbo_mn2o
        return kbo_mn2o
    
    @kbo_mn2o.setter
    def kbo_mn2o(self, kbo_mn2o):
        self.kbo_mn2o[...] = kbo_mn2o
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 48
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 49
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng8(self):
        """
        Element ng8 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 80
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg08__get__ng8()
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 82
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 83
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def cfc12(self):
        """
        Element cfc12 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 84
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__cfc12(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            cfc12 = self._arrays[array_handle]
        else:
            cfc12 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__cfc12)
            self._arrays[array_handle] = cfc12
        return cfc12
    
    @cfc12.setter
    def cfc12(self, cfc12):
        self.cfc12[...] = cfc12
    
    @property
    def cfc22adj(self):
        """
        Element cfc22adj ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 85
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__cfc22adj(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            cfc22adj = self._arrays[array_handle]
        else:
            cfc22adj = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__cfc22adj)
            self._arrays[array_handle] = cfc22adj
        return cfc22adj
    
    @cfc22adj.setter
    def cfc22adj(self, cfc22adj):
        self.cfc22adj[...] = cfc22adj
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 87
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 87
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 88
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 88
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def ka_mco2(self):
        """
        Element ka_mco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 89
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__ka_mco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mco2 = self._arrays[array_handle]
        else:
            ka_mco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__ka_mco2)
            self._arrays[array_handle] = ka_mco2
        return ka_mco2
    
    @ka_mco2.setter
    def ka_mco2(self, ka_mco2):
        self.ka_mco2[...] = ka_mco2
    
    @property
    def ka_mn2o(self):
        """
        Element ka_mn2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 90
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__ka_mn2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mn2o = self._arrays[array_handle]
        else:
            ka_mn2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__ka_mn2o)
            self._arrays[array_handle] = ka_mn2o
        return ka_mn2o
    
    @ka_mn2o.setter
    def ka_mn2o(self, ka_mn2o):
        self.ka_mn2o[...] = ka_mn2o
    
    @property
    def ka_mo3(self):
        """
        Element ka_mo3 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 91
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__ka_mo3(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mo3 = self._arrays[array_handle]
        else:
            ka_mo3 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__ka_mo3)
            self._arrays[array_handle] = ka_mo3
        return ka_mo3
    
    @ka_mo3.setter
    def ka_mo3(self, ka_mo3):
        self.ka_mo3[...] = ka_mo3
    
    @property
    def kb_mco2(self):
        """
        Element kb_mco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 92
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__kb_mco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb_mco2 = self._arrays[array_handle]
        else:
            kb_mco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__kb_mco2)
            self._arrays[array_handle] = kb_mco2
        return kb_mco2
    
    @kb_mco2.setter
    def kb_mco2(self, kb_mco2):
        self.kb_mco2[...] = kb_mco2
    
    @property
    def kb_mn2o(self):
        """
        Element kb_mn2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 93
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__kb_mn2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb_mn2o = self._arrays[array_handle]
        else:
            kb_mn2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__kb_mn2o)
            self._arrays[array_handle] = kb_mn2o
        return kb_mn2o
    
    @kb_mn2o.setter
    def kb_mn2o(self, kb_mn2o):
        self.kb_mn2o[...] = kb_mn2o
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 94
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90 line 95
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg08__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg08__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg08>{\n']
        ret.append('    no8 : ')
        ret.append(repr(self.no8))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    cfc12o : ')
        ret.append(repr(self.cfc12o))
        ret.append(',\n    cfc22adjo : ')
        ret.append(repr(self.cfc22adjo))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kao_mco2 : ')
        ret.append(repr(self.kao_mco2))
        ret.append(',\n    kao_mn2o : ')
        ret.append(repr(self.kao_mn2o))
        ret.append(',\n    kao_mo3 : ')
        ret.append(repr(self.kao_mo3))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    kbo_mco2 : ')
        ret.append(repr(self.kbo_mco2))
        ret.append(',\n    kbo_mn2o : ')
        ret.append(repr(self.kbo_mn2o))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng8 : ')
        ret.append(repr(self.ng8))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    cfc12 : ')
        ret.append(repr(self.cfc12))
        ret.append(',\n    cfc22adj : ')
        ret.append(repr(self.cfc22adj))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    ka_mco2 : ')
        ret.append(repr(self.ka_mco2))
        ret.append(',\n    ka_mn2o : ')
        ret.append(repr(self.ka_mn2o))
        ret.append(',\n    ka_mo3 : ')
        ret.append(repr(self.ka_mo3))
        ret.append(',\n    kb_mco2 : ')
        ret.append(repr(self.kb_mco2))
        ret.append(',\n    kb_mn2o : ')
        ret.append(repr(self.kb_mn2o))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg08 = Rrlw_Kg08()

class Rrlw_Kg09(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg09
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 lines 1-76
    
    """
    @property
    def no9(self):
        """
        Element no9 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 29
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg09__get__no9()
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def kao_mn2o(self):
        """
        Element kao_mn2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__kao_mn2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mn2o = self._arrays[array_handle]
        else:
            kao_mn2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__kao_mn2o)
            self._arrays[array_handle] = kao_mn2o
        return kao_mn2o
    
    @kao_mn2o.setter
    def kao_mn2o(self, kao_mn2o):
        self.kao_mn2o[...] = kao_mn2o
    
    @property
    def kbo_mn2o(self):
        """
        Element kbo_mn2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__kbo_mn2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo_mn2o = self._arrays[array_handle]
        else:
            kbo_mn2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__kbo_mn2o)
            self._arrays[array_handle] = kbo_mn2o
        return kbo_mn2o
    
    @kbo_mn2o.setter
    def kbo_mn2o(self, kbo_mn2o):
        self.kbo_mn2o[...] = kbo_mn2o
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 39
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng9(self):
        """
        Element ng9 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 65
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg09__get__ng9()
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def ka_mn2o(self):
        """
        Element ka_mn2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__ka_mn2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mn2o = self._arrays[array_handle]
        else:
            ka_mn2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__ka_mn2o)
            self._arrays[array_handle] = ka_mn2o
        return ka_mn2o
    
    @ka_mn2o.setter
    def ka_mn2o(self, ka_mn2o):
        self.ka_mn2o[...] = ka_mn2o
    
    @property
    def kb_mn2o(self):
        """
        Element kb_mn2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 72
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__kb_mn2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb_mn2o = self._arrays[array_handle]
        else:
            kb_mn2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__kb_mn2o)
            self._arrays[array_handle] = kb_mn2o
        return kb_mn2o
    
    @kb_mn2o.setter
    def kb_mn2o(self, kb_mn2o):
        self.kb_mn2o[...] = kb_mn2o
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 73
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90 line 74
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg09__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg09__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg09>{\n']
        ret.append('    no9 : ')
        ret.append(repr(self.no9))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    kao_mn2o : ')
        ret.append(repr(self.kao_mn2o))
        ret.append(',\n    kbo_mn2o : ')
        ret.append(repr(self.kbo_mn2o))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng9 : ')
        ret.append(repr(self.ng9))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    ka_mn2o : ')
        ret.append(repr(self.ka_mn2o))
        ret.append(',\n    kb_mn2o : ')
        ret.append(repr(self.kb_mn2o))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg09 = Rrlw_Kg09()

class Rrlw_Kg10(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg10
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 lines 1-69
    
    """
    @property
    def no10(self):
        """
        Element no10 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 27
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg10__get__no10()
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 29
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 30
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng10(self):
        """
        Element ng10 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 59
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg10__get__ng10()
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 61
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 62
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg10__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg10__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg10>{\n']
        ret.append('    no10 : ')
        ret.append(repr(self.no10))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng10 : ')
        ret.append(repr(self.ng10))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg10 = Rrlw_Kg10()

class Rrlw_Kg11(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg11
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 lines 1-77
    
    """
    @property
    def no11(self):
        """
        Element no11 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 29
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg11__get__no11()
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def kao_mo2(self):
        """
        Element kao_mo2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__kao_mo2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mo2 = self._arrays[array_handle]
        else:
            kao_mo2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__kao_mo2)
            self._arrays[array_handle] = kao_mo2
        return kao_mo2
    
    @kao_mo2.setter
    def kao_mo2(self, kao_mo2):
        self.kao_mo2[...] = kao_mo2
    
    @property
    def kbo_mo2(self):
        """
        Element kbo_mo2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__kbo_mo2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo_mo2 = self._arrays[array_handle]
        else:
            kbo_mo2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__kbo_mo2)
            self._arrays[array_handle] = kbo_mo2
        return kbo_mo2
    
    @kbo_mo2.setter
    def kbo_mo2(self, kbo_mo2):
        self.kbo_mo2[...] = kbo_mo2
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 39
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng11(self):
        """
        Element ng11 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 65
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg11__get__ng11()
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def ka_mo2(self):
        """
        Element ka_mo2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 72
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__ka_mo2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mo2 = self._arrays[array_handle]
        else:
            ka_mo2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__ka_mo2)
            self._arrays[array_handle] = ka_mo2
        return ka_mo2
    
    @ka_mo2.setter
    def ka_mo2(self, ka_mo2):
        self.ka_mo2[...] = ka_mo2
    
    @property
    def kb_mo2(self):
        """
        Element kb_mo2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 73
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__kb_mo2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb_mo2 = self._arrays[array_handle]
        else:
            kb_mo2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__kb_mo2)
            self._arrays[array_handle] = kb_mo2
        return kb_mo2
    
    @kb_mo2.setter
    def kb_mo2(self, kb_mo2):
        self.kb_mo2[...] = kb_mo2
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 74
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90 line 75
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg11__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg11__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg11>{\n']
        ret.append('    no11 : ')
        ret.append(repr(self.no11))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    kao_mo2 : ')
        ret.append(repr(self.kao_mo2))
        ret.append(',\n    kbo_mo2 : ')
        ret.append(repr(self.kbo_mo2))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng11 : ')
        ret.append(repr(self.ng11))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    ka_mo2 : ')
        ret.append(repr(self.ka_mo2))
        ret.append(',\n    kb_mo2 : ')
        ret.append(repr(self.kb_mo2))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg11 = Rrlw_Kg11()

class Rrlw_Kg12(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg12
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90 lines 1-58
    
    """
    @property
    def no12(self):
        """
        Element no12 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90 line 25
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg12__get__no12()
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90 line 27
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg12__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg12__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90 line 28
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg12__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg12__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90 line 29
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg12__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg12__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90 line 30
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg12__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg12__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng12(self):
        """
        Element ng12 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90 line 51
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg12__get__ng12()
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90 line 53
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg12__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg12__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90 line 54
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg12__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg12__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90 line 54
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg12__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg12__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90 line 55
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg12__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg12__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90 line 56
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg12__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg12__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg12>{\n']
        ret.append('    no12 : ')
        ret.append(repr(self.no12))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng12 : ')
        ret.append(repr(self.ng12))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg12 = Rrlw_Kg12()

class Rrlw_Kg13(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg13
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 lines 1-74
    
    """
    @property
    def no13(self):
        """
        Element no13 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 28
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg13__get__no13()
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 30
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kao_mco2(self):
        """
        Element kao_mco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__kao_mco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mco2 = self._arrays[array_handle]
        else:
            kao_mco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__kao_mco2)
            self._arrays[array_handle] = kao_mco2
        return kao_mco2
    
    @kao_mco2.setter
    def kao_mco2(self, kao_mco2):
        self.kao_mco2[...] = kao_mco2
    
    @property
    def kao_mco(self):
        """
        Element kao_mco ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__kao_mco(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mco = self._arrays[array_handle]
        else:
            kao_mco = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__kao_mco)
            self._arrays[array_handle] = kao_mco
        return kao_mco
    
    @kao_mco.setter
    def kao_mco(self, kao_mco):
        self.kao_mco[...] = kao_mco
    
    @property
    def kbo_mo3(self):
        """
        Element kbo_mo3 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__kbo_mo3(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo_mo3 = self._arrays[array_handle]
        else:
            kbo_mo3 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__kbo_mo3)
            self._arrays[array_handle] = kbo_mo3
        return kbo_mo3
    
    @kbo_mo3.setter
    def kbo_mo3(self, kbo_mo3):
        self.kbo_mo3[...] = kbo_mo3
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng13(self):
        """
        Element ng13 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 62
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg13__get__ng13()
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def ka_mco2(self):
        """
        Element ka_mco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__ka_mco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mco2 = self._arrays[array_handle]
        else:
            ka_mco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__ka_mco2)
            self._arrays[array_handle] = ka_mco2
        return ka_mco2
    
    @ka_mco2.setter
    def ka_mco2(self, ka_mco2):
        self.ka_mco2[...] = ka_mco2
    
    @property
    def ka_mco(self):
        """
        Element ka_mco ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__ka_mco(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mco = self._arrays[array_handle]
        else:
            ka_mco = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__ka_mco)
            self._arrays[array_handle] = ka_mco
        return ka_mco
    
    @ka_mco.setter
    def ka_mco(self, ka_mco):
        self.ka_mco[...] = ka_mco
    
    @property
    def kb_mo3(self):
        """
        Element kb_mo3 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__kb_mo3(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb_mo3 = self._arrays[array_handle]
        else:
            kb_mo3 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__kb_mo3)
            self._arrays[array_handle] = kb_mo3
        return kb_mo3
    
    @kb_mo3.setter
    def kb_mo3(self, kb_mo3):
        self.kb_mo3[...] = kb_mo3
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90 line 72
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg13__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg13__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg13>{\n']
        ret.append('    no13 : ')
        ret.append(repr(self.no13))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kao_mco2 : ')
        ret.append(repr(self.kao_mco2))
        ret.append(',\n    kao_mco : ')
        ret.append(repr(self.kao_mco))
        ret.append(',\n    kbo_mo3 : ')
        ret.append(repr(self.kbo_mo3))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng13 : ')
        ret.append(repr(self.ng13))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    ka_mco2 : ')
        ret.append(repr(self.ka_mco2))
        ret.append(',\n    ka_mco : ')
        ret.append(repr(self.ka_mco))
        ret.append(',\n    kb_mo3 : ')
        ret.append(repr(self.kb_mo3))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg13 = Rrlw_Kg13()

class Rrlw_Kg14(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg14
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 lines 1-69
    
    """
    @property
    def no14(self):
        """
        Element no14 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 27
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg14__get__no14()
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 29
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 30
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng14(self):
        """
        Element ng14 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 59
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg14__get__ng14()
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 61
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 62
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg14__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg14__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg14>{\n']
        ret.append('    no14 : ')
        ret.append(repr(self.no14))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng14 : ')
        ret.append(repr(self.ng14))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg14 = Rrlw_Kg14()

class Rrlw_Kg15(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg15
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 lines 1-63
    
    """
    @property
    def no15(self):
        """
        Element no15 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 26
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg15__get__no15()
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 28
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg15__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg15__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 29
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg15__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg15__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kao_mn2(self):
        """
        Element kao_mn2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 30
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg15__array__kao_mn2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao_mn2 = self._arrays[array_handle]
        else:
            kao_mn2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg15__array__kao_mn2)
            self._arrays[array_handle] = kao_mn2
        return kao_mn2
    
    @kao_mn2.setter
    def kao_mn2(self, kao_mn2):
        self.kao_mn2[...] = kao_mn2
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg15__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg15__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg15__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg15__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng15(self):
        """
        Element ng15 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 55
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg15__get__ng15()
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 57
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg15__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg15__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 58
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg15__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg15__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 58
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg15__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg15__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def ka_mn2(self):
        """
        Element ka_mn2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 59
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg15__array__ka_mn2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka_mn2 = self._arrays[array_handle]
        else:
            ka_mn2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg15__array__ka_mn2)
            self._arrays[array_handle] = ka_mn2
        return ka_mn2
    
    @ka_mn2.setter
    def ka_mn2(self, ka_mn2):
        self.ka_mn2[...] = ka_mn2
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 60
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg15__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg15__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90 line 61
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg15__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg15__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg15>{\n']
        ret.append('    no15 : ')
        ret.append(repr(self.no15))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kao_mn2 : ')
        ret.append(repr(self.kao_mn2))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng15 : ')
        ret.append(repr(self.ng15))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    ka_mn2 : ')
        ret.append(repr(self.ka_mn2))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg15 = Rrlw_Kg15()

class Rrlw_Kg16(f90wrap.runtime.FortranModule):
    """
    Module rrlw_kg16
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 lines 1-68
    
    """
    @property
    def no16(self):
        """
        Element no16 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 26
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg16__get__no16()
    
    @property
    def fracrefbo(self):
        """
        Element fracrefbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 28
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__fracrefbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefbo = self._arrays[array_handle]
        else:
            fracrefbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__fracrefbo)
            self._arrays[array_handle] = fracrefbo
        return fracrefbo
    
    @fracrefbo.setter
    def fracrefbo(self, fracrefbo):
        self.fracrefbo[...] = fracrefbo
    
    @property
    def fracrefao(self):
        """
        Element fracrefao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 30
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__fracrefao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefao = self._arrays[array_handle]
        else:
            fracrefao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__fracrefao)
            self._arrays[array_handle] = fracrefao
        return fracrefao
    
    @fracrefao.setter
    def fracrefao(self, fracrefao):
        self.fracrefao[...] = fracrefao
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def ng16(self):
        """
        Element ng16 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 57
        
        """
        return _rrtmg_lw.f90wrap_rrlw_kg16__get__ng16()
    
    @property
    def fracrefb(self):
        """
        Element fracrefb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 59
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__fracrefb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefb = self._arrays[array_handle]
        else:
            fracrefb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__fracrefb)
            self._arrays[array_handle] = fracrefb
        return fracrefb
    
    @fracrefb.setter
    def fracrefb(self, fracrefb):
        self.fracrefb[...] = fracrefb
    
    @property
    def fracrefa(self):
        """
        Element fracrefa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 61
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__fracrefa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            fracrefa = self._arrays[array_handle]
        else:
            fracrefa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__fracrefa)
            self._arrays[array_handle] = fracrefa
        return fracrefa
    
    @fracrefa.setter
    def fracrefa(self, fracrefa):
        self.fracrefa[...] = fracrefa
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 62
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 62
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 63
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 63
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_lw.f90wrap_rrlw_kg16__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_lw.f90wrap_rrlw_kg16__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    def __str__(self):
        ret = ['<rrlw_kg16>{\n']
        ret.append('    no16 : ')
        ret.append(repr(self.no16))
        ret.append(',\n    fracrefbo : ')
        ret.append(repr(self.fracrefbo))
        ret.append(',\n    fracrefao : ')
        ret.append(repr(self.fracrefao))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    ng16 : ')
        ret.append(repr(self.ng16))
        ret.append(',\n    fracrefb : ')
        ret.append(repr(self.fracrefb))
        ret.append(',\n    fracrefa : ')
        ret.append(repr(self.fracrefa))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrlw_kg16 = Rrlw_Kg16()

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

