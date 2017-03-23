import _rrtmg_sw
import f90wrap.runtime
import logging

class Parrrsw(f90wrap.runtime.FortranModule):
    """
    Module parrrsw
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 lines 2-117
    
    """
    @property
    def mxlay(self):
        """
        Element mxlay ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 28
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__mxlay()
    
    @property
    def mg(self):
        """
        Element mg ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 29
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__mg()
    
    @property
    def nbndsw(self):
        """
        Element nbndsw ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 30
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__nbndsw()
    
    @property
    def naerec(self):
        """
        Element naerec ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__naerec()
    
    @property
    def mxmol(self):
        """
        Element mxmol ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 32
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__mxmol()
    
    @property
    def nstr(self):
        """
        Element nstr ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 33
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__nstr()
    
    @property
    def nmol(self):
        """
        Element nmol ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 34
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__nmol()
    
    @property
    def ngptsw(self):
        """
        Element ngptsw ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 36
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngptsw()
    
    @property
    def jpband(self):
        """
        Element jpband ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 41
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__jpband()
    
    @property
    def jpb1(self):
        """
        Element jpb1 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 42
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__jpb1()
    
    @property
    def jpb2(self):
        """
        Element jpb2 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 43
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__jpb2()
    
    @property
    def jmcmu(self):
        """
        Element jmcmu ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 45
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__jmcmu()
    
    @property
    def jmumu(self):
        """
        Element jmumu ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 46
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__jmumu()
    
    @property
    def jmphi(self):
        """
        Element jmphi ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 47
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__jmphi()
    
    @property
    def jmxang(self):
        """
        Element jmxang ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 48
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__jmxang()
    
    @property
    def jmxstr(self):
        """
        Element jmxstr ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 49
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__jmxstr()
    
    @property
    def ng16(self):
        """
        Element ng16 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 53
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng16()
    
    @property
    def ng17(self):
        """
        Element ng17 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 54
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng17()
    
    @property
    def ng18(self):
        """
        Element ng18 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 55
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng18()
    
    @property
    def ng19(self):
        """
        Element ng19 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 56
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng19()
    
    @property
    def ng20(self):
        """
        Element ng20 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 57
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng20()
    
    @property
    def ng21(self):
        """
        Element ng21 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 58
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng21()
    
    @property
    def ng22(self):
        """
        Element ng22 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 59
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng22()
    
    @property
    def ng23(self):
        """
        Element ng23 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 60
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng23()
    
    @property
    def ng24(self):
        """
        Element ng24 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 61
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng24()
    
    @property
    def ng25(self):
        """
        Element ng25 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 62
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng25()
    
    @property
    def ng26(self):
        """
        Element ng26 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 63
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng26()
    
    @property
    def ng27(self):
        """
        Element ng27 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 64
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng27()
    
    @property
    def ng28(self):
        """
        Element ng28 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 65
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng28()
    
    @property
    def ng29(self):
        """
        Element ng29 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 66
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ng29()
    
    @property
    def ngs16(self):
        """
        Element ngs16 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 68
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs16()
    
    @property
    def ngs17(self):
        """
        Element ngs17 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 69
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs17()
    
    @property
    def ngs18(self):
        """
        Element ngs18 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 70
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs18()
    
    @property
    def ngs19(self):
        """
        Element ngs19 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 71
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs19()
    
    @property
    def ngs20(self):
        """
        Element ngs20 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 72
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs20()
    
    @property
    def ngs21(self):
        """
        Element ngs21 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 73
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs21()
    
    @property
    def ngs22(self):
        """
        Element ngs22 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 74
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs22()
    
    @property
    def ngs23(self):
        """
        Element ngs23 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 75
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs23()
    
    @property
    def ngs24(self):
        """
        Element ngs24 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 76
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs24()
    
    @property
    def ngs25(self):
        """
        Element ngs25 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 77
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs25()
    
    @property
    def ngs26(self):
        """
        Element ngs26 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 78
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs26()
    
    @property
    def ngs27(self):
        """
        Element ngs27 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 79
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs27()
    
    @property
    def ngs28(self):
        """
        Element ngs28 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 80
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs28()
    
    @property
    def ngs29(self):
        """
        Element ngs29 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 81
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__ngs29()
    
    @property
    def rrsw_scon(self):
        """
        Element rrsw_scon ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/parrrsw.f90 line 115
        
        """
        return _rrtmg_sw.f90wrap_parrrsw__get__rrsw_scon()
    
    def __str__(self):
        ret = ['<parrrsw>{\n']
        ret.append('    mxlay : ')
        ret.append(repr(self.mxlay))
        ret.append(',\n    mg : ')
        ret.append(repr(self.mg))
        ret.append(',\n    nbndsw : ')
        ret.append(repr(self.nbndsw))
        ret.append(',\n    naerec : ')
        ret.append(repr(self.naerec))
        ret.append(',\n    mxmol : ')
        ret.append(repr(self.mxmol))
        ret.append(',\n    nstr : ')
        ret.append(repr(self.nstr))
        ret.append(',\n    nmol : ')
        ret.append(repr(self.nmol))
        ret.append(',\n    ngptsw : ')
        ret.append(repr(self.ngptsw))
        ret.append(',\n    jpband : ')
        ret.append(repr(self.jpband))
        ret.append(',\n    jpb1 : ')
        ret.append(repr(self.jpb1))
        ret.append(',\n    jpb2 : ')
        ret.append(repr(self.jpb2))
        ret.append(',\n    jmcmu : ')
        ret.append(repr(self.jmcmu))
        ret.append(',\n    jmumu : ')
        ret.append(repr(self.jmumu))
        ret.append(',\n    jmphi : ')
        ret.append(repr(self.jmphi))
        ret.append(',\n    jmxang : ')
        ret.append(repr(self.jmxang))
        ret.append(',\n    jmxstr : ')
        ret.append(repr(self.jmxstr))
        ret.append(',\n    ng16 : ')
        ret.append(repr(self.ng16))
        ret.append(',\n    ng17 : ')
        ret.append(repr(self.ng17))
        ret.append(',\n    ng18 : ')
        ret.append(repr(self.ng18))
        ret.append(',\n    ng19 : ')
        ret.append(repr(self.ng19))
        ret.append(',\n    ng20 : ')
        ret.append(repr(self.ng20))
        ret.append(',\n    ng21 : ')
        ret.append(repr(self.ng21))
        ret.append(',\n    ng22 : ')
        ret.append(repr(self.ng22))
        ret.append(',\n    ng23 : ')
        ret.append(repr(self.ng23))
        ret.append(',\n    ng24 : ')
        ret.append(repr(self.ng24))
        ret.append(',\n    ng25 : ')
        ret.append(repr(self.ng25))
        ret.append(',\n    ng26 : ')
        ret.append(repr(self.ng26))
        ret.append(',\n    ng27 : ')
        ret.append(repr(self.ng27))
        ret.append(',\n    ng28 : ')
        ret.append(repr(self.ng28))
        ret.append(',\n    ng29 : ')
        ret.append(repr(self.ng29))
        ret.append(',\n    ngs16 : ')
        ret.append(repr(self.ngs16))
        ret.append(',\n    ngs17 : ')
        ret.append(repr(self.ngs17))
        ret.append(',\n    ngs18 : ')
        ret.append(repr(self.ngs18))
        ret.append(',\n    ngs19 : ')
        ret.append(repr(self.ngs19))
        ret.append(',\n    ngs20 : ')
        ret.append(repr(self.ngs20))
        ret.append(',\n    ngs21 : ')
        ret.append(repr(self.ngs21))
        ret.append(',\n    ngs22 : ')
        ret.append(repr(self.ngs22))
        ret.append(',\n    ngs23 : ')
        ret.append(repr(self.ngs23))
        ret.append(',\n    ngs24 : ')
        ret.append(repr(self.ngs24))
        ret.append(',\n    ngs25 : ')
        ret.append(repr(self.ngs25))
        ret.append(',\n    ngs26 : ')
        ret.append(repr(self.ngs26))
        ret.append(',\n    ngs27 : ')
        ret.append(repr(self.ngs27))
        ret.append(',\n    ngs28 : ')
        ret.append(repr(self.ngs28))
        ret.append(',\n    ngs29 : ')
        ret.append(repr(self.ngs29))
        ret.append(',\n    rrsw_scon : ')
        ret.append(repr(self.rrsw_scon))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

parrrsw = Parrrsw()

class Rrsw_Kg16(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg16
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 lines 1-74
    
    """
    @property
    def no16(self):
        """
        Element no16 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg16__get__no16()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 40
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg16__get__rayl()
    
    @rayl.setter
    def rayl(self, rayl):
        _rrtmg_sw.f90wrap_rrsw_kg16__set__rayl(rayl)
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg16__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg16__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    def __str__(self):
        ret = ['<rrsw_kg16>{\n']
        ret.append('    no16 : ')
        ret.append(repr(self.no16))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
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
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg16 = Rrsw_Kg16()

class Rrsw_Kg17(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg17
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 lines 1-74
    
    """
    @property
    def no17(self):
        """
        Element no17 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg17__get__no17()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 40
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg17__get__rayl()
    
    @rayl.setter
    def rayl(self, rayl):
        _rrtmg_sw.f90wrap_rrsw_kg17__set__rayl(rayl)
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg17__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg17__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    def __str__(self):
        ret = ['<rrsw_kg17>{\n']
        ret.append('    no17 : ')
        ret.append(repr(self.no17))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
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
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg17 = Rrsw_Kg17()

class Rrsw_Kg18(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg18
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 lines 1-74
    
    """
    @property
    def no18(self):
        """
        Element no18 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg18__get__no18()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 40
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg18__get__rayl()
    
    @rayl.setter
    def rayl(self, rayl):
        _rrtmg_sw.f90wrap_rrsw_kg18__set__rayl(rayl)
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg18__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg18__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    def __str__(self):
        ret = ['<rrsw_kg18>{\n']
        ret.append('    no18 : ')
        ret.append(repr(self.no18))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
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
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg18 = Rrsw_Kg18()

class Rrsw_Kg19(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg19
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 lines 1-74
    
    """
    @property
    def no19(self):
        """
        Element no19 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg19__get__no19()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 40
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg19__get__rayl()
    
    @rayl.setter
    def rayl(self, rayl):
        _rrtmg_sw.f90wrap_rrsw_kg19__set__rayl(rayl)
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg19__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg19__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    def __str__(self):
        ret = ['<rrsw_kg19>{\n']
        ret.append('    no19 : ')
        ret.append(repr(self.no19))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
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
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg19 = Rrsw_Kg19()

class Rrsw_Kg20(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg20
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 lines 1-78
    
    """
    @property
    def no20(self):
        """
        Element no20 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 32
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg20__get__no20()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 39
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 39
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def absch4o(self):
        """
        Element absch4o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 40
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__absch4o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absch4o = self._arrays[array_handle]
        else:
            absch4o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__absch4o)
            self._arrays[array_handle] = absch4o
        return absch4o
    
    @absch4o.setter
    def absch4o(self, absch4o):
        self.absch4o[...] = absch4o
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 42
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg20__get__rayl()
    
    @rayl.setter
    def rayl(self, rayl):
        _rrtmg_sw.f90wrap_rrsw_kg20__set__rayl(rayl)
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 72
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 73
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 74
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 74
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    @property
    def absch4(self):
        """
        Element absch4 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90 line 75
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg20__array__absch4(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absch4 = self._arrays[array_handle]
        else:
            absch4 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg20__array__absch4)
            self._arrays[array_handle] = absch4
        return absch4
    
    @absch4.setter
    def absch4(self, absch4):
        self.absch4[...] = absch4
    
    def __str__(self):
        ret = ['<rrsw_kg20>{\n']
        ret.append('    no20 : ')
        ret.append(repr(self.no20))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    absch4o : ')
        ret.append(repr(self.absch4o))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
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
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append(',\n    absch4 : ')
        ret.append(repr(self.absch4))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg20 = Rrsw_Kg20()

class Rrsw_Kg21(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg21
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 lines 1-74
    
    """
    @property
    def no21(self):
        """
        Element no21 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg21__get__no21()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 40
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg21__get__rayl()
    
    @rayl.setter
    def rayl(self, rayl):
        _rrtmg_sw.f90wrap_rrsw_kg21__set__rayl(rayl)
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg21__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg21__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    def __str__(self):
        ret = ['<rrsw_kg21>{\n']
        ret.append('    no21 : ')
        ret.append(repr(self.no21))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
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
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg21 = Rrsw_Kg21()

class Rrsw_Kg22(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg22
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 lines 1-74
    
    """
    @property
    def no22(self):
        """
        Element no22 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg22__get__no22()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 40
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg22__get__rayl()
    
    @rayl.setter
    def rayl(self, rayl):
        _rrtmg_sw.f90wrap_rrsw_kg22__set__rayl(rayl)
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg22__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg22__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    def __str__(self):
        ret = ['<rrsw_kg22>{\n']
        ret.append('    no22 : ')
        ret.append(repr(self.no22))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
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
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg22 = Rrsw_Kg22()

class Rrsw_Kg23(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg23
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 lines 1-71
    
    """
    @property
    def no23(self):
        """
        Element no23 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg23__get__no23()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def raylo(self):
        """
        Element raylo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__raylo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            raylo = self._arrays[array_handle]
        else:
            raylo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__raylo)
            self._arrays[array_handle] = raylo
        return raylo
    
    @raylo.setter
    def raylo(self, raylo):
        self.raylo[...] = raylo
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__rayl(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            rayl = self._arrays[array_handle]
        else:
            rayl = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__rayl)
            self._arrays[array_handle] = rayl
        return rayl
    
    @rayl.setter
    def rayl(self, rayl):
        self.rayl[...] = rayl
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg23__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg23__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    def __str__(self):
        ret = ['<rrsw_kg23>{\n']
        ret.append('    no23 : ')
        ret.append(repr(self.no23))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    raylo : ')
        ret.append(repr(self.raylo))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    selfref : ')
        ret.append(repr(self.selfref))
        ret.append(',\n    forref : ')
        ret.append(repr(self.forref))
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg23 = Rrsw_Kg23()

class Rrsw_Kg24(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg24
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 lines 1-84
    
    """
    @property
    def no24(self):
        """
        Element no24 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 35
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg24__get__no24()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 39
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 39
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 40
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 41
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 42
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 42
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def abso3ao(self):
        """
        Element abso3ao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 43
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__abso3ao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            abso3ao = self._arrays[array_handle]
        else:
            abso3ao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__abso3ao)
            self._arrays[array_handle] = abso3ao
        return abso3ao
    
    @abso3ao.setter
    def abso3ao(self, abso3ao):
        self.abso3ao[...] = abso3ao
    
    @property
    def abso3bo(self):
        """
        Element abso3bo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 43
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__abso3bo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            abso3bo = self._arrays[array_handle]
        else:
            abso3bo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__abso3bo)
            self._arrays[array_handle] = abso3bo
        return abso3bo
    
    @abso3bo.setter
    def abso3bo(self, abso3bo):
        self.abso3bo[...] = abso3bo
    
    @property
    def raylao(self):
        """
        Element raylao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 44
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__raylao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            raylao = self._arrays[array_handle]
        else:
            raylao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__raylao)
            self._arrays[array_handle] = raylao
        return raylao
    
    @raylao.setter
    def raylao(self, raylao):
        self.raylao[...] = raylao
    
    @property
    def raylbo(self):
        """
        Element raylbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 44
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__raylbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            raylbo = self._arrays[array_handle]
        else:
            raylbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__raylbo)
            self._arrays[array_handle] = raylbo
        return raylbo
    
    @raylbo.setter
    def raylbo(self, raylbo):
        self.raylbo[...] = raylbo
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 74
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 74
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 75
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 75
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 76
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 76
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 77
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 78
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 79
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 79
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    @property
    def abso3a(self):
        """
        Element abso3a ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 80
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__abso3a(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            abso3a = self._arrays[array_handle]
        else:
            abso3a = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__abso3a)
            self._arrays[array_handle] = abso3a
        return abso3a
    
    @abso3a.setter
    def abso3a(self, abso3a):
        self.abso3a[...] = abso3a
    
    @property
    def abso3b(self):
        """
        Element abso3b ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 80
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__abso3b(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            abso3b = self._arrays[array_handle]
        else:
            abso3b = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__abso3b)
            self._arrays[array_handle] = abso3b
        return abso3b
    
    @abso3b.setter
    def abso3b(self, abso3b):
        self.abso3b[...] = abso3b
    
    @property
    def rayla(self):
        """
        Element rayla ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 81
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__rayla(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            rayla = self._arrays[array_handle]
        else:
            rayla = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__rayla)
            self._arrays[array_handle] = rayla
        return rayla
    
    @rayla.setter
    def rayla(self, rayla):
        self.rayla[...] = rayla
    
    @property
    def raylb(self):
        """
        Element raylb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90 line 81
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg24__array__raylb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            raylb = self._arrays[array_handle]
        else:
            raylb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg24__array__raylb)
            self._arrays[array_handle] = raylb
        return raylb
    
    @raylb.setter
    def raylb(self, raylb):
        self.raylb[...] = raylb
    
    def __str__(self):
        ret = ['<rrsw_kg24>{\n']
        ret.append('    no24 : ')
        ret.append(repr(self.no24))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    abso3ao : ')
        ret.append(repr(self.abso3ao))
        ret.append(',\n    abso3bo : ')
        ret.append(repr(self.abso3bo))
        ret.append(',\n    raylao : ')
        ret.append(repr(self.raylao))
        ret.append(',\n    raylbo : ')
        ret.append(repr(self.raylbo))
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
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append(',\n    abso3a : ')
        ret.append(repr(self.abso3a))
        ret.append(',\n    abso3b : ')
        ret.append(repr(self.abso3b))
        ret.append(',\n    rayla : ')
        ret.append(repr(self.rayla))
        ret.append(',\n    raylb : ')
        ret.append(repr(self.raylb))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg24 = Rrsw_Kg24()

class Rrsw_Kg25(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg25
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 lines 1-71
    
    """
    @property
    def no25(self):
        """
        Element no25 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg25__get__no25()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def abso3ao(self):
        """
        Element abso3ao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__abso3ao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            abso3ao = self._arrays[array_handle]
        else:
            abso3ao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__abso3ao)
            self._arrays[array_handle] = abso3ao
        return abso3ao
    
    @abso3ao.setter
    def abso3ao(self, abso3ao):
        self.abso3ao[...] = abso3ao
    
    @property
    def abso3bo(self):
        """
        Element abso3bo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__abso3bo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            abso3bo = self._arrays[array_handle]
        else:
            abso3bo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__abso3bo)
            self._arrays[array_handle] = abso3bo
        return abso3bo
    
    @abso3bo.setter
    def abso3bo(self, abso3bo):
        self.abso3bo[...] = abso3bo
    
    @property
    def raylo(self):
        """
        Element raylo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__raylo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            raylo = self._arrays[array_handle]
        else:
            raylo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__raylo)
            self._arrays[array_handle] = raylo
        return raylo
    
    @raylo.setter
    def raylo(self, raylo):
        self.raylo[...] = raylo
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 63
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 63
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    @property
    def abso3a(self):
        """
        Element abso3a ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__abso3a(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            abso3a = self._arrays[array_handle]
        else:
            abso3a = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__abso3a)
            self._arrays[array_handle] = abso3a
        return abso3a
    
    @abso3a.setter
    def abso3a(self, abso3a):
        self.abso3a[...] = abso3a
    
    @property
    def abso3b(self):
        """
        Element abso3b ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__abso3b(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            abso3b = self._arrays[array_handle]
        else:
            abso3b = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__abso3b)
            self._arrays[array_handle] = abso3b
        return abso3b
    
    @abso3b.setter
    def abso3b(self, abso3b):
        self.abso3b[...] = abso3b
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg25__array__rayl(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            rayl = self._arrays[array_handle]
        else:
            rayl = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg25__array__rayl)
            self._arrays[array_handle] = rayl
        return rayl
    
    @rayl.setter
    def rayl(self, rayl):
        self.rayl[...] = rayl
    
    def __str__(self):
        ret = ['<rrsw_kg25>{\n']
        ret.append('    no25 : ')
        ret.append(repr(self.no25))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    abso3ao : ')
        ret.append(repr(self.abso3ao))
        ret.append(',\n    abso3bo : ')
        ret.append(repr(self.abso3bo))
        ret.append(',\n    raylo : ')
        ret.append(repr(self.raylo))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append(',\n    abso3a : ')
        ret.append(repr(self.abso3a))
        ret.append(',\n    abso3b : ')
        ret.append(repr(self.abso3b))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg25 = Rrsw_Kg25()

class Rrsw_Kg26(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg26
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90 lines 1-58
    
    """
    @property
    def no26(self):
        """
        Element no26 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90 line 28
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg26__get__no26()
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90 line 30
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg26__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg26__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg26__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg26__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg26__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg26__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg26__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg26__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def raylo(self):
        """
        Element raylo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg26__array__raylo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            raylo = self._arrays[array_handle]
        else:
            raylo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg26__array__raylo)
            self._arrays[array_handle] = raylo
        return raylo
    
    @raylo.setter
    def raylo(self, raylo):
        self.raylo[...] = raylo
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90 line 54
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg26__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg26__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90 line 55
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg26__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg26__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90 line 56
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg26__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg26__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90 line 56
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg26__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg26__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90 line 57
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg26__array__rayl(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            rayl = self._arrays[array_handle]
        else:
            rayl = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg26__array__rayl)
            self._arrays[array_handle] = rayl
        return rayl
    
    @rayl.setter
    def rayl(self, rayl):
        self.rayl[...] = rayl
    
    def __str__(self):
        ret = ['<rrsw_kg26>{\n']
        ret.append('    no26 : ')
        ret.append(repr(self.no26))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    raylo : ')
        ret.append(repr(self.raylo))
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg26 = Rrsw_Kg26()

class Rrsw_Kg27(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg27
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 lines 1-70
    
    """
    @property
    def no27(self):
        """
        Element no27 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 30
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg27__get__no27()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def raylo(self):
        """
        Element raylo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__raylo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            raylo = self._arrays[array_handle]
        else:
            raylo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__raylo)
            self._arrays[array_handle] = raylo
        return raylo
    
    @raylo.setter
    def raylo(self, raylo):
        self.raylo[...] = raylo
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 62
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 62
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 63
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 63
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 64
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 65
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90 line 67
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg27__array__rayl(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            rayl = self._arrays[array_handle]
        else:
            rayl = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg27__array__rayl)
            self._arrays[array_handle] = rayl
        return rayl
    
    @rayl.setter
    def rayl(self, rayl):
        self.rayl[...] = rayl
    
    def __str__(self):
        ret = ['<rrsw_kg27>{\n']
        ret.append('    no27 : ')
        ret.append(repr(self.no27))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    raylo : ')
        ret.append(repr(self.raylo))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg27 = Rrsw_Kg27()

class Rrsw_Kg28(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg28
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 lines 1-66
    
    """
    @property
    def no28(self):
        """
        Element no28 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 29
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg28__get__no28()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 31
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 32
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 33
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 34
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 37
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg28__get__rayl()
    
    @rayl.setter
    def rayl(self, rayl):
        _rrtmg_sw.f90wrap_rrsw_kg28__set__rayl(rayl)
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 59
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 59
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 60
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 60
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 61
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 62
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 63
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90 line 63
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg28__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg28__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    def __str__(self):
        ret = ['<rrsw_kg28>{\n']
        ret.append('    no28 : ')
        ret.append(repr(self.no28))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
        ret.append(',\n    ka : ')
        ret.append(repr(self.ka))
        ret.append(',\n    absa : ')
        ret.append(repr(self.absa))
        ret.append(',\n    kb : ')
        ret.append(repr(self.kb))
        ret.append(',\n    absb : ')
        ret.append(repr(self.absb))
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg28 = Rrsw_Kg28()

class Rrsw_Kg29(f90wrap.runtime.FortranModule):
    """
    Module rrsw_kg29
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 lines 1-78
    
    """
    @property
    def no29(self):
        """
        Element no29 ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 33
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg29__get__no29()
    
    @property
    def kao(self):
        """
        Element kao ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 35
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__kao(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kao = self._arrays[array_handle]
        else:
            kao = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__kao)
            self._arrays[array_handle] = kao
        return kao
    
    @kao.setter
    def kao(self, kao):
        self.kao[...] = kao
    
    @property
    def kbo(self):
        """
        Element kbo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 36
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__kbo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kbo = self._arrays[array_handle]
        else:
            kbo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__kbo)
            self._arrays[array_handle] = kbo
        return kbo
    
    @kbo.setter
    def kbo(self, kbo):
        self.kbo[...] = kbo
    
    @property
    def selfrefo(self):
        """
        Element selfrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__selfrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfrefo = self._arrays[array_handle]
        else:
            selfrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__selfrefo)
            self._arrays[array_handle] = selfrefo
        return selfrefo
    
    @selfrefo.setter
    def selfrefo(self, selfrefo):
        self.selfrefo[...] = selfrefo
    
    @property
    def forrefo(self):
        """
        Element forrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 37
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__forrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forrefo = self._arrays[array_handle]
        else:
            forrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__forrefo)
            self._arrays[array_handle] = forrefo
        return forrefo
    
    @forrefo.setter
    def forrefo(self, forrefo):
        self.forrefo[...] = forrefo
    
    @property
    def sfluxrefo(self):
        """
        Element sfluxrefo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 38
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__sfluxrefo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxrefo = self._arrays[array_handle]
        else:
            sfluxrefo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__sfluxrefo)
            self._arrays[array_handle] = sfluxrefo
        return sfluxrefo
    
    @sfluxrefo.setter
    def sfluxrefo(self, sfluxrefo):
        self.sfluxrefo[...] = sfluxrefo
    
    @property
    def irradnceo(self):
        """
        Element irradnceo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 39
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__irradnceo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnceo = self._arrays[array_handle]
        else:
            irradnceo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__irradnceo)
            self._arrays[array_handle] = irradnceo
        return irradnceo
    
    @irradnceo.setter
    def irradnceo(self, irradnceo):
        self.irradnceo[...] = irradnceo
    
    @property
    def facbrghto(self):
        """
        Element facbrghto ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 40
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__facbrghto(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrghto = self._arrays[array_handle]
        else:
            facbrghto = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__facbrghto)
            self._arrays[array_handle] = facbrghto
        return facbrghto
    
    @facbrghto.setter
    def facbrghto(self, facbrghto):
        self.facbrghto[...] = facbrghto
    
    @property
    def snsptdrko(self):
        """
        Element snsptdrko ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 40
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__snsptdrko(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrko = self._arrays[array_handle]
        else:
            snsptdrko = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__snsptdrko)
            self._arrays[array_handle] = snsptdrko
        return snsptdrko
    
    @snsptdrko.setter
    def snsptdrko(self, snsptdrko):
        self.snsptdrko[...] = snsptdrko
    
    @property
    def absh2oo(self):
        """
        Element absh2oo ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 41
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__absh2oo(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absh2oo = self._arrays[array_handle]
        else:
            absh2oo = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__absh2oo)
            self._arrays[array_handle] = absh2oo
        return absh2oo
    
    @absh2oo.setter
    def absh2oo(self, absh2oo):
        self.absh2oo[...] = absh2oo
    
    @property
    def absco2o(self):
        """
        Element absco2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 41
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__absco2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absco2o = self._arrays[array_handle]
        else:
            absco2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__absco2o)
            self._arrays[array_handle] = absco2o
        return absco2o
    
    @absco2o.setter
    def absco2o(self, absco2o):
        self.absco2o[...] = absco2o
    
    @property
    def rayl(self):
        """
        Element rayl ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 43
        
        """
        return _rrtmg_sw.f90wrap_rrsw_kg29__get__rayl()
    
    @rayl.setter
    def rayl(self, rayl):
        _rrtmg_sw.f90wrap_rrsw_kg29__set__rayl(rayl)
    
    @property
    def ka(self):
        """
        Element ka ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__ka(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            ka = self._arrays[array_handle]
        else:
            ka = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__ka)
            self._arrays[array_handle] = ka
        return ka
    
    @ka.setter
    def ka(self, ka):
        self.ka[...] = ka
    
    @property
    def absa(self):
        """
        Element absa ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 69
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__absa(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absa = self._arrays[array_handle]
        else:
            absa = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__absa)
            self._arrays[array_handle] = absa
        return absa
    
    @absa.setter
    def absa(self, absa):
        self.absa[...] = absa
    
    @property
    def kb(self):
        """
        Element kb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__kb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            kb = self._arrays[array_handle]
        else:
            kb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__kb)
            self._arrays[array_handle] = kb
        return kb
    
    @kb.setter
    def kb(self, kb):
        self.kb[...] = kb
    
    @property
    def absb(self):
        """
        Element absb ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__absb(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absb = self._arrays[array_handle]
        else:
            absb = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__absb)
            self._arrays[array_handle] = absb
        return absb
    
    @absb.setter
    def absb(self, absb):
        self.absb[...] = absb
    
    @property
    def selfref(self):
        """
        Element selfref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__selfref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            selfref = self._arrays[array_handle]
        else:
            selfref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__selfref)
            self._arrays[array_handle] = selfref
        return selfref
    
    @selfref.setter
    def selfref(self, selfref):
        self.selfref[...] = selfref
    
    @property
    def forref(self):
        """
        Element forref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 71
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__forref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            forref = self._arrays[array_handle]
        else:
            forref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__forref)
            self._arrays[array_handle] = forref
        return forref
    
    @forref.setter
    def forref(self, forref):
        self.forref[...] = forref
    
    @property
    def sfluxref(self):
        """
        Element sfluxref ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 72
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__sfluxref(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            sfluxref = self._arrays[array_handle]
        else:
            sfluxref = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__sfluxref)
            self._arrays[array_handle] = sfluxref
        return sfluxref
    
    @sfluxref.setter
    def sfluxref(self, sfluxref):
        self.sfluxref[...] = sfluxref
    
    @property
    def irradnce(self):
        """
        Element irradnce ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 73
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__irradnce(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            irradnce = self._arrays[array_handle]
        else:
            irradnce = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__irradnce)
            self._arrays[array_handle] = irradnce
        return irradnce
    
    @irradnce.setter
    def irradnce(self, irradnce):
        self.irradnce[...] = irradnce
    
    @property
    def facbrght(self):
        """
        Element facbrght ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 74
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__facbrght(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            facbrght = self._arrays[array_handle]
        else:
            facbrght = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__facbrght)
            self._arrays[array_handle] = facbrght
        return facbrght
    
    @facbrght.setter
    def facbrght(self, facbrght):
        self.facbrght[...] = facbrght
    
    @property
    def snsptdrk(self):
        """
        Element snsptdrk ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 74
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__snsptdrk(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            snsptdrk = self._arrays[array_handle]
        else:
            snsptdrk = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__snsptdrk)
            self._arrays[array_handle] = snsptdrk
        return snsptdrk
    
    @snsptdrk.setter
    def snsptdrk(self, snsptdrk):
        self.snsptdrk[...] = snsptdrk
    
    @property
    def absh2o(self):
        """
        Element absh2o ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 75
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__absh2o(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absh2o = self._arrays[array_handle]
        else:
            absh2o = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__absh2o)
            self._arrays[array_handle] = absh2o
        return absh2o
    
    @absh2o.setter
    def absh2o(self, absh2o):
        self.absh2o[...] = absh2o
    
    @property
    def absco2(self):
        """
        Element absco2 ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg29.f90 line 75
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_kg29__array__absco2(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            absco2 = self._arrays[array_handle]
        else:
            absco2 = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_kg29__array__absco2)
            self._arrays[array_handle] = absco2
        return absco2
    
    @absco2.setter
    def absco2(self, absco2):
        self.absco2[...] = absco2
    
    def __str__(self):
        ret = ['<rrsw_kg29>{\n']
        ret.append('    no29 : ')
        ret.append(repr(self.no29))
        ret.append(',\n    kao : ')
        ret.append(repr(self.kao))
        ret.append(',\n    kbo : ')
        ret.append(repr(self.kbo))
        ret.append(',\n    selfrefo : ')
        ret.append(repr(self.selfrefo))
        ret.append(',\n    forrefo : ')
        ret.append(repr(self.forrefo))
        ret.append(',\n    sfluxrefo : ')
        ret.append(repr(self.sfluxrefo))
        ret.append(',\n    irradnceo : ')
        ret.append(repr(self.irradnceo))
        ret.append(',\n    facbrghto : ')
        ret.append(repr(self.facbrghto))
        ret.append(',\n    snsptdrko : ')
        ret.append(repr(self.snsptdrko))
        ret.append(',\n    absh2oo : ')
        ret.append(repr(self.absh2oo))
        ret.append(',\n    absco2o : ')
        ret.append(repr(self.absco2o))
        ret.append(',\n    rayl : ')
        ret.append(repr(self.rayl))
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
        ret.append(',\n    sfluxref : ')
        ret.append(repr(self.sfluxref))
        ret.append(',\n    irradnce : ')
        ret.append(repr(self.irradnce))
        ret.append(',\n    facbrght : ')
        ret.append(repr(self.facbrght))
        ret.append(',\n    snsptdrk : ')
        ret.append(repr(self.snsptdrk))
        ret.append(',\n    absh2o : ')
        ret.append(repr(self.absh2o))
        ret.append(',\n    absco2 : ')
        ret.append(repr(self.absco2))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_kg29 = Rrsw_Kg29()

class Rrsw_Ncpar(f90wrap.runtime.FortranModule):
    """
    Module rrsw_ncpar
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 lines 1-103
    
    """
    @staticmethod
    def getabsorberindex(absorbername):
        """
        absorberindex = getabsorberindex(absorbername)
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 lines 87-103
        
        Parameters
        ----------
        absorbername : str
        
        Returns
        -------
        absorberindex : int
        
        """
        absorberindex = _rrtmg_sw.f90wrap_getabsorberindex(absorbername=absorbername)
        return absorberindex
    
    @property
    def cpdair(self):
        """
        Element cpdair ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 7
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__cpdair()
    
    @property
    def status(self):
        """
        Element status ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 11
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_ncpar__array__status(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            status = self._arrays[array_handle]
        else:
            status = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_ncpar__array__status)
            self._arrays[array_handle] = status
        return status
    
    @status.setter
    def status(self, status):
        self.status[...] = status
    
    @property
    def i(self):
        """
        Element i ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 12
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__i()
    
    @i.setter
    def i(self, i):
        _rrtmg_sw.f90wrap_rrsw_ncpar__set__i(i)
    
    @property
    def keylower(self):
        """
        Element keylower ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__keylower()
    
    @property
    def keyupper(self):
        """
        Element keyupper ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__keyupper()
    
    @property
    def tdiff(self):
        """
        Element tdiff ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__tdiff()
    
    @property
    def ps(self):
        """
        Element ps ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__ps()
    
    @property
    def plower(self):
        """
        Element plower ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__plower()
    
    @property
    def pupper(self):
        """
        Element pupper ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__pupper()
    
    @property
    def tself(self):
        """
        Element tself ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__tself()
    
    @property
    def tforeignlower(self):
        """
        Element tforeignlower ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__tforeignlower()
    
    @property
    def tforeignupper(self):
        """
        Element tforeignupper ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__tforeignupper()
    
    @property
    def pforeign(self):
        """
        Element pforeign ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__pforeign()
    
    @property
    def t(self):
        """
        Element t ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__t()
    
    @property
    def band(self):
        """
        Element band ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__band()
    
    @property
    def gpoint(self):
        """
        Element gpoint ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__gpoint()
    
    @property
    def gpointset(self):
        """
        Element gpointset ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 26
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__gpointset()
    
    @property
    def maxabsorbernamelength(self):
        """
        Element maxabsorbernamelength ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__maxabsorbernamelength()
    
    @property
    def absorber(self):
        """
        Element absorber ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__absorber()
    
    @property
    def maxkeyspeciesnamelength(self):
        """
        Element maxkeyspeciesnamelength ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__maxkeyspeciesnamelength()
    
    @property
    def maxkeyspeciesnames(self):
        """
        Element maxkeyspeciesnames ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 31
        
        """
        return _rrtmg_sw.f90wrap_rrsw_ncpar__get__maxkeyspeciesnames()
    
    @property
    def bandnums(self):
        """
        Element bandnums ftype=integer(kind=im) pytype=int
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 63
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_ncpar__array__bandnums(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            bandnums = self._arrays[array_handle]
        else:
            bandnums = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_ncpar__array__bandnums)
            self._arrays[array_handle] = bandnums
        return bandnums
    
    @bandnums.setter
    def bandnums(self, bandnums):
        self.bandnums[...] = bandnums
    
    @property
    def keyspecieslower(self):
        """
        Element keyspecieslower ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 66
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_ncpar__array__keyspecieslower(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            keyspecieslower = self._arrays[array_handle]
        else:
            keyspecieslower = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_ncpar__array__keyspecieslower)
            self._arrays[array_handle] = keyspecieslower
        return keyspecieslower
    
    @keyspecieslower.setter
    def keyspecieslower(self, keyspecieslower):
        self.keyspecieslower[...] = keyspecieslower
    
    @property
    def keyspeciesupper(self):
        """
        Element keyspeciesupper ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 68
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_ncpar__array__keyspeciesupper(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            keyspeciesupper = self._arrays[array_handle]
        else:
            keyspeciesupper = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_ncpar__array__keyspeciesupper)
            self._arrays[array_handle] = keyspeciesupper
        return keyspeciesupper
    
    @keyspeciesupper.setter
    def keyspeciesupper(self, keyspeciesupper):
        self.keyspeciesupper[...] = keyspeciesupper
    
    @property
    def tempdiffs(self):
        """
        Element tempdiffs ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 70
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_ncpar__array__tempdiffs(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            tempdiffs = self._arrays[array_handle]
        else:
            tempdiffs = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_ncpar__array__tempdiffs)
            self._arrays[array_handle] = tempdiffs
        return tempdiffs
    
    @tempdiffs.setter
    def tempdiffs(self, tempdiffs):
        self.tempdiffs[...] = tempdiffs
    
    @property
    def tempself(self):
        """
        Element tempself ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 73
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_ncpar__array__tempself(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            tempself = self._arrays[array_handle]
        else:
            tempself = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_ncpar__array__tempself)
            self._arrays[array_handle] = tempself
        return tempself
    
    @tempself.setter
    def tempself(self, tempself):
        self.tempself[...] = tempself
    
    @property
    def tempforeignlower(self):
        """
        Element tempforeignlower ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 75
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_ncpar__array__tempforeignlower(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            tempforeignlower = self._arrays[array_handle]
        else:
            tempforeignlower = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_ncpar__array__tempforeignlower)
            self._arrays[array_handle] = tempforeignlower
        return tempforeignlower
    
    @tempforeignlower.setter
    def tempforeignlower(self, tempforeignlower):
        self.tempforeignlower[...] = tempforeignlower
    
    @property
    def tempforeignupper(self):
        """
        Element tempforeignupper ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 77
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_ncpar__array__tempforeignupper(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            tempforeignupper = self._arrays[array_handle]
        else:
            tempforeignupper = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_ncpar__array__tempforeignupper)
            self._arrays[array_handle] = tempforeignupper
        return tempforeignupper
    
    @tempforeignupper.setter
    def tempforeignupper(self, tempforeignupper):
        self.tempforeignupper[...] = tempforeignupper
    
    @property
    def pressforeign(self):
        """
        Element pressforeign ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 79
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_ncpar__array__pressforeign(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            pressforeign = self._arrays[array_handle]
        else:
            pressforeign = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_ncpar__array__pressforeign)
            self._arrays[array_handle] = pressforeign
        return pressforeign
    
    @pressforeign.setter
    def pressforeign(self, pressforeign):
        self.pressforeign[...] = pressforeign
    
    @property
    def temp(self):
        """
        Element temp ftype=real(kind=rb) pytype=float
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90 line 83
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _rrtmg_sw.f90wrap_rrsw_ncpar__array__temp(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            temp = self._arrays[array_handle]
        else:
            temp = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _rrtmg_sw.f90wrap_rrsw_ncpar__array__temp)
            self._arrays[array_handle] = temp
        return temp
    
    @temp.setter
    def temp(self, temp):
        self.temp[...] = temp
    
    def __str__(self):
        ret = ['<rrsw_ncpar>{\n']
        ret.append('    cpdair : ')
        ret.append(repr(self.cpdair))
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
        ret.append(',\n    tforeignlower : ')
        ret.append(repr(self.tforeignlower))
        ret.append(',\n    tforeignupper : ')
        ret.append(repr(self.tforeignupper))
        ret.append(',\n    pforeign : ')
        ret.append(repr(self.pforeign))
        ret.append(',\n    t : ')
        ret.append(repr(self.t))
        ret.append(',\n    band : ')
        ret.append(repr(self.band))
        ret.append(',\n    gpoint : ')
        ret.append(repr(self.gpoint))
        ret.append(',\n    gpointset : ')
        ret.append(repr(self.gpointset))
        ret.append(',\n    maxabsorbernamelength : ')
        ret.append(repr(self.maxabsorbernamelength))
        ret.append(',\n    absorber : ')
        ret.append(repr(self.absorber))
        ret.append(',\n    maxkeyspeciesnamelength : ')
        ret.append(repr(self.maxkeyspeciesnamelength))
        ret.append(',\n    maxkeyspeciesnames : ')
        ret.append(repr(self.maxkeyspeciesnames))
        ret.append(',\n    bandnums : ')
        ret.append(repr(self.bandnums))
        ret.append(',\n    keyspecieslower : ')
        ret.append(repr(self.keyspecieslower))
        ret.append(',\n    keyspeciesupper : ')
        ret.append(repr(self.keyspeciesupper))
        ret.append(',\n    tempdiffs : ')
        ret.append(repr(self.tempdiffs))
        ret.append(',\n    tempself : ')
        ret.append(repr(self.tempself))
        ret.append(',\n    tempforeignlower : ')
        ret.append(repr(self.tempforeignlower))
        ret.append(',\n    tempforeignupper : ')
        ret.append(repr(self.tempforeignupper))
        ret.append(',\n    pressforeign : ')
        ret.append(repr(self.pressforeign))
        ret.append(',\n    temp : ')
        ret.append(repr(self.temp))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

rrsw_ncpar = Rrsw_Ncpar()

class Mcica_Subcol_Gen_Sw(f90wrap.runtime.FortranModule):
    """
    Module mcica_subcol_gen_sw
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/src/mcica_subcol_gen_sw.f90 lines 7-593
    
    """
    @staticmethod
    def mcica_subcol_sw(iplon, ncol, nlay, icld, permuteseed, irng, play, cldfrac, \
        ciwp, clwp, rei, rel, tauc, ssac, asmc, fsfc, cldfmcl, ciwpmcl, clwpmcl, \
        reicmcl, relqmcl, taucmcl, ssacmcl, asmcmcl, fsfcmcl):
        """
        mcica_subcol_sw(iplon, ncol, nlay, icld, permuteseed, irng, play, cldfrac, ciwp, \
            clwp, rei, rel, tauc, ssac, asmc, fsfc, cldfmcl, ciwpmcl, clwpmcl, reicmcl, \
            relqmcl, taucmcl, ssacmcl, asmcmcl, fsfcmcl)
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/mcica_subcol_gen_sw.f90 lines 69-179
        
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
        ssac : float array
        asmc : float array
        fsfc : float array
        cldfmcl : float array
        ciwpmcl : float array
        clwpmcl : float array
        reicmcl : float array
        relqmcl : float array
        taucmcl : float array
        ssacmcl : float array
        asmcmcl : float array
        fsfcmcl : float array
        
        """
        _rrtmg_sw.f90wrap_mcica_subcol_sw(iplon=iplon, ncol=ncol, nlay=nlay, icld=icld, \
            permuteseed=permuteseed, irng=irng, play=play, cldfrac=cldfrac, ciwp=ciwp, \
            clwp=clwp, rei=rei, rel=rel, tauc=tauc, ssac=ssac, asmc=asmc, fsfc=fsfc, \
            cldfmcl=cldfmcl, ciwpmcl=ciwpmcl, clwpmcl=clwpmcl, reicmcl=reicmcl, \
            relqmcl=relqmcl, taucmcl=taucmcl, ssacmcl=ssacmcl, asmcmcl=asmcmcl, \
            fsfcmcl=fsfcmcl)
    
    @staticmethod
    def generate_stochastic_clouds_sw(ncol, nlay, nsubcol, icld, irng, pmid, cld, \
        clwp, ciwp, tauc, ssac, asmc, fsfc, cld_stoch, clwp_stoch, ciwp_stoch, \
        tauc_stoch, ssac_stoch, asmc_stoch, fsfc_stoch, changeseed=None):
        """
        generate_stochastic_clouds_sw(ncol, nlay, nsubcol, icld, irng, pmid, cld, clwp, \
            ciwp, tauc, ssac, asmc, fsfc, cld_stoch, clwp_stoch, ciwp_stoch, tauc_stoch, \
            ssac_stoch, asmc_stoch, fsfc_stoch[, changeseed])
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/mcica_subcol_gen_sw.f90 lines 184-556
        
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
        ssac : float array
        asmc : float array
        fsfc : float array
        cld_stoch : float array
        clwp_stoch : float array
        ciwp_stoch : float array
        tauc_stoch : float array
        ssac_stoch : float array
        asmc_stoch : float array
        fsfc_stoch : float array
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
        _rrtmg_sw.f90wrap_generate_stochastic_clouds_sw(ncol=ncol, nlay=nlay, \
            nsubcol=nsubcol, icld=icld, irng=irng, pmid=pmid, cld=cld, clwp=clwp, \
            ciwp=ciwp, tauc=tauc, ssac=ssac, asmc=asmc, fsfc=fsfc, cld_stoch=cld_stoch, \
            clwp_stoch=clwp_stoch, ciwp_stoch=ciwp_stoch, tauc_stoch=tauc_stoch, \
            ssac_stoch=ssac_stoch, asmc_stoch=asmc_stoch, fsfc_stoch=fsfc_stoch, \
            changeseed=changeseed)
    
    @staticmethod
    def kissvec(seed1, seed2, seed3, seed4, ran_arr):
        """
        kissvec(seed1, seed2, seed3, seed4, ran_arr)
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/mcica_subcol_gen_sw.f90 lines 559-591
        
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
        _rrtmg_sw.f90wrap_kissvec(seed1=seed1, seed2=seed2, seed3=seed3, seed4=seed4, \
            ran_arr=ran_arr)
    
    _dt_array_initialisers = []
    

mcica_subcol_gen_sw = Mcica_Subcol_Gen_Sw()

class Rrtmg_Sw_Init(f90wrap.runtime.FortranModule):
    """
    Module rrtmg_sw_init
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 6-3515
    
    """
    @staticmethod
    def rrtmg_sw_ini(cpdair):
        """
        rrtmg_sw_ini(cpdair)
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 47-174
        
        Parameters
        ----------
        cpdair : float
        
        """
        _rrtmg_sw.f90wrap_rrtmg_sw_ini(cpdair=cpdair)
    
    @staticmethod
    def swdatinit(cpdair):
        """
        swdatinit(cpdair)
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 177-261
        
        Parameters
        ----------
        cpdair : float
        
        """
        _rrtmg_sw.f90wrap_swdatinit(cpdair=cpdair)
    
    @staticmethod
    def swcmbdat():
        """
        swcmbdat()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 264-387
        
        
        """
        _rrtmg_sw.f90wrap_swcmbdat()
    
    @staticmethod
    def swaerpr():
        """
        swaerpr()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 390-490
        
        
        """
        _rrtmg_sw.f90wrap_swaerpr()
    
    @staticmethod
    def cmbgb16s():
        """
        cmbgb16s()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 493-595
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb16s()
    
    @staticmethod
    def cmbgb17():
        """
        cmbgb17()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 598-691
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb17()
    
    @staticmethod
    def cmbgb18():
        """
        cmbgb18()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 694-785
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb18()
    
    @staticmethod
    def cmbgb19():
        """
        cmbgb19()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 788-879
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb19()
    
    @staticmethod
    def cmbgb20():
        """
        cmbgb20()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 882-969
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb20()
    
    @staticmethod
    def cmbgb21():
        """
        cmbgb21()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 972-1065
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb21()
    
    @staticmethod
    def cmbgb22():
        """
        cmbgb22()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 1068-1159
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb22()
    
    @staticmethod
    def cmbgb23():
        """
        cmbgb23()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 1162-1238
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb23()
    
    @staticmethod
    def cmbgb24():
        """
        cmbgb24()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 1241-1353
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb24()
    
    @staticmethod
    def cmbgb25():
        """
        cmbgb25()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 1356-1416
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb25()
    
    @staticmethod
    def cmbgb26():
        """
        cmbgb26()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 1419-1457
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb26()
    
    @staticmethod
    def cmbgb27():
        """
        cmbgb27()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 1460-1523
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb27()
    
    @staticmethod
    def cmbgb28():
        """
        cmbgb28()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 1526-1595
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb28()
    
    @staticmethod
    def cmbgb29():
        """
        cmbgb29()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 1598-1690
        
        
        -----------------------------------------------------------------------
        """
        _rrtmg_sw.f90wrap_cmbgb29()
    
    @staticmethod
    def swcldpr():
        """
        swcldpr()
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90 lines 1693-3515
        
        
        -----------------------------------------------------------------------
         Explanation of the method for each value of INFLAG.  A value of
          0 for INFLAG do not distingish being liquid and ice clouds.
          INFLAG = 2 does distinguish between liquid and ice clouds, and
            requires further user input to specify the method to be used to
            compute the aborption due to each.
          INFLAG = 0: For each cloudy layer, the cloud fraction, the cloud optical
            depth, the cloud single-scattering albedo, and the
            moments of the phase function (0:NSTREAM).  Note
            that these values are delta-m scaled within this
            subroutine.
          INFLAG = 2:  For each cloudy layer, the cloud fraction, cloud
            water path (g/m2), and cloud ice fraction are input.
          ICEFLAG = 2:  The ice effective radius (microns) is input and the
            optical properties due to ice clouds are computed from
            the optical properties stored in the RT code, STREAMER v3.0
            (Reference: Key. J., Streamer User's Guide, Cooperative
            Institute for Meteorological Satellite Studies, 2001, 96 pp.).
            Valid range of values for re are between 5.0 and
            131.0 micron.
            This version uses Ebert and Curry, JGR, (1992) method for
            ice particles larger than 131.0 microns.
          ICEFLAG = 3:  The ice generalized effective size (dge) is input
            and the optical depths, single-scattering albedo,
            and phase function moments are calculated as in
            Q. Fu, J. Climate, (1996). Q. Fu provided high resolution
            tables which were appropriately averaged for the
            bands in RRTM_SW.  Linear interpolation is used to
            get the coefficients from the stored tables.
            Valid range of values for dge are between 5.0 and
            140.0 micron.
            This version uses Ebert and Curry, JGR, (1992) method for
            ice particles larger than 140.0 microns.
          LIQFLAG = 1:  The water droplet effective radius (microns) is input
            and the optical depths due to water clouds are computed
            as in Hu and Stamnes, J., Clim., 6, 728-742, (1993) with
            modified coefficients derived from Mie scattering calculations.
            The values for absorption coefficients appropriate for
            the spectral bands in RRTM/RRTMG have been obtained for a
            range of effective radii by an averaging procedure
            based on the work of J. Pinto (private communication).
            Linear interpolation is used to get the absorption
            coefficients for the input effective radius.
             ------------------------------------------------------------------
         Everything below is for INFLAG = 2.
         Coefficients for Ebert and Curry method
        """
        _rrtmg_sw.f90wrap_swcldpr()
    
    _dt_array_initialisers = []
    

rrtmg_sw_init = Rrtmg_Sw_Init()

class Rrtmg_Sw_Rad(f90wrap.runtime.FortranModule):
    """
    Module rrtmg_sw_rad
    
    
    Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_rad.f90 lines 7-1570
    
    """
    @staticmethod
    def rrtmg_sw(ncol, nlay, icld, iaer, play, plev, tlay, tlev, tsfc, h2ovmr, \
        o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, asdir, asdif, aldir, aldif, coszen, \
        adjes, dyofyr, scon, isolvar, inflgsw, iceflgsw, liqflgsw, cldfmcl, taucmcl, \
        ssacmcl, asmcmcl, fsfcmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, tauaer, \
        ssaaer, asmaer, ecaer, swuflx, swdflx, swhr, swuflxc, swdflxc, swhrc, \
        bndsolvar=None, indsolvar=None, solcycfrac=None):
        """
        rrtmg_sw(ncol, nlay, icld, iaer, play, plev, tlay, tlev, tsfc, h2ovmr, o3vmr, \
            co2vmr, ch4vmr, n2ovmr, o2vmr, asdir, asdif, aldir, aldif, coszen, adjes, \
            dyofyr, scon, isolvar, inflgsw, iceflgsw, liqflgsw, cldfmcl, taucmcl, \
            ssacmcl, asmcmcl, fsfcmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, tauaer, \
            ssaaer, asmaer, ecaer, swuflx, swdflx, swhr, swuflxc, swdflxc, swhrc[, \
            bndsolvar, indsolvar, solcycfrac])
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_rad.f90 lines 110-843
        
        Parameters
        ----------
        ncol : int
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
        asdir : float array
        asdif : float array
        aldir : float array
        aldif : float array
        coszen : float array
        adjes : float
        dyofyr : int
        scon : float
        isolvar : int
        inflgsw : int
        iceflgsw : int
        liqflgsw : int
        cldfmcl : float array
        taucmcl : float array
        ssacmcl : float array
        asmcmcl : float array
        fsfcmcl : float array
        ciwpmcl : float array
        clwpmcl : float array
        reicmcl : float array
        relqmcl : float array
        tauaer : float array
        ssaaer : float array
        asmaer : float array
        ecaer : float array
        swuflx : float array
        swdflx : float array
        swhr : float array
        swuflxc : float array
        swdflxc : float array
        swhrc : float array
        bndsolvar : float array
        indsolvar : float array
        solcycfrac : float
        
        """
        _rrtmg_sw.f90wrap_rrtmg_sw(ncol=ncol, nlay=nlay, icld=icld, iaer=iaer, \
            play=play, plev=plev, tlay=tlay, tlev=tlev, tsfc=tsfc, h2ovmr=h2ovmr, \
            o3vmr=o3vmr, co2vmr=co2vmr, ch4vmr=ch4vmr, n2ovmr=n2ovmr, o2vmr=o2vmr, \
            asdir=asdir, asdif=asdif, aldir=aldir, aldif=aldif, coszen=coszen, \
            adjes=adjes, dyofyr=dyofyr, scon=scon, isolvar=isolvar, inflgsw=inflgsw, \
            iceflgsw=iceflgsw, liqflgsw=liqflgsw, cldfmcl=cldfmcl, taucmcl=taucmcl, \
            ssacmcl=ssacmcl, asmcmcl=asmcmcl, fsfcmcl=fsfcmcl, ciwpmcl=ciwpmcl, \
            clwpmcl=clwpmcl, reicmcl=reicmcl, relqmcl=relqmcl, tauaer=tauaer, \
            ssaaer=ssaaer, asmaer=asmaer, ecaer=ecaer, swuflx=swuflx, swdflx=swdflx, \
            swhr=swhr, swuflxc=swuflxc, swdflxc=swdflxc, swhrc=swhrc, \
            bndsolvar=bndsolvar, indsolvar=indsolvar, solcycfrac=solcycfrac)
    
    @staticmethod
    def earth_sun(idn):
        """
        earth_sun = earth_sun(idn)
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_rad.f90 lines 846-870
        
        Parameters
        ----------
        idn : int
        
        Returns
        -------
        earth_sun : float
        
        """
        earth_sun = _rrtmg_sw.f90wrap_earth_sun(idn=idn)
        return earth_sun
    
    @staticmethod
    def inatm_sw(iplon, nlay, icld, iaer, play, plev, tlay, tlev, tsfc, h2ovmr, \
        o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, adjes, dyofyr, scon, isolvar, inflgsw, \
        iceflgsw, liqflgsw, cldfmcl, taucmcl, ssacmcl, asmcmcl, fsfcmcl, ciwpmcl, \
        clwpmcl, reicmcl, relqmcl, tauaer, ssaaer, asmaer, pavel, pz, pdp, tavel, \
        tz, coldry, wkl, adjflux, cldfmc, taucmc, ssacmc, asmcmc, fsfcmc, ciwpmc, \
        clwpmc, reicmc, relqmc, taua, ssaa, asma, svar_f_bnd, svar_s_bnd, \
        svar_i_bnd, bndsolvar=None, indsolvar=None, solcycfrac=None):
        """
        nlayers, tbound, inflag, iceflag, liqflag, svar_f, svar_s, svar_i = \
            inatm_sw(iplon, nlay, icld, iaer, play, plev, tlay, tlev, tsfc, h2ovmr, \
            o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, adjes, dyofyr, scon, isolvar, inflgsw, \
            iceflgsw, liqflgsw, cldfmcl, taucmcl, ssacmcl, asmcmcl, fsfcmcl, ciwpmcl, \
            clwpmcl, reicmcl, relqmcl, tauaer, ssaaer, asmaer, pavel, pz, pdp, tavel, \
            tz, coldry, wkl, adjflux, cldfmc, taucmc, ssacmc, asmcmc, fsfcmc, ciwpmc, \
            clwpmc, reicmc, relqmc, taua, ssaa, asma, svar_f_bnd, svar_s_bnd, \
            svar_i_bnd[, bndsolvar, indsolvar, solcycfrac])
        
        
        Defined at rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_rad.f90 lines 885-1570
        
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
        adjes : float
        dyofyr : int
        scon : float
        isolvar : int
        inflgsw : int
        iceflgsw : int
        liqflgsw : int
        cldfmcl : float array
        taucmcl : float array
        ssacmcl : float array
        asmcmcl : float array
        fsfcmcl : float array
        ciwpmcl : float array
        clwpmcl : float array
        reicmcl : float array
        relqmcl : float array
        tauaer : float array
        ssaaer : float array
        asmaer : float array
        pavel : float array
        pz : float array
        pdp : float array
        tavel : float array
        tz : float array
        coldry : float array
        wkl : float array
        adjflux : float array
        cldfmc : float array
        taucmc : float array
        ssacmc : float array
        asmcmc : float array
        fsfcmc : float array
        ciwpmc : float array
        clwpmc : float array
        reicmc : float array
        relqmc : float array
        taua : float array
        ssaa : float array
        asma : float array
        svar_f_bnd : float array
        svar_s_bnd : float array
        svar_i_bnd : float array
        bndsolvar : float array
        indsolvar : float array
        solcycfrac : float
        
        Returns
        -------
        nlayers : int
        tbound : float
        inflag : int
        iceflag : int
        liqflag : int
        svar_f : float
        svar_s : float
        svar_i : float
        
        """
        nlayers, tbound, inflag, iceflag, liqflag, svar_f, svar_s, svar_i = \
            _rrtmg_sw.f90wrap_inatm_sw(iplon=iplon, nlay=nlay, icld=icld, iaer=iaer, \
            play=play, plev=plev, tlay=tlay, tlev=tlev, tsfc=tsfc, h2ovmr=h2ovmr, \
            o3vmr=o3vmr, co2vmr=co2vmr, ch4vmr=ch4vmr, n2ovmr=n2ovmr, o2vmr=o2vmr, \
            adjes=adjes, dyofyr=dyofyr, scon=scon, isolvar=isolvar, inflgsw=inflgsw, \
            iceflgsw=iceflgsw, liqflgsw=liqflgsw, cldfmcl=cldfmcl, taucmcl=taucmcl, \
            ssacmcl=ssacmcl, asmcmcl=asmcmcl, fsfcmcl=fsfcmcl, ciwpmcl=ciwpmcl, \
            clwpmcl=clwpmcl, reicmcl=reicmcl, relqmcl=relqmcl, tauaer=tauaer, \
            ssaaer=ssaaer, asmaer=asmaer, pavel=pavel, pz=pz, pdp=pdp, tavel=tavel, \
            tz=tz, coldry=coldry, wkl=wkl, adjflux=adjflux, cldfmc=cldfmc, \
            taucmc=taucmc, ssacmc=ssacmc, asmcmc=asmcmc, fsfcmc=fsfcmc, ciwpmc=ciwpmc, \
            clwpmc=clwpmc, reicmc=reicmc, relqmc=relqmc, taua=taua, ssaa=ssaa, \
            asma=asma, svar_f_bnd=svar_f_bnd, svar_s_bnd=svar_s_bnd, \
            svar_i_bnd=svar_i_bnd, bndsolvar=bndsolvar, indsolvar=indsolvar, \
            solcycfrac=solcycfrac)
        return nlayers, tbound, inflag, iceflag, liqflag, svar_f, svar_s, svar_i
    
    _dt_array_initialisers = []
    

rrtmg_sw_rad = Rrtmg_Sw_Rad()

