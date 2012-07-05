from smv2 import get_cspec
from tracerinfo import get_tracers
from collections import defaultdict
class geos(defaultdict):
    def __init__(self, tracerinfo, smvlog = None):
        self.tracer_data = get_tracers(tracerinfo)
        keys = self.tracer_data.keys()
        keys.sort()
        self._tracer_spcs = [self.tracer_data[k]['NAME'] for k in keys]
        self._tracer_usage = dict([(t,0) for t in self._tracer_spcs])
        self._tracer_usage = dict([(t,0) for t in self._tracer_spcs])
        self._cspec_spcs = []
        self._cspec_spcs += get_cspec(smvlog)
        self._cspec_usage = dict([(t,0) for t in self._cspec_spcs])
        self._aero = False
    def tracer_info(self):
        out = "       INTEGER, PARAMETER :: NSPC_GT = %d\n" % len(self._tracer_spcs) + \
              "       CHARACTER( 16 )  :: GT_SPNAME( NSPC_GT ) = ( /\n"
        ids = []
        names = []
        for ti, tracer in enumerate(self._tracer_spcs):
            names.append("     &                          '%s'" % tracer.ljust(16))
            ids.append("      INTEGER  ::  GT_%-16s = %4d" % (tracer, ti + 1))
        out += ",\n".join(names)
        out += "\n     & /)\n\n"
        out += '\n'.join(ids)
        out += '\n'
        return out
    def cspec_info(self):
        out = "       INTEGER, PARAMETER :: NSPC_GS = %d\n" % len(self._cspec_spcs) + \
              "       CHARACTER( 16 )  :: GS_SPNAME( NSPC_GS ) = ( /\n"
        names = []
        ids = []
        for ti, tracer in enumerate(self._cspec_spcs):
            names.append("     &                          '%s'" % tracer.ljust(16))
            ids.append("      INTEGER  ::  GS_%-16s =   %d" % (tracer, ti + 1))
        out += ",\n".join(names)
        out += "\n     & /)\n\n"
        out += '\n'.join(ids)
        out += '\n'
        return out

    def has_tracer(self, key):
        return key in self._tracer_spcs
    def has_cspec(self, key):
        return key in self._cspec_spcs

    def __missing__(self, gkey):
        if gkey in self._tracer_spcs:
            self._tracer_usage[gkey] += 1
            return self.get_tracer(gkey)
        elif gkey in self._cspec_spcs:
            self._cspec_usage[gkey] += 1
            return self.get_cspec(gkey)
        else:
            raise KeyError('%s not in tracers or cspec\n\ttracer %s\n\tcspec: %s' % (gkey, self._tracer_spcs, self._cspec_spcs))
    def get_tracer(self, key):
        out = '(BC1_GT_VERT( 1:N, 1:L, GT_%s ) * 1e6)' % key
        tracer_id = self._tracer_spcs.index(key) + 1
        tracer_dat = self.tracer_data[tracer_id]
        if tracer_dat['C'] > 1 and tracer_dat['MOLWT'] == .012:
            out = '(%s / %d)' % (out, tracer_dat['C'])
        if self._aero:
            out = '(%s * %s / UGMTOPPM(1:N, 1:L))' % (out, tracer_dat['MOLWT'] * 1000.)
        return out
    def get_cspec(self, key):
        out = '(BC1_GS_VERT( 1:N, 1:L, GS_%s ) * MOLTOPPM( 1:N, 1:L))' % key
        return out
    def check(self):
        uts = set([t for t, c in self._tracer_usage.iteritems() if c > 0])
        ts = set([t for t, c in self._tracer_usage.iteritems() if c < 1])
        ucs = set([t for t, c in self._cspec_usage.iteritems() if c > 0])
        cs = set([t for t, c in self._cspec_usage.iteritems() if c < 1])
        cs = set(cs).difference(self._tracer_spcs)
        uts =list(uts); uts.sort()
        ts =list(ts); ts.sort()
        ucs =list(ucs); ucs.sort()
        cs =list(cs); cs.sort()
        print 'Used Tracers: %s' % (' '.join(uts))
        print
        print 'Unused Tracers: %s' % (' '.join(ts))
        print
        print 'Used CSPEC: %s' % (' '.join(ucs))
        print
        print 'Unused CSPEC: %s' % (' '.join(cs))
        print
    def aero(self, aero):
        self._aero = aero




        
if __name__ == '__main__':
    import unittest
    class TestGeos(unittest.TestCase):
        def __init__(self, *args, **kwds):
            pass
        def run(self):
            pass
    go = geos('/project/inf15w/bar/geos-chem/baseict/tracerinfo.dat',
              '/project/inf15w/bar/geos-chem/baseict/smv2.log')
