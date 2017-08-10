from matplotlib.mlab import csv2rec
from io import StringIO
import numpy as np

class profile(object):
    def __init__(self, path):
        lines = open(path).read().split('\n')
        header = lines[3].split()
        nlay, nspc = list(map(int, header[:2]))
        sigmas = list(map(float, header[2:]))
        nsigmas = len(sigmas)
        date, time = list(map(int, lines[4].split()))
        starts =  [5 + i + i * nspc for i in range(4)]
        ends = [s + 1 + nspc for s in starts]
        keys = [lines[s].strip().lower() for s in starts]
        fieldnames = ('name',) + tuple(['s%f' % i for i in sigmas])
        self.data = dict([(k, csv2rec(StringIO(u'\n'.join(lines[s+1:e])), delimiter = ' ', names = fieldnames, converterd = dict(names = lambda x: str(x).strip()))) for k, s, e in zip(keys, starts, ends)])
        self._profile_spcs = np.char.strip(self.data[keys[0]]['name'])
        data_type = self.data[keys[0]].dtype
        data_shape =  self.data[keys[0]].shape
        ks = keys[1:]
        for k in ks:
            try:
                assert((np.char.strip(self.data[k]['name']) == self._profile_spcs).all())
                assert(self.data[k].dtype == data_type)
                assert(self.data[k].dtype == data_type)
            except AssertionError:
                raise IOError('File is corrupt or inconsistent')
            
        self._prof_spc = ['NO2', 'NO', 'O3P', 'O3', 'NO3', 'N2O5', 'HNO3', 'O1D', 'HO', 'HONO', 'HO2', 'CO', 'HNO4', 'H2O2', 'SO2', 'SULF', 'MO2', 'HCHO', 'OP1', 'OP2', 'ONIT', 'KET', 'ACO3', 'PAN', 'PAA', 'ORA2', 'TPAN', 'ALD', 'ORA1', 'GLY', 'MGLY', 'CSL', 'MACR', 'MVK', 'ISOPROD', 'DCB', 'OL2', 'ISO', 'TERP', 'ETH', 'HC3', 'HC3', 'HC5', 'HC8', 'TOL', 'XYL', 'XYL', 'XYL', 'OLT', 'OLI', 'BENZENE', 'HG', 'HGIIGAS', 'CO2']
        self._prof_dict = dict([(k, []) for k in self._prof_spc])
    
    def __missing__(self, gkey):
        if gkey in self._profile_spcs:
            return self.get_profile(gkey)
        else:
            raise KeyError('%s not in profile: %s' % (gkey, self._profile_spcs))
    
    def __contains__(self, key):
        return key in self._profile_spcs.tolist()
    
    def __getitem__(self, key):
        return self.__missing__(key)
        
    def get_profile(self, key):
        out = '(BC1_PF_VERT( 1:N, 1:L, PF_%s ))' % key
        return out

    def profile_info(self):
        out = "       INTEGER, PARAMETER :: NSPC_PF = %d\n" % len(self._profile_spcs) + \
              "       CHARACTER( 16 )  :: PF_SPNAME( NSPC_PF ) = ( /\n"
        ids = []
        names = []
        for pi, spcname in enumerate(self._profile_spcs):
            names.append("     &                          '%s'" % spcname.ljust(16))
            ids.append("      INTEGER  ::  PF_%-16s = %4d" % (spcname, pi + 1))
        out += ",\n".join(names)
        out += "\n     & /)\n\n"
        out += '\n'.join(ids)
        out += '\n'
        return out

if __name__ == '__main__':
    po = profile('testdata/profile.dat')
