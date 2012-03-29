from glob import glob
import os
def mech(inpath):
    spc_paths = glob(os.path.join(inpath, '*_SPC.EXT'))
    spc_files = dict([(os.path.basename(p)[:2], p) for p in spc_paths])
    spc_data = {}
    for k in spc_files.keys():
        path = spc_files[k]
        spc_dat = dict([(key, eval(v)) for key, v in [l.strip().replace('/','').replace(' ', '').replace(',', '').split("'")[1:3] for l in file(path).read().split('\n') if "'" in l]])
        spc_data[k] =  spc_dat
    return spc_data

def mechinc(indict):
    units = []
    names = []
    out = ""
    for spc_typ, spc_dat in indict.iteritems():
        if spc_typ == 'AE':
            unit = "micrograms / m**3".ljust(16)
        else:
            unit = 'ppmV'.ljust(16)
        for spcn, spcw in spc_dat.iteritems():
            units.append('# / m**3' if spcn[:3] == 'NUM' and spc_typ == 'AE' else unit)
            names.append(spcn.ljust(16))
    out += "      INTEGER, PARAMETER :: NSPC_CMAQ = %d\n" % len(names)
    out += "      CHARACTER( 16 )  :: CMAQ_SNAME( NSPC_CMAQ ) = ( /\n"
    out += ",\n".join(["     &                                 '%s'" % s for s in names])
    out += "\n     & /)\n"
    out += "\n"
    out += "      CHARACTER( 16 )  :: CMAQ_LNAME( NSPC_CMAQ ) = ( /\n"
    out += ",\n".join(["     &                                 '%s'" % s for s in names])
    out += "\n     & /)\n"
    out += "\n"
    out += "      CHARACTER( 16 )  :: CMAQ_UNITS( NSPC_CMAQ ) = ( /\n"
    out += ",\n".join(["     &                                 '%s'" % s for s in units])
    out += "\n     & /)\n"
    out += "\n"
    out += "\n".join(["      INTEGER  ::  C_%-16s =   %d" % (s.strip(), si + 1) for si, s in enumerate(names)])
    out += "\n"
    return out
        

if __name__ == "__main__":
    x = mech('testdata')
