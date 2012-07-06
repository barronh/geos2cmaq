from both import geos
from profile import profile
from warnings import warn
import re

var = re.compile('([^ .,+=\-*/%0-9)(][^ .,+=\-*/%)(]*)')
def trymap(spc_data, mappath, mapo):
    outf = file(mappath, 'w')
    for spc_typ, spc_dat in spc_data.iteritems():
        for spcn, spcw in spc_dat.iteritems():
            if mapo.has_tracer(spcn) or mapo.has_cspec(spcn):
                outf.write('%s, %s\n' % (spcn, spcn))
            else:
                outf.write('%s,\n' % (spcn, ))
    outf.close()

def mapping(mappath):
    import csv
    rows = csv.reader(file(mappath))
    rows.next() # ignore header
    rows = [[c.strip() for c in row] for row in rows if row[0][0] != '#'] # Ignore comments
    rows = [row for row in rows if len(row) > 1] # Ignore unmapped
    rows = [row for row in rows if row[1].upper() not in ('PROFILE', '')] # Ignore profiles
    return rows

def map(spc_data, mappath, mapo, profo):
    intext = mapping(mappath)
    # old mapping reader - [kv for kv in [kv.split(',') for kv in file(mappath).read().strip().split('\n')[1:] if kv[0] != '#'] if len(kv) > 1 and kv[-1].strip() != '']
    ks = set([k for k, v in intext])
    mechspcs = set(reduce(list.__add__, [spc_dat.keys() for spc_dat in spc_data.values()]))
    mapped = list(ks)
    mapped.sort()
    nmapped = list(mechspcs.difference(ks))
    nmapped.sort()
    print 'Mapped MECH:', ' '.join(mapped)
    print
    print 'Unmapped MECH:', ' '.join(nmapped)
    print
    print 'Unmapped species might be available:'
    print ' - in TRACER data:', ' '.join([spc for spc in nmapped if mapo.has_tracer(spc)])
    print ' - in CSPEC  data:', ' '.join([spc for spc in nmapped if mapo.has_cspec(spc)])
    print ' - in PROFILE data:', ' '.join([spc for spc in nmapped if spc in profo])
    print 
    outs = []
    nprof = 0
    for k, v in intext:
        if k in mechspcs:
            start = '      BC2( 1:N, 1:L, C_%s )' % k
            if k in ks:
                start += '= (%s)'
                ks.remove(k)
            else:
                start += '= %s +\n     & (%%s)' % start.strip()
            if k in spc_data['AE']:
                mapo.aero(True)
            else:
                mapo.aero(False)
            try:
                out = start % (var.sub(r'%(\1)s', v) % mapo)
            except KeyError, e1:
                try:
                    out = start % (var.sub(r'%(\1)s', v) % profo)
                    nprof += 1
                except KeyError, e2:
                    raise KeyError('Variable not found in either GEOS-Chem or Profile data\n\n' + str(e1) + '\n' + str(e2))
        else:
            warn("Skipping %s; not in mech" % k)

        
        outs.append(out)
    mapo.check()
    return '\n'.join(outs), nprof

if __name__ == '__main__':
    from both import geos
    from profile import profile
    go = geos('testdata/tracerinfo.dat',
                  'testdata/smv2.log')
    po = geos('testdata/profile.dat')
    from mech import mechext as mech
    map(mech('testdata'), 'mapping/saprc07t.csv', go, po)
