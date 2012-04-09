from both import geos
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

def map(spc_data, mappath, mapo):
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
    outs = []
    prof_ids = []
    prof_maps = []
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
            except KeyError:
                if v.strip()[:8] == 'PROFILE_':
                    PROFKEY = '_'.join(v.split('_')[1:])
                    prof_ids.append("       INTEGER :: P_%-16s = INDEX1( '%-16s', NSPC_PROF, PROF_SP_NAME )" % (k, PROFKEY.strip()))
                    prof_maps.append("       BC3P( 1:N, 1:L, C_%-16s) = BC2P( 1:N, 1:L, P_%-16s)" % (k, k))
                else:
                    raise
        else:
            warn("Skipping %s; not in mech" % k)

        
        outs.append(out)
    prof_out = '\n\n'.join(['\n'.join(prof_ids), '\n'.join(prof_maps)])
    mapo.check()
    return '\n'.join(outs), prof_out, len(prof_ids)

if __name__ == '__main__':
    from both import geos
    go = geos('testdata/tracerinfo.dat',
                  'testdata/smv2.log')
    from mech import mechext as mech
    map(mech('testdata'), 'mapping/saprc07t.csv', go)
