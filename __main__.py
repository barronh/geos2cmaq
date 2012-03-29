import optparse
import os
usage = "usage: %prog [options] outpath"
parser = optparse.OptionParser(usage=usage)
parser.add_option("-t", dest = "tracerinfo", help = "path to tracerinfo.dat (defaults to SOA from v9-01-01)", metavar="TRACERINFO", default = None)
parser.add_option("-s", dest = "smvlog", help = "path to smv2.log (defaults to SOA from v9-01-01)", metavar="SMV", default = None)
parser.add_option("-m", dest = "mechpath", help = "path to mechanism include files (e.g., mechpath*.EXT; defaults to cb05cl_ae6_aq)", metavar="MECHPATH", default = None)
parser.add_option("-c", dest = "conversion", help = "path to converstion (i.e., mapping file)", metavar="CONV", default = None)
options, args = parser.parse_args()

mapopt = dict([(o.dest, getattr(options, o.dest)) for o in parser.option_list[1:]])

convpath = options.conversion or os.path.join(os.path.dirname(__file__), 'mapping', 'saprc07t.csv')
tracerpath = options.tracerinfo or os.path.join(os.path.dirname(__file__), 'testdata', 'tracerinfo.dat')
smvpath = options.smvlog or os.path.join(os.path.dirname(__file__), 'testdata', 'smv2.log')
mechpath = options.mechpath or os.path.join(os.path.dirname(__file__), 'testdata')
if len(args) < 1:
    parser.print_help()
    exit()
else:
    out = args[0]
from both import geos
from mech import mech, mechinc
from map import map, trymap
go = geos(tracerpath,
          smvpath)
if not os.path.exists(convpath):
    if 'Y' == raw_input("Conversion path does not exist; type Y to create it or any other key to abort"):
        trymap(mech(mechpath), convpath, go)
    else:
        exit()
mech_info = mechinc(mech(mechpath))
cspec_info = go.cspec_info()
tracer_info = go.tracer_info()
mappings, profiles, nprofs = map(mech(mechpath), convpath, go)
mech_info = ("      INTEGER, PARAMETER :: NSPC_DFLT = %d\n" % nprofs) + mech_info
if not os.path.exists(out):
    os.mkdir(out)
outdir = out
out = os.path.join(out, 'MAPPING')
file('%s.MECH' % out, 'w').write(mech_info)
file('%s.CSPEC' % out, 'w').write(cspec_info)
file('%s.TRACER' % out, 'w').write(tracer_info)
file('%s.MAP' % out, 'w').write(mappings)
file('%s.PROFILE' % out, 'w').write(profiles)
import shutil
from glob import glob
for f in glob(os.path.join(os.path.dirname(__file__), 'src', '*')):
    shutil.copy(f, outdir)

