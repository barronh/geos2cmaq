import optparse
import os
from glob import glob
import shutil
from sys import exit

usage = "usage: %prog [options] outpath"
parser = optparse.OptionParser(usage=usage)
archived_options = map(os.path.basename, glob(os.path.join(os.path.dirname(__file__), 'mechmap_archive', '*')))
parser.add_option("-a", dest = "archive", help = "Use archived mapping (one of: %s)" % ', '.join(archived_options), metavar="ARCHIVE", default = None)
parser.add_option("-t", dest = "tracerinfo", help = "path to tracerinfo.dat (defaults to SOA from v9-01-01)", metavar="TRACERINFO", default = None)
parser.add_option("-s", dest = "smvlog", help = "path to smv2.log (defaults to SOA from v9-01-01)", metavar="SMV", default = None)
parser.add_option("-m", dest = "mechpath", help = "path to mechanism include files (e.g., mechpath*.EXT; defaults to cb05cl_ae6_aq)", metavar="MECHPATH", default = None)
parser.add_option("-c", dest = "conversion", help = "path to converstion (i.e., mapping file)", metavar="CONV", default = None)
parser.add_option("-e", dest = "extfiles", help = "use ext files instead of namelist", metavar="EXT", default = False, action = 'store_true')
parser.add_option("-p", dest = "profiledat", help = "path to profile (e.g., CMAQ BCON input) (defaults profile.dat", metavar="PROFILEDAT", default = None)
options, args = parser.parse_args()

mapopt = dict([(o.dest, getattr(options, o.dest)) for o in parser.option_list[1:]])

convpath = options.conversion or os.path.join(os.path.dirname(__file__), 'mapping', 'saprc07t.csv')
if not os.path.exists(convpath):
    temppath = os.path.join(os.path.dirname(__file__), 'mapping', convpath + '.csv')
    if os.path.exists(temppath):
        convpath = temppath
        del temppath
tracerpath = options.tracerinfo or os.path.join(os.path.dirname(__file__), 'testdata', 'tracerinfo.dat')
smvpath = options.smvlog or os.path.join(os.path.dirname(__file__), 'testdata', 'smv2.log')
mechpath = options.mechpath or os.path.join(os.path.dirname(__file__), 'testdata')
profilepath = options.profiledat or os.path.join(os.path.dirname(__file__), 'testdata', 'profile.dat')
if len(args) < 1:
    parser.print_help()
    exit()
else:
    out = args[0]

if not os.path.exists(out):
    os.mkdir(out)
outdir = out

if options.archive in archived_options:
    for f in glob(os.path.join(os.path.dirname(__file__), 'fortran_template', '*')):
        shutil.copy(f, outdir)
    for f in glob(os.path.join(os.path.dirname(__file__), 'mechmap_archive', options.archive, '*')):
        shutil.copy(f, outdir)

    

from both import geos
from profile import profile
from mech import mechnml, mechinc, mechext
from map import map, trymap
if options.extfiles:
    mech = mechext
else:
    mech = mechnml
go = geos(tracerpath,
          smvpath)
po = profile(profilepath)

if not os.path.exists(convpath):
    if os.path.exists(os.path.join(os.path.dirname(__file__), convpath + '.csv')):
        convpath = os.path.join(os.path.dirname(__file__), convpath + '.csv')
    elif 'Y' == raw_input("Conversion path does not exist; type Y to create it or any other key to abort\n"):
        trymap(mech(mechpath), convpath, go)
    else:
        exit()
mech_info = mechinc(mech(mechpath), convpath)
cspec_info = go.cspec_info()
tracer_info = go.tracer_info()
profile_info = po.profile_info()
try: 
    mappings, nprof = map(mech(mechpath), convpath, go, po)
except TypeError, err:
    print 'No CMAQ namelist found. Check namelist directory.'
    exit()
mech_info = ("      INTEGER :: NSPC_DFLT = %d\n" % nprof) + mech_info
out = os.path.join(out, 'MAPPING')
file('%s.MECH' % out, 'w').write(mech_info)
file('%s.CSPEC' % out, 'w').write(cspec_info)
file('%s.TRACER' % out, 'w').write(tracer_info)
file('%s.MAP' % out, 'w').write(mappings)
file('%s.PROFILE' % out, 'w').write(profile_info)

for f in glob(os.path.join(os.path.dirname(__file__), 'fortran_template', '*')):
    shutil.copy(f, outdir)

