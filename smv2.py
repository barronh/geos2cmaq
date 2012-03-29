import re
from StringIO import StringIO
import numpy as np

def get_cspec(smvpath):
    text = file(smvpath, 'r').read()
    reo = re.compile('(?:NBR NAME\s+MW BKGAS\(VMRAT\))(?P<active>.*)(?:INACTIVE SPECIES FOR THIS RUN ARE:)(?P<inactive>.*)(?:THE DEAD SPECIES FOR THIS RUN ARE:)(?P<dead>.*?)(?:=====)', re.M | re.DOTALL)
    gv = reo.search(text).groupdict()
    active = [v.strip().split()[1] for v in gv['active'].strip().split('\n')]
    inactive = gv['inactive'].replace('\n', ' ').split()
    dead = gv['dead'].replace('\n', ' ').split()
    all = active + inactive #+ dead
    return all

if __name__ == '__main__':
    specs = get_cspec('/project/inf15w/bar/geos-chem/baseict/smv2.log')
