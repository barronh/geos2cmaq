def get_tracers(tracerinfo):
    tracer_data = dict([(int(l[52:61].strip()), dict(NAME = l[:8].strip(), FULLNAME = l[9:39].strip(), MOLWT = float(l[39:49]), C = int(l[49:52]), TRACER = int(l[52:61]), SCALE = float(l[61:71]), UNIT = l[72:].strip())) for l in open(tracerinfo).readlines() if l[0] not in ('#', ' ')])
    tracer_data = dict([(k, d) for k, d in tracer_data.items() if k < 1000 and 'Pure' not in d['FULLNAME']])
    return tracer_data

if __name__ == '__main__':
    tracers = get_tracers('testdata/tracerinfo.dat')

