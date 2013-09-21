import sys
import pylab as pl
import numpy as np
from warnings import warn
from netCDF4 import MFDataset


def aqmeiidomain():
    from mpl_toolkits.basemap import Basemap
    # From griddesc
    aqmeii_proj = {}
    aqmeii_proj['llcrnrx'] = -2556000.
    aqmeii_proj['llcrnry'] = -1728000.
    aqmeii_proj['dx'] = 12000.0
    aqmeii_proj['dy'] = 12000.0
    aqmeii_proj['nx'] = 459
    aqmeii_proj['ny'] = 299

    # Derived
    exec('width = nx * dx', None, aqmeii_proj)
    exec('height = ny * dy', None, aqmeii_proj)
    exec('urcrnrx = llcrnrx + width', None, aqmeii_proj)
    exec('urcrnry = llcrnry + height', None, aqmeii_proj)

    cmaqmap = Basemap(rsphere = (6370000., 6370000.),\
                        resolution = 'c', projection = 'lcc',\
                        lat_1 = 33., lat_2 = 45., lat_0 = 40., lon_0 = -97.,\
                        llcrnrx = aqmeii_proj['llcrnrx'], llcrnry = aqmeii_proj['llcrnry'],\
                        urcrnrx = aqmeii_proj['urcrnrx'], urcrnry = aqmeii_proj['urcrnry'])
    return cmaqmap



def plot(paths, keys = ['O3'], func = 'mean', map = True, prefix = 'BC', scale = 'deciles', minmax = (None, None), minmaxq = (0, 100)):
    from pylab import figure, NullFormatter, close, rcParams
    rcParams['text.usetex'] = False
    from matplotlib.colors import LinearSegmentedColormap, BoundaryNorm, LogNorm
    f = MFDataset(paths)
    for var_name in keys:
        var = eval(var_name, None, f.variables)[:]
        if func == 'each':
            vars = [(vi, v) for vi, v in enumerate(var)]
        else:
            vars = [(func, getattr(np, func)(var, axis = 0))]
        for func, var in vars:
            bmap = None
            vmin, vmax = np.percentile(np.ma.compressed(var).ravel(), list(minmaxq))
            if minmax[0] is not None:
                vmin = minmax[0]
            if minmax[1] is not None:
                vmax = minmax[1]
            if scale == 'log':
                bins = np.logspace(np.log10(vmin), np.log10(vmax), 11)
            elif scale == 'linear':
                bins = np.linspace(vmin, vmax, 11)
            elif scale == 'deciles':
                bins = np.percentile(np.ma.compressed(np.ma.masked_greater(np.ma.masked_less(var, vmin), vmax)).ravel(), [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100])
                bins[0] = vmin; bins[-1] = vmax
            norm = BoundaryNorm(bins, ncolors = 256)
            if map:
                fig = pl.figure(figsize = (8, 8))
                axmap = fig.add_subplot(3,3,5)
                try:
                    cmaqmap = aqmeiidomain()
                    cmaqmap.drawcoastlines(ax = axmap)
                    cmaqmap.drawcountries(ax = axmap)
                    cmaqmap.drawstates(ax = axmap)
                except Exception, e:
                    warn('An error occurred and no map will be shown:\n%s' % str(e))
                axn = fig.add_subplot(3,3,2, sharex = axmap)
                axw = fig.add_subplot(3,3,4, sharey = axmap)
                axe = fig.add_subplot(3,3,6, sharey = axmap)
                axs = fig.add_subplot(3,3,8, sharex = axmap)
                cax = fig.add_axes([.8, .7, .05, .25])
                for ax in [axmap, axe]:
                    ax.yaxis.set_major_formatter(NullFormatter())
                for ax in [axmap, axn]:
                    ax.xaxis.set_major_formatter(NullFormatter())
                for ax in [axn, axs]:
                    ax.set_ylabel('sigma')
                for ax in [axe, axw]:
                    ax.set_xlabel('sigma')
                xyfactor = 1
            else:
                fig = pl.figure(figsize = (16, 4))
                axw = fig.add_subplot(1,4,1)
                axn = fig.add_subplot(1,4,2)
                axe = fig.add_subplot(1,4,3)
                axs = fig.add_subplot(1,4,4)
                cax = fig.add_axes([.91, .1, .025, .8])
                axw.set_ylabel('sigma')
            
                xyfactor = 1e-3
                     
            x = f.NCOLS + 1
            y = f.NROWS + 1
            X, Y = np.meshgrid(np.arange(x)[1:] * f.XCELL * xyfactor, f.VGLVLS)
            patchess = axs.pcolor(X, Y, var[:, :x-1], cmap = bmap, vmin = vmin, vmax = vmax, norm = norm)
            if not map:
                axs.set_ylim(*axs.get_ylim()[::-1])
                axs.set_xlim(*axs.get_xlim()[::-1])
                axs.set_title('South')
                axs.set_xlabel('E to W km')
        
            X, Y = np.meshgrid(np.arange(x) * f.XCELL * xyfactor, f.VGLVLS)
            patchesn = axn.pcolor(X, Y, var[:, x+y:x+y+x], cmap = bmap, vmin = vmin, vmax = vmax, norm = norm)
            axn.set_ylim(*axn.get_ylim()[::-1])
            if not map:
                axn.set_title('North')
                axn.set_xlabel('W to E km')

            if map:
                X, Y = np.meshgrid(f.VGLVLS, np.arange(y) * f.YCELL)
                patchese = axe.pcolor(X, Y, var[:, x:x+y].swapaxes(0,1), cmap = bmap, vmin = vmin, vmax = vmax, norm = norm)
                axe.set_xlim(*axe.get_xlim()[::-1])
            else:
                X, Y = np.meshgrid(np.arange(y) * f.YCELL * xyfactor, f.VGLVLS)
                patchese = axe.pcolor(X, Y, var[:, x:x+y], cmap = bmap, vmin = vmin, vmax = vmax, norm = norm)
                axe.set_ylim(*axe.get_ylim()[::-1])
                axe.set_title('East')
                axe.set_xlabel('N to S km')
                axe.set_xlim(*axe.get_xlim()[::-1])
            if map:
                X, Y = np.meshgrid(f.VGLVLS, np.arange(y) * f.YCELL)
                patchesw = axw.pcolor(X, Y, var[:, x+y+x:x+y+x+y].swapaxes(0,1), cmap = bmap, vmin = vmin, vmax = vmax, norm = norm)
            else:
                X, Y = np.meshgrid(np.arange(y) * f.YCELL * xyfactor, f.VGLVLS)
                patchesw = axw.pcolor(X, Y, var[:, x+y+x:x+y+x+y], cmap = bmap, vmin = vmin, vmax = vmax, norm = norm)
                axw.set_ylim(*axw.get_ylim()[::-1])
                axw.set_title('West')
                axw.set_xlabel('S to N km')

            fig.colorbar(patchesw, cax = cax, boundaries = bins)
            fig.savefig('%s_%s_%s.png' % (prefix, var_name, func))
            pl.close(fig)
    
if __name__ == '__main__':
    from optparse import OptionParser
    parser = OptionParser()
    parser.set_usage("""Usage: python -m geos2cmaq.plot [-v VAR1,VAR2] [-p prefix] ifile

    ifile - path to a file formatted as type -f
    
    """)

    parser.add_option("-v", "--variables", dest = "variables", action = "append", default = ["O3"],
                        help = "Variable names separated by ','")

    parser.add_option("-p", "--prefix", dest = "prefix", type = "string", default = None,
                        help = "Prefix for figures")

    parser.add_option("-n", "--no-map", dest = "nomap", action = "store_true", default = False,
                        help = "Try to plot with map")

    parser.add_option("-s", "--scale", dest = "scale", type = "string", default = 'deciles',
                        help = "Defaults to deciles (i.e., 10 equal probability bins), but linear and log are also options.")

    parser.add_option("", "--minmax", dest = "minmax", type = "string", default = (None, None),
                        help = "Defaults None, None.")

    parser.add_option("", "--minmaxq", dest = "minmaxq", type = "string", default = '0,100',
                        help = "Defaults 0,100.")

    parser.add_option("-f", "--time-func", dest = "timefunc", default = "mean",
                        help = "Use time-func to reduce the time dimension (mean, min, max, std, var, ndarray.__iter__, etc.")

    (options, args) = parser.parse_args()
    
    if not len(args) > 0:
        parser.print_help()
        exit()
    if options.prefix is None:
        options.prefix = args[0]
    plot(args, keys = reduce(list.__add__, [v.split(',') for v in options.variables]), map = not options.nomap, prefix = options.prefix, func = options.timefunc, scale = options.scale, minmax = eval(options.minmax), minmaxq = eval(options.minmaxq))
