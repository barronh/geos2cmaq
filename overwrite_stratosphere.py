import os
import shutil
from glob import glob
from datetime import datetime, timedelta
from warnings import warn

import numpy as np
from netCDF4 import Dataset

def copyup(date_start, date_end, bctemp, mettemp, minz = None, maxz = None):
    """
    Identifying tropopause following Lam and Fu (doi:10.5194/acp-10-4013-2010)
    and using only GEOS-Chem values below or at the tropopause.
    """
    if minz is None:
        minz = 8000. # based on where clause for equation 2 in Lam and Fu
    if maxz is None:
        maxz = 19000. # based on where clause for equation 2 in Lam and Fu

    # Require that ozone concentrations be representative
    # of the stratosphere Lam and Fu cite McPeters et al. 
    # (2007, doi:10.1029/2005JD006823).
    mino3 = .3 # CMAQ concentrations are in ppmV 

    # Decrement the day so that it can be incremented in the loop
    date_now = date_start - timedelta(days = 1)
    while date_now < date_end:
        # Increment the date by 1 day
        date_now = date_now + timedelta(days = 1)
        
        # Find the day specific METBDY file
        metpath = mettemp % date_now.strftime('%y%m%d')
        bcpathold = bctemp % date_now.strftime('%Y%m%d')
        bcpathnew = os.path.basename(bcpathold + '.LamFu')
        shutil.copyfile(bcpathold, bcpathnew)
        print bcpathold, metpath, bcpathnew
        bcf = Dataset(bcpathnew, mode = 'r+')
        o3 = bcf.variables['O3']

        # Skip if no values are greater than 300 ppb
        o3constraint = o3[:] > mino3
        if (o3constraint == False).all():
            warn('Boundary file (%s) has no ozone greater than 300 ppb; max o3=%.f ppb' % (bcpathold, o3.max() * 1000.))
            return

        bctimes = len(bcf.dimensions['TSTEP'])

        metf = Dataset(metpath, 'r')
        mettimes = len(metf.dimensions['TSTEP'])
        if bctimes < mettimes:
            warn('Boundary files has %d times and met has %d times; pairing the first %d values from each file' % (bctimes, mettimes, bctimes))

        # Get height values for layers.
        zf = metf.variables['ZF'][:bctimes] # Read Full Height of layer
        zh = metf.variables['ZH'][:bctimes] # Read half height of layer
        #zf = np.fromstring('17556 14780 12822 11282 10002 8901 7932 7064 6275 5553 4885 4264 3683 3136 2619 2226 1941 1665 1485 1308 1134 964 797 714 632 551 470 390 311 232 154 115 77 38 19', sep = ' ')[::-1]
        #zh = zf - np.diff(np.append(0, zf)) * .5
        #zf = zf[None, :, None].repeat(o3.shape[2], 2).repeat(o3.shape[0], 0)
        #zh = zh[None, :, None].repeat(o3.shape[2], 2).repeat(o3.shape[0], 0)
        # Lowest layer must have a full height (ZF) greater
        # than 8km. This could just as easily be half height (ZH)
        lowest = np.ma.masked_less(zf, minz).argmin(1)

        # Highest layer cannot have a full height (ZF) greater
        # than 8km. This could just as easily be half height (ZH)
        highest = np.ma.masked_greater(zf, maxz).argmax(1)
        
        # Calculate the C_i - C_{i-1}
        # for layers max to min+1
        do3 = np.diff(o3[:, :], axis = 1)

        # Calculate the Z_i - Z_{i-1}
        # for layers max to min+1
        # Distance between concentrations is based on half height
        dz = np.diff(zh, axis = 1)
        
        # Calculate the dC/dz
        do3dz = do3 / dz
        
        # Difference between dC(i+1,i)/dH(i+1,i) - dC(i,i-1)/dH(i,i-1)
        # Values at surface and at maximum layer are not calculatable
        costf = do3dz[:, 1:] - do3dz[:, :-1]
        
        # Mask (read as reject) any values where the full height
        # is lower than the minimum
        costf = np.ma.masked_where(zf[:, 1:-1] < minz, costf)
        
        # If there are no valid points, warn and skip.
        if costf.mask.all():
            warn('Cannot copy up.  Equation 2 from Lam and Fu (doi:10.5194/acp-10-4013-2010) \nhas no valid solution given the altitude constraints (min=%.f, max%.f).' % (minz, maxz))
            return
        
        # The maximum layer index (argmax) must be
        # incremented by 1 to account for the lack of the
        # surface layer in the cost function
        id = costf.argmax(1) + 1
        
        # Triple check that the id is within the acceptable
        # boundaries
        id = np.where(id > highest, highest, id)
        id = np.where(id < lowest, lowest, id)
        # Loop over times
        for time_idx, timeslice in enumerate(id):
            # For each grid-cell in the perimeter
            count = 0.
            skipcount = 0
            writecount = 0
            for perim_idx, layv in enumerate(timeslice):
                count += 1
                if isinstance(layv,np.ndarray):
                    # if data is multidimensional, iterate over add'l dims
                    for perim_idx_2, layv_2 in enumerate(layv):
                        # Check that ozone concentrations are above 300 ppb
                        destination_vector = o3[time_idx, layv_2+1:, perim_idx, perim_idx_2].copy()
                        destination_mask = destination_vector > mino3
                        if not (destination_mask == False).all():
                            # Overwrite only those values where ozone is greater than 300ppb
                            writecount += 1
                            for di, dv in enumerate(destination_mask):
                                if dv: o3[time_idx, layv_2+1+di, perim_idx, perim_idx_2] = o3[time_idx, layv_2, perim_idx, perim_idx_2]
                            assert((o3[time_idx, layv_2+1:, perim_idx, perim_idx_2] != destination_vector).any())
                        else:
                            skipcount += 1
                else:
                    # Check that ozone concentrations are above 300 ppb
                    destination_vector = o3[time_idx, layv+1:, perim_idx].copy()
                    destination_mask = destination_vector > mino3
                    if not (destination_mask == False).all():
                        # Overwrite only those values where ozone is greater than 300ppb
                        writecount += 1
                        for di, dv in enumerate(destination_mask):
                            if dv: o3[time_idx, layv+1+di, perim_idx] = o3[time_idx, layv, perim_idx]
                        assert((o3[time_idx, layv+1:, perim_idx] != destination_vector).any())
                    else:
                        skipcount += 1
            bcf.sync()
            tflag = '%dT%06d' % tuple(bcf.variables['TFLAG'][time_idx, 0].tolist())
            print '%s, N, Skip Col, Write Col, Skip/Count: %.f %6d %6d %.2f' % (tflag, count, skipcount, writecount, skipcount / count)
        bcf.close()
if __name__ == '__main__':
    import sys
    try:
        args = sys.argv[1:]
        if len(args) >= 4:
            year_month_day, ndays, bctemp, mettemp = args[:4]
            if len(args) == 4:
                minz, maxz = None, None
            elif len(args) == 6:
                minz, maxz = args[4:]
            else:
                raise ValueError('')
        else:
            raise ValueError('')
    except:
        print """Usage: %s YYYYMMDD NDAYS BCTEMP METTEMP [minz maxz]

Description:
    Identifies tropopause following Lam and Fu (doi:10.5194/acp-10-4013-2010)
    and copies troposphere values over any stratosphere values.
    
    Inputs are METBDY3D files from MCIP and a CMAQ IOAPI boundary file

Parameters:
 - YYYYMMDD is the 4 digit year, 2 digit month and 2 digit day of the start date
 - NDAYS is the number of days to operate on (e.g., 1 = just the start date)
 - BCTEMP is a template string following printf that can take a date string (e.g. YYYYMMDD) to get a day value
 - METTEMP same as BCTEMP for metbdy, but uses two digit year (e.g., YYMMDD)
 - minz minimum altitude for the tropopause (based on layer top) (optional)
 - maxz minimum altitude for the tropopause (based on layer top) (optional)

e.g.,

$ %s 
""" % (__file__, __file__)
        exit()
    date_start = datetime.strptime(year_month_day, '%Y%m%d')
    date_end = date_start + timedelta(days = int(ndays) - 1)
    copyup(date_start = date_start, date_end = date_end, bctemp = bctemp, mettemp = mettemp, minz = minz, maxz = maxz)
