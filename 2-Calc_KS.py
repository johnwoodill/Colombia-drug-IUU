import pandas as pd
import numpy as np
from scipy import stats
from scipy.stats import kurtosis
import multiprocessing
import glob
from scipy.spatial import distance
from scipy.stats import entropy
from math import log2
import math
from math import radians, cos, sin, asin, sqrt

def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points 
    on the earth (specified in decimal degrees)
    """

    # convert decimal degrees to radians 
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    # haversine formula 
    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a)) 
    km = 6367 * c
    return km
  



def kl_divergence(p, q):
	return sum(p[i] * log2(p[i]/q[i]) for i in range(len(p)))


def hellinger_explicit(p, q):
    """Hellinger distance between two discrete distributions.
       Same as original version but without list comprehension
    """
    list_of_squares = []
    for p_i, q_i in zip(p, q):

        # caluclate the square of the difference of ith distr elements
        s = (math.sqrt(p_i) - math.sqrt(q_i)) ** 2

        # append 
        list_of_squares.append(s)

    # calculate sum of squares
    sosq = sum(list_of_squares)    

    return sosq / math.sqrt(2)


def process_ks(idat):
    i = pd.to_datetime(idat['timestamp'].iat[0])
    hdat = dat[(dat['timestamp'] < i ) & (dat['timestamp'] >= i - pd.Timedelta(hours=24*10) )]
    rvs1 = idat['distance'].values
    date_range = pd.to_datetime(hdat.timestamp.unique()).sort_values(ascending=False)
    retdat = pd.DataFrame()
    for j in date_range:
        # Filter last hour
        hdat2 = hdat[hdat.timestamp == j]
        rvs2 = hdat2['distance'].values
        # Calculate KS statistic
        ks_res = stats.ks_2samp(rvs1, rvs2)
        ks_stat = ks_res[0]
        pvalue = ks_res[1]

        # Calc pdf with 30 bins
        rvs1_d = pd.cut(rvs1, bins = 30, right=False)
        rvs2_d = pd.cut(rvs2, bins = 30, right=False)

        rvs1_dat = pd.DataFrame({'rvs1': rvs1, 'rvs1_d': rvs1_d})
        rvs2_dat = pd.DataFrame({'rvs2': rvs2, 'rvs2_d': rvs2_d})

        rvs1_dat.loc[:, 'pdf'] = (rvs1_dat.groupby(rvs1_dat['rvs1_d']).transform('count') / len(rvs1_dat)).values
        rvs1_pdf = rvs1_dat.groupby('rvs1_d', as_index=False).mean().pdf

        rvs2_dat.loc[:, 'pdf'] = (rvs2_dat.groupby(rvs2_dat['rvs2_d']).transform('count') / len(rvs2_dat)).values
        rvs2_pdf = rvs2_dat.groupby('rvs2_d', as_index=False).mean().pdf

        # Jensen-Shannon
        js = distance.jensenshannon(rvs1_pdf, rvs2_pdf)
        
        # KL Stat
        kl = kl_divergence(rvs1_pdf, rvs2_pdf)

        # Hellinger Dist
        hl = hellinger_explicit(rvs1_pdf, rvs2_pdf)

        # Time delta
        td = (i - j)/np.timedelta64(1,'h')
        indat = pd.DataFrame({'t': [i], 'lag': [td], 'ks': [ks_stat], 'pvalue': [pvalue], 'js': [js], 'kl': [kl], 'hl': [hl]})
        retdat = pd.concat([retdat, indat])
    retdat = retdat.reset_index(drop=True)
    retdat.to_feather(f"data/daily_ks/{i}.feather")
    print(f"Saving: data/daily_ks/{i}.feather")
    return 0


beg_date = '2018-03-10'
end_date = '2018-04-22'

lon1 = -96.517685
lon2 = -77.515387
lat1 = -5.186610
lat2 = 15.709619

dat = pd.read_feather(f"data/Colombia_Reduced_{beg_date}_{end_date}.feather")



# dat.loc[:, 'timestamp'] = pd.to_datetime(dat.timestamp)

# Remove Longbeach
dat = dat[dat['vessel_A_lat'] <= lat2]
dat = dat[dat['vessel_A_lat'] >= lat1]

dat = dat[dat['vessel_A_lon'] >= lon1]
dat = dat[dat['vessel_A_lon'] <= lon2]


# Group by
gb = dat.groupby('timestamp')
timest = [gb.get_group(x) for x in gb.groups]

# Parallel Process
pool = multiprocessing.Pool(50)
pool.map(process_ks, timest)
pool.close()


# Combine processed files
files = glob.glob('data/daily_ks/*.feather')


list_ = []
for file in files:
    df = pd.read_feather(file)
    list_.append(df)
    mdat = pd.concat(list_, sort=False)



mdat = mdat.reset_index(drop=True)
mdat.to_feather(f"data/SanDiego_KSDaily_{beg_date}_{end_date}.feather")
