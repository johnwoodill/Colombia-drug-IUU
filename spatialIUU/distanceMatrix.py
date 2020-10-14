import pandas as pd
import numpy as np
import scipy.spatial.distance as ssd
from scipy.spatial import distance
#from scipy.stats import entropy
#from scipy.stats import gaussian_kde
from scipy import stats

def f_js(x, y):
    return distance.jensenshannon(x, y)
    #return stats.ks_2samp(x, y)

def d_matrix(dat, interval, NN=0):
    dat = dat[dat['NN'] != 0]
    dat = dat[dat['NN'] <= NN]
    dat.loc[:, 'distance'] = np.log(1 + dat.loc[:, 'distance'])

    dat.loc[:, 'month'] = pd.DatetimeIndex(dat['timestamp']).month
    dat.loc[:, 'day'] = pd.DatetimeIndex(dat['timestamp']).day
    dat.loc[:, 'hour'] = pd.DatetimeIndex(dat['timestamp']).hour

    if interval == 'day':
        dat = dat.groupby(['vessel_A', 'month', 'day'], as_index=False)[
            'distance'].mean()
        
        x = []

        gb = dat.groupby(['month', 'day'])['distance']
        lst = [gb.get_group(x) for x in gb.groups]
        x = []
        for i in range(len(lst)):
            for j in range(len(lst)):
                x += [(i, j, f_js(lst[i], lst[j]))]

        distMatrix = pd.DataFrame(x).pivot(index=0, columns=1, values=2)
        distMatrix = np.matrix(distMatrix)
        distArray = ssd.squareform(distMatrix)

    if interval == 'dayhour':
        dat = dat.groupby(['vessel_A', 'month', 'day', 'hour'],
                          as_index=False)['distance'].mean()
        #dat['lag_distance'] = dat.sort_values(['vessel_A', 'month', 'day', 'hour'], ascending=False).groupby(['vessel_A'], as_index=False)['distance'].shift(-1)
        #dat['lag_distance'] = dat.sort_values(['vessel_A', 'month', 'day', 'hour'], ascending=False).groupby(['vessel_A'], as_index=False)['distance'].transform('mean')
        #dat = dat.dropna()
        #dat['distance'] = dat['distance'] - dat['lag_distance']
        #dat = dat[['vessel_A', 'month', 'day', 'hour', 'distance']]
        
        #print(dat)
        
        gb = dat.groupby(['month', 'day', 'hour'])['distance']
        lst = [gb.get_group(x) for x in gb.groups]
        x = []
        y = []
        for i in range(len(lst)):
            for j in range(len(lst)):
                p = lst[i]
                q = lst[j]
                p1 = np.histogram(p, bins = 20)[0] / len(p)
                q1 = np.histogram(q, bins = 20)[0] / len(q)
                x += [(i, j, f_js(p1, q1))]
                #x += [(i, j, ks.statistic)]
                #y += [(i, j, ks.pvalue)]
                #print(i, j)

        distMatrix = pd.DataFrame(x).pivot(index=0, columns=1, values=2)
        distMatrix = np.matrix(distMatrix)
        distArray = ssd.squareform(distMatrix)

        # xdistMatrix = pd.DataFrame(x).pivot(index=0, columns=1, values=2)
        # np.fill_diagonal(np.asarray(xdistMatrix), 0)
        # xdistMatrix = np.matrix(xdistMatrix)

        # ydistMatrix = pd.DataFrame(y).pivot(index=0, columns=1, values=2)
        # np.fill_diagonal(np.asarray(ydistMatrix), 0)
        # ydistMatrix = np.matrix(ydistMatrix)
        
    return (distMatrix, distArray)
    #return (xdistMatrix, ydistMatrix)



#p = lst[i]
#q = lst[j]


#p = np.array(p)
#q = np.array(q)

#m = (p + q) / 2

#divergence = (stats.entropy(p, m) + stats.entropy(q, m)) / 2

#divergence


#from scipy import stats, integrate

#kdep = stats.gaussian_kde(p)
#kdeq = stats.gaussian_kde(q)

#p = (kdep(p)/sum(kdep(p)))
#q = (kdeq(q)/sum(kdeq(q)))

#from scipy.spatial import distance
#import numpy as np
#from scipy import stats

#x1 = np.random.normal(size=100)
#x2 = np.random.normal(size=100)
#p = x1/sum(x1)
#q = x2/sum(x2)

#p = stats.norm.pdf(x1)
#q = stats.norm.pdf(x2)

#distance.jensenshannon(p, q)

