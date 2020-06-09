import spatialIUU.processGFW as siuu
import os
import pandas as pd
import shapefile
from shapely.geometry import shape, Point
from pyproj import Proj, transform
from joblib import Parallel, delayed
import multiprocessing
from datetime import datetime
import glob
import pytz

# -------------------------------------------
# [1]Processing raw GFW Data

# Set global constants
global GFW_DIR, GFW_OUT_DIR_CSV, GFW_OUT_DIR_FEATHER, PROC_DATA_LOC, MAX_SPEED, REGION, lon1, lon2, lat1, lat2

siuu.GFW_DIR = '/data2/GFW_point/'
siuu.GFW_OUT_DIR_CSV = '/home/server/pi/homes/woodilla/Data/GFW_point/Colombia/csv/'
siuu.GFW_OUT_DIR_FEATHER = '/home/server/pi/homes/woodilla/Data/GFW_point/Colombia/feather/'
siuu.PROC_DATA_LOC = '/home/server/pi/homes/woodilla/Projects/Colombia-drug-IUU/data/'
siuu.REGION = 'Colombia'
siuu.MAX_SPEED = 32

# GFW_OUT_DIR_CSV = '/home/server/pi/homes/woodilla/Data/GFW_point/SanDiego/csv/'
# GFW_OUT_DIR_FEATHER = '/home/server/pi/homes/woodilla/Data/GFW_point/SanDiego/feather/'
# PROC_DATA_LOC = '/home/server/pi/homes/woodilla/Projects/San-Diego-IUU/data/'

# Check if dir exists and create
os.makedirs(siuu.GFW_OUT_DIR_CSV, exist_ok=True) 
os.makedirs(siuu.GFW_OUT_DIR_FEATHER, exist_ok=True) 
os.makedirs(siuu.PROC_DATA_LOC, exist_ok=True) 

siuu.region = 1
siuu.lon1 = -96.517685
siuu.lon2 = -77.515387
siuu.lat1 = -5.186610
siuu.lat2 = 15.709619


GFW_OUT_DIR_CSV = '/home/server/pi/homes/woodilla/Data/GFW_point/Colombia/csv/'
GFW_OUT_DIR_FEATHER = '/home/server/pi/homes/woodilla/Data/GFW_point/Colombia/feather/'
PROC_DATA_LOC = '/home/server/pi/homes/woodilla/Projects/Colombia-drug-IUU/data/'
beg_date = '2018-03-10'
end_date = '2018-04-22'

# Colombia
# Event: https://content.govdelivery.com/accounts/USDHSCG/bulletins/1ec3219
siuu.compileData(beg_date, end_date, 1, parallel=True, ncores=20)




# Combine processed files
mdat = pd.read_feather(f"data/Colombia_{beg_date}_{end_date}.feather")


mdat = mdat.sort_values('timestamp')
# mdat = mdat.dropna()

mdat_timestamp = pd.DatetimeIndex(mdat.timestamp, tz='UTC')
mdat_timestamp = mdat_timestamp.tz_convert('America/Panama')
mdat_timestamp = mdat_timestamp.strftime("%Y-%m-%d %H:%M:%S")
mdat.loc[:, 'timestamp'] = mdat_timestamp
mdat.loc[:, 'timestamp'] = pd.to_datetime(mdat.timestamp, format="%Y-%m-%d %H:%M:%S")

# Save Processed
mdat = mdat.reset_index(drop=True)
mdat.to_feather(f"data/Colombia_Processed_{beg_date}_{end_date}.feather")

# Save Reduced columns
mdat2 = mdat[['timestamp', 'vessel_A', 'vessel_B', 'vessel_A_lon', 'vessel_A_lat', 'distance']]

# Save data
mdat2 = mdat2.reset_index(drop=True)
mdat2.to_feather(f"data/Colombia_Reduced_{beg_date}_{end_date}.feather")


