import ee, datetime
import pandas as pd
import geos
import gdal
import fiona
import geopandas as gpd
import matplotlib.dates as mdates
from IPython.display import Image
from matplotlib import dates
from shapely.geometry import shape
import skimage


def fc2df(fc):
    # Convert a FeatureCollection into a pandas DataFrame
    # Features is a list of dict with the output
    features = fc.getInfo()['features']

    dictarr = []

    for f in features:
        # Store all attributes in a dict
        attr = f['properties']
        # and treat geometry separately
        attr['geometry'] = f['geometry']  # GeoJSON Feature!
        # attr['geometrytype'] = f['geometry']['type']
        dictarr.append(attr)

    df = gpd.GeoDataFrame(dictarr)
    # Convert GeoJSON features to shape
    df['geometry'] = map(lambda s: shape(s), df.geometry)
    return df

ee.Initialize()

collection = ee.ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG').filterDate('2012-01-01', '2017-01-01').select('avg_rad')

pop = ee.Image('users/giacomofalchetta/LandScanGlobal2017')

def function(im):
    return im.rename([im.get("system:index")])

collection = collection.map(function)

def conditional(image):
	image = image.mask(pop.gt(250))
	return image.mask(image.gt(5))
	

collection = collection.map(conditional)

collection2 = ee.ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG').filterDate('2017-01-01', '2019-01-01').select('avg_rad')

def function(im):
    return im.rename([im.get("system:index")])

collection2 = collection2.map(function)

def conditional(image):
	image = image.subtract(0.15).mask(pop.gt(250))
	return image.mask(image.gt(5))

collection2 = collection2.map(conditional)

collection = collection.merge(collection2)

def stackCollection(collection):
    first = ee.Image(collection.first()).select([])
    def appendBands(image, previous):
        return ee.Image(previous).addBands(image)
    return ee.Image(collection.iterate(appendBands, first))

stacked = stackCollection(collection)

Countries = ee.FeatureCollection('users/giacomofalchetta/gadm36_1').filter(ee.Filter.Or(ee.Filter.eq('GID_0', 'MWI')))

lightsum = stacked.reduceRegions(collection=Countries, reducer=ee.Reducer.sum())

lightsum = fc2df(lightsum)

lightsum.to_csv("C:\\Users\\Falchetta\\OneDrive - FONDAZIONE ENI ENRICO MATTEI\\Visiting IIASA\\hydropower_remotesensing/viirs_dnb_2012_2018.csv")

