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

collection = ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY').filterDate('2010-01-01', '2018-09-01').select('precipitation');

def function(im):
    return im.rename([im.get("system:index")])

arr = collection.map(function)

collection = arr


def stackCollection(collection):
    first = ee.Image(collection.first()).select([])
    def appendBands(image, previous):
        return ee.Image(previous).addBands(image)
    return ee.Image(collection.iterate(appendBands, first))

stacked = stackCollection(collection)

Countries = ee.FeatureCollection('users/giacomofalchetta/shirebasin')

lightsum = stacked.reduceRegions(collection=Countries, reducer=ee.Reducer.mean())

lightsum = fc2df(lightsum)

lightsum.to_csv("C:\\Users\\Falchetta\\OneDrive - FONDAZIONE ENI ENRICO MATTEI\\Visiting IIASA\\hydropower_remotesensing/chirps_rainfall_malawi_2010_2018.csv")
