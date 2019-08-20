import ee, datetime
import pandas as pd
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

image = ee.Image('UMD/hansen/global_forest_change_2018_v1_6')

dataset = image
loss = dataset.select('loss');
gain = dataset.select('gain');
lossyear = dataset.select('lossyear');

//for each value of lossyear create an image
ly2001= lossyear.mask(lossyear.eq(1))
ly2002= lossyear.mask(lossyear.eq(2))
ly2003= lossyear.mask(lossyear.eq(3))
ly2004= lossyear.mask(lossyear.eq(4))
ly2005= lossyear.mask(lossyear.eq(5))
ly2006= lossyear.mask(lossyear.eq(6))
ly2007= lossyear.mask(lossyear.eq(7))
ly2008= lossyear.mask(lossyear.eq(8))
ly2009= lossyear.mask(lossyear.eq(9)) 
ly2010= lossyear.mask(lossyear.eq(10))
ly2011= lossyear.mask(lossyear.eq(11))
ly2012= lossyear.mask(lossyear.eq(12))
ly2013= lossyear.mask(lossyear.eq(13))
ly2014= lossyear.mask(lossyear.eq(14))
ly2015= lossyear.mask(lossyear.eq(15))
ly2016= lossyear.mask(lossyear.eq(16))
ly2017= lossyear.mask(lossyear.eq(17))
ly2018= lossyear.mask(lossyear.eq(18))

loss = ee.ImageCollection([ly2001, ly2002, ly2003, ly2004, ly2005, ly2006, ly2007, ly2008, ly2009, ly2010, ly2011, ly2012, ly2013, ly2014, ly2015, ly2016, ly2017, ly2018])

#calculate total lost area for each year in each GADM 

def fun(image) {
  return image.multiply(ee.Image.pixelArea())
};

loss = loss.map(fun);

def stackCollection(collection) {
  #Create an initial image.
  first = ee.Image(collection.first()).select([]);

  #Write a function that appends a band to an image.
  appendBands = function(image, previous) {
    return ee.Image(previous).addBands(image);
  };
  return ee.Image(collection.iterate(appendBands, first));
};

loss = stackCollection(loss);

#Calculate in 10-km raidus buffer of grid and highly urbanised areas
Countries = ee.FeatureCollection('users/giacomofalchetta/gridandcitiesproximity_10km_malawi')

loss = loss.reduceRegions({
  reducer: ee.Reducer.sum(),
  collection: Countries,
  scale: 250,
});

loss = fc2df(loss)

loss.to_csv("D:\OneDrive - FONDAZIONE ENI ENRICO MATTEI\Visiting IIASA\hydropower_remotesensing/losstreecovermalawiperyear.csv")



