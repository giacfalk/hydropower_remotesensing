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

crop_dominance = ee.Image('USGS/GFSAD1000_V0')
crop_mask = ee.Image('USGS/GFSAD1000_V1')
croplands = ee.Image('users/giacomofalchetta/cropland_malawi')
croplands = croplands.mask(croplands.eq(2))

malawi = ee.FeatureCollection('users/giacomofalchetta/gadm').filter(ee.Filter.Or(ee.Filter.eq('ISO3', 'MWI')))

croplands = croplands.clip(malawi)

irrigated = croplands.mask(crop_mask.lte(3).And(crop_mask.neq(0)))

rainfed = croplands.mask(crop_mask.gte(4).And(crop_mask.lte(7)))

rainfed = rainfed.reduceToVectors({
  reducer: ee.Reducer.countEvery(), 
  geometry: malawi, 
  scale: 30,
  maxPixels: 1387423504
})

rainfed = ee.FeatureCollection(rainfed)

irrigated = irrigated.reduceToVectors({
  reducer: ee.Reducer.countEvery(), 
  geometry: malawi, 
  scale: 30,
  maxPixels: 1387423504
});

irrigated = ee.FeatureCollection(irrigated);

month_mean = ee.List.sequence(0, 7*12).map(function(n) { // .sequence: number of years from starting year to present
  var start = ee.Date('2012-01-01').advance(n, 'month'); // Starting date
  var end = start.advance(1, 'month'); // Step by each iteration

  return ee.ImageCollection("NOAA/CDR/AVHRR/NDVI/V4").select('NDVI')
        .filterDate(start, end)
        .mean()
        .set('system:time_start', start.millis());
});

collection = ee.ImageCollection(month_mean);

collection = collection.toBands()

rainfed_ndvi = collection.reduceRegions({
  collection: rainfed,
  reducer: ee.Reducer.mean(),
  scale: 500
});


irrigated_ndvi = collection.reduceRegions({
  collection: irrigated,
  reducer: ee.Reducer.mean(),
  scale: 500
});


//Export monthly TS of NDVI in rainfed and irrigated cropland
irrigated_ndvi = irrigated_ndvi.select(['.*'],null,false);

Export.table.toDrive({
  collection: irrigated_ndvi,
  description: 'irrigated_ndvi',
  fileFormat: 'CSV',
});  
rainfed_ndvi = rainfed_ndvi.select(['.*'],null,false);

Export.table.toDrive({
  collection: rainfed_ndvi,
  description: 'rainfed_ndvi',
  fileFormat: 'CSV',
});  