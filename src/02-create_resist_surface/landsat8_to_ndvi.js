// Calculate NDVI for a single year 
function NVDI(image){
  var nir = image.select('SR_B5');
  var red = image.select('SR_B4');
  var ndvi = nir.subtract(red).divide(nir.add(red)).rename('nd');
  return(ndvi);
}

// Get NDVI composites across 2016-2024
// during the frost-free growing season in Toronto, Ontario (Zone B)
// April 30 to October 13 
// https://www.ontario.ca/page/climate-zones-and-planting-dates-vegetables-ontario

var geometry = ee.FeatureCollection("projects/ee-garlandxie/assets/to_boundary")
                 .geometry();

var l8_2016 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2016, 4, 30), 
             ee.Date.fromYMD(2016, 10, 13))
           .map(NVDI);

var l8_2017 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2017, 4, 30), 
             ee.Date.fromYMD(2017, 10, 13))
           .map(NVDI);

var l8_2018 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2018, 4, 30), 
             ee.Date.fromYMD(2018, 10, 13))
           .map(NVDI);
           
var l8_2019 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2019, 4, 30), 
             ee.Date.fromYMD(2019, 10, 13))
           .map(NVDI);
           
var l8_2020 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2020, 4, 30), 
             ee.Date.fromYMD(2020, 10, 13))
           .map(NVDI);
           
var l8_2021 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2021, 4, 30), 
             ee.Date.fromYMD(2021, 10, 13))
           .map(NVDI);
           
var l8_2022 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2022, 4, 30), 
             ee.Date.fromYMD(2022, 10, 13))
           .map(NVDI);
           
var l8_2023 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2023, 4, 30), 
             ee.Date.fromYMD(2023, 10, 13))
           .map(NVDI);

var l8_2024 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2024, 4, 30), 
             ee.Date.fromYMD(2024, 10, 13))
           .map(NVDI);
           
// Calculate NDVI yearly composite 
var mergedCollection = ee.ImageCollection(
  l8_2016.merge(l8_2017)
         .merge(l8_2018)
         .merge(l8_2019)
         .merge(l8_2020)
         .merge(l8_2021)
         .merge(l8_2022)
         .merge(l8_2023)
         .merge(l8_2024)
         );
         
// Get yearly NDVI composite            
var finalOutput = mergedCollection.reduce(ee.Reducer.median())
                                  .clip(geometry);
                                  
// Visualize data
var ndviParams = {min: -1, max: 1, palette: ['blue', 'white', 'green']};
Map.addLayer(finalOutput, ndviParams, 'NDVI image');

// Export to Google Drive
Export.image.toDrive({
  image: finalOutput,
  description: 'ndvi_to_2016-2024_3m_res',
  folder: "google earth engine",
  fileFormat: 'GeoTIFF',
  crs: 'EPSG:26917',
  maxPixels: 209242992,
  scale: 2.5
}); 
