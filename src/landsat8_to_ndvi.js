// Get the boundary of Toronto
var geometry = 
    /* color: #d63000 */
    /* displayProperties: [
      {
        "type": "rectangle"
      },
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.MultiPolygon(
        [[[[-79.67199031180144, 43.87844835367451],
           [-79.67199031180144, 43.558231140741036],
           [-79.10070124930144, 43.558231140741036],
           [-79.10070124930144, 43.87844835367451]]],
         [[[-79.53500453299284, 43.71352873374248],
           [-79.53500453299284, 43.713032421571846],
           [-79.52333155936003, 43.713032421571846],
           [-79.52333155936003, 43.71352873374248]]]], null, false);


// Calculate NDVI for a single year 
function NVDI(image){
  var nir = image.select('SR_B5')
  var red = image.select('SR_B4')
  var ndvi = nir.subtract(red).divide(nir.add(red)).rename('nd');
  return(ndvi)
}


// Get NDVI composites across 2016-2024
// during the frost-free growing season in Toronto, Ontario (Zone B)
// April 30 to October 13 
// https://www.ontario.ca/page/climate-zones-and-planting-dates-vegetables-ontario

// I know the code is extremely redundant BUT I'm not a JavaScript developer
// and my memory is really really bad (this helps me understand the GIS process)
// plus it's easier to find errors for each year if needed for reproducibility


// Note: I need to figure out how to apply certain masks (e.g., cloud cover)

var l8_2016 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2016, 4, 30), 
             ee.Date.fromYMD(2016, 10, 13))
           .map(NVDI)

var l8_2017 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2017, 4, 30), 
             ee.Date.fromYMD(2017, 10, 13))
           .map(NVDI)

var l8_2018 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2018, 4, 30), 
             ee.Date.fromYMD(2018, 10, 13))
           .map(NVDI)
           
var l8_2019 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2019, 4, 30), 
             ee.Date.fromYMD(2019, 10, 13))
           .map(NVDI)
           
var l8_2020 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2020, 4, 30), 
             ee.Date.fromYMD(2020, 10, 13))
           .map(NVDI)
           
var l8_2021 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2021, 4, 30), 
             ee.Date.fromYMD(2021, 10, 13))
           .map(NVDI)
           
var l8_2022 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2022, 4, 30), 
             ee.Date.fromYMD(2022, 10, 13))
           .map(NVDI)
           
var l8_2023 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2023, 4, 30), 
             ee.Date.fromYMD(2023, 10, 13))
           .map(NVDI)

var l8_2024 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
           .filterBounds(geometry)
           .filterDate(
             ee.Date.fromYMD(2024, 4, 30), 
             ee.Date.fromYMD(2024, 10, 13))
           .map(NVDI)
           
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
         )
         
var finalOutput = mergedCollection.reduce(ee.Reducer.median())
                                  .clip(geometry)

var ndviParams = {min: -1, max: 1, palette: ['blue', 'white', 'green']};
Map.addLayer(finalOutput, ndviParams, 'NDVI image')

// Export to Google Drive
Export.image.toDrive({
  image: finalOutput,
  description: 'ndvi_to_2016-2024_10m_res',
  scale: 30
})
