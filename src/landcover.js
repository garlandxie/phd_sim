// Import landcover data
var dataset = ee.ImageCollection('ESA/WorldCover/v100').first()
          
// clip by bbox
//dataset = dataset.clip(geometry);
var geometry = ee.FeatureCollection("projects/ee-garlandxie/assets/to_boundary")
                 .geometry()
                 
var image = dataset.clip(geometry)

// Visualize 
var visualization = {
  bands: ['Map'],
};

Map.addLayer(image, visualization, 'Landcover');
Map.centerObject(image);

// Export to Google Drive
Export.image.toDrive({
  image: dataset,
  description: 'landcover_to_3m_res',
  folder: "google earth engine",
  fileFormat: 'GeoTIFF',
  crs: 'EPSG:26917',
  maxPixels: 654958084580,
  scale: 2.5
})
