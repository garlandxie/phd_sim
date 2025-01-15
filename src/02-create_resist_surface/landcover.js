// Import landcover data
var dataset = ee.ImageCollection('ESA/WorldCover/v100').first()
          
// clip by bbox
//dataset = dataset.clip(geometry);
var geometry = ee.FeatureCollection("projects/ee-garlandxie/assets/to_boundary")
                 .geometry()
                 
var lc_to = dataset.clip(geometry)

// Visualize 
var visualization = {
  bands: ['Map'],
};

Map.addLayer(lc_to, visualization, 'Landcover');
Map.centerObject(lc_to);

// Export to Google Drive
Export.image.toDrive({
  image: lc_to,
  description: 'landcover_to_20m_res',
  fileFormat: 'GeoTIFF',
  crs: 'EPSG:26917',
  maxPixels: 654958084580,
  scale: 20
})
