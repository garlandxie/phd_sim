// Import data
var dataset = ee.Image('CSP/ERGo/1_0/Global/SRTM_topoDiversity');

// Clip to boundary of TO
var geometry = ee.FeatureCollection("projects/ee-garlandxie/assets/to_boundary")
                 .geometry();
                 
var srtmTopographicDiversity = dataset.select('constant').clip(geometry);

// Visualize data                
var srtmTopographicDiversityVis = {
  min: 0,
  max: 1,
};
Map.setCenter(-79.3832, 43.6532, 9);
Map.addLayer(
    srtmTopographicDiversity, srtmTopographicDiversityVis,
    'SRTM Topographic Diversity');

// Export to Google Drive 
Export.image.toDrive({
  image: srtmTopographicDiversity,
  crs: 'EPSG:26917',
  description: 'strm_to_3m_res',
  folder: "google earth engine",
  fileFormat: 'GeoTIFF',
  maxPixels: 209242992,
  scale: 2.5
});
