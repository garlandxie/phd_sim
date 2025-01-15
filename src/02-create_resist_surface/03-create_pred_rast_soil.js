var dataset = ee.Image('projects/ee-garlandxie/assets/ontario_wspd_10m');

// Clip to boundary of TO
var geometry = ee.FeatureCollection("projects/ee-garlandxie/assets/to_boundary")
                 .geometry();
var wind_to = dataset.clip(geometry);

// Visualize
var wind_vis = {
  min: 0.854656457901001,
  max: 12.806132316589355
};
Map.setCenter(-79.3832, 43.6532, 9);
Map.addLayer(wind_to, wind_vis);

// Export to Google Drive
Export.image.toDrive({
  image: wind_to,
  folder: "google earth engine",
  crs: 'EPSG:26917',
  description: 'wind_to_3m_res',
  maxPixels: 209242992,
  scale: 2.5
});

