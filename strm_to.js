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

// Import STRM dataset
var dataset = ee.Image('CSP/ERGo/1_0/Global/SRTM_topoDiversity');
var srtmTopographicDiversity = dataset.select('constant').clip(geometry);
var srtmTopographicDiversityVis = {
  min: 0,
  max: 1,
};

// Visualize
Map.setCenter(-79.3832, 43.6532, 9);
Map.addLayer(
    srtmTopographicDiversity, srtmTopographicDiversityVis,
    'SRTM Topographic Diversity');

// Export to Google Drive
Export.image.toDrive({
  image: srtmTopographicDiversity,
  description: 'strm_toronto_30m_res',
  scale: 30
})
