/**
 * @description
 *    Calculates area by class id and year using shapefiles
 * 
 * @author
 *    João Siqueira
 * 
 * @changes in code by
 *     Tainá Rocha
 * 
 */

// Asset mapbiomas
var asset = "projects/mapbiomas-workspace/public/collection7/mapbiomas_collection70_integration_v2";

// Asset of regions for which you want to calculate statistics
var assetTerritories = "users/taina013/bioma";

// Numeric attribute to index the shapefile
var attribute = "objectid";

// A list of class ids you are interested
var classIds = [
    3, // Formação Florestal Forest Formation
    4, // Formação Savânica Savanna Formation
    5, // Mangue Mangrove
    49, // Restinga Arborizada Wooded Sandbank Vegetation
    11, // Campo Alagado e Área Pantanosa Wetland
    12, // Formação Campestre Grassland
    32, // Apicum Salt Flat
    29, // Afloramento Rochoso Rocky Outcrop
    50, // Restinga Herbácea 
    13, // Outras Formações não Florestais Other non Forest Formations
    15, // Pastagem Pasture
    19, // Lavoura Temporária Temporary Crop
    39, // Soja Soybean
    20, // Cana Sugar cane
    40, // Arroz (beta) Rice
    62, // Algodão (beta) Cotton
    41, // Outras Lavouras Temporárias Other Temporary Crops
    36, // Lavoura Perene Perennial Crop
    46, // Café Coffee
    47, // Citrus Citrus
    48, // Outras Lavouras Perenes Other Perennial Crops
    9, // Silvicultura Forest Plantation
    21, // Mosaico de Agricultura ou Pastagem Mosaic of Uses
    23, // Praia e Duna Beach, Dune and Sand Spot
    24, // Infraestrutura Urbana Urban Area
    30, // Mineração Mining
    25, // Outra Área não Vegetada Other non Vegetated Areas
    33, // Rio, Lago e Oceano River, Lake and Ocean
    31 // 'Aquicultura Aquaculture
   
];

// Output csv name
var outputName = 'Bioma_7_20_21';

// Change the scale if you need.
var scale = 30;

// Define a list of years to export
var years = [
    '2020', '2021'
];

// Define a Google Drive output folder 
var driverFolder = 'AREA-EXPORT';

/**
 * 
 */
// Territory
var territory = ee.FeatureCollection(assetTerritories);

// LULC mapbiomas image
var mapbiomas = ee.Image(asset).selfMask();

// Image area in km2
var pixelArea = ee.Image.pixelArea().divide(1000000);

// Geometry to export
var geometry = mapbiomas.geometry();

/**
 * Convert a complex ob to feature collection
 * @param obj 
 */
var convert2table = function (obj) {

    obj = ee.Dictionary(obj);

    var territory = obj.get('territory');

    var classesAndAreas = ee.List(obj.get('groups'));

    var tableRows = classesAndAreas.map(
        function (classAndArea) {
            classAndArea = ee.Dictionary(classAndArea);

            var classId = classAndArea.get('class');
            var area = classAndArea.get('sum');

            var tableColumns = ee.Feature(null)
                .set(attribute, territory)
                .set('class', classId)
                .set('area', area);

            return tableColumns;
        }
    );

    return ee.FeatureCollection(ee.List(tableRows));
};

/**
 * Calculate area crossing a cover map (deforestation, mapbiomas)
 * and a region map (states, biomes, municipalites)
 * @param image 
 * @param territory 
 * @param geometry
 */
var calculateArea = function (image, territory, geometry) {

    var reducer = ee.Reducer.sum().group(1, 'class').group(1, 'territory');

    var territotiesData = pixelArea.addBands(territory).addBands(image)
        .reduceRegion({
            reducer: reducer,
            geometry: geometry,
            scale: scale,
            maxPixels: 1e12
        });

    territotiesData = ee.List(territotiesData.get('groups'));

    var areas = territotiesData.map(convert2table);

    areas = ee.FeatureCollection(areas).flatten();

    return areas;
};

var areas = years.map(
    function (year) {
        var image = mapbiomas.select('classification_' + year);

        var areas = territory.map(
            function (feature) {
                return calculateArea(
                    image.remap(classIds, classIds, 0),
                    ee.Image().int64().paint({
                        'featureCollection': ee.FeatureCollection(feature),
                        'color': attribute
                    }),
                    feature.geometry()
                );
            }
        );

        areas = areas.flatten();

        // set additional properties
        areas = areas.map(
            function (feature) {
                return feature.set('year', year);
            }
        );

        return areas;
    }
);

areas = ee.FeatureCollection(areas).flatten();

Map.addLayer(territory);

Export.table.toDrive({
    collection: areas,
    description: outputName,
    folder: driverFolder,
    fileNamePrefix: outputName,
    fileFormat: 'CSV'
});
