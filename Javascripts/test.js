var sussex_map_vaccine_sites_leaf = L.map("map_vaccine_sites");

var overlayMaps_all_age = {
  "Show Pharmacy sites": pharmacy_site_markers,
  "Show GP led sites": gp_led_markers,
  "Show Hospital hub sites": hospital_hub_markers,
  "Show Vaccination centre sites": vaccination_centre_markers,
};

var basemap_msoa_vaccine = L.tileLayer(tileUrl, {
  attribution,
  minZoom: 8,
}).addTo(sussex_map_vaccine_sites_leaf);

L.control
  // .layers(baseMaps_all_age, null, { collapsed: false })
  .layers(null, overlayMaps_all_age, { collapsed: false })
  .addTo(sussex_map_vaccine_sites_leaf);

sussex_map_vaccine_sites_leaf.fitBounds(
  msoa_vaccine_all_age_1_count_map_layer.getBounds()
);
