var sussex_map_vaccine_sites_leaf = L.map("map_vaccine_sites");

var myIconClass = L.Icon.extend({
  options: {
    shadowUrl:
      "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
    iconSize: [25, 41],
    iconAnchor: [12, 41],
    popupAnchor: [1, -34],
    shadowSize: [41, 41],
  },
});

var pharm_icon = new myIconClass({
    iconUrl:
      "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
  }),
  gp_icon = new myIconClass({
    iconUrl:
      "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-blue.png",
  }),
  hh_icon = new myIconClass({
    iconUrl:
      "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
  }),
  vac_site_icon = new myIconClass({
    iconUrl:
      "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-violet.png",
  });

pharmacy_vac_sites = sussex_vaccination_sites.filter(function (d) {
  return d.Type === "Pharmacies";
});

gp_vac_sites = sussex_vaccination_sites.filter(function (d) {
  return d.Type === "GP led service";
});

hh_vac_sites = sussex_vaccination_sites.filter(function (d) {
  return d.Type === "Hospital Hub";
});

vaccine_centre_vac_sites = sussex_vaccination_sites.filter(function (d) {
  return d.Type === "Vaccination centre";
});

// If you want to create a group of markers you want to hide/show you need to add them to a layergroup inside the loop, then add the layergroup to the map
pharmacy_site_markers = L.layerGroup();
for (item of pharmacy_vac_sites) {
  pharm_mark_item = L.marker([item.latitude, item.longitude], {
    icon: pharm_icon,
  })
    .bindPopup(
      "<p><b>" +
        item.Site +
        " (" +
        item.LTLA +
        ")</b></p><p>Address: " +
        item.Address +
        " " +
        item.Postcode +
        "</p><p>This is a pharmacy led vaccination site.</p>"
    )
    .addTo(pharmacy_site_markers);
}
pharmacy_site_markers.addTo(sussex_map_vaccine_sites_leaf);

gp_led_markers = L.layerGroup();
for (item of gp_vac_sites) {
  gp_mark_item = L.marker([item.latitude, item.longitude], {
    icon: gp_icon,
  })
    .bindPopup(
      "<p><b>" +
        item.Site +
        " (" +
        item.LTLA +
        ")</b></p><p>Address: " +
        item.Address +
        " " +
        item.Postcode +
        "</p><p>This is a GP led vaccination site.</p>"
    )
    .addTo(gp_led_markers);
}
gp_led_markers.addTo(sussex_map_vaccine_sites_leaf);

hospital_hub_markers = L.layerGroup();
for (item of hh_vac_sites) {
  hh_mark_item = L.marker([item.latitude, item.longitude], {
    icon: hh_icon,
  })
    .bindPopup(
      "<p><b>" +
        item.Site +
        " (" +
        item.LTLA +
        ")</b></p><p>Address: " +
        item.Address +
        " " +
        item.Postcode +
        "</p><p>This is a hospital hub vaccination site.</p>"
    )
    .addTo(hospital_hub_markers);
}
hospital_hub_markers.addTo(sussex_map_vaccine_sites_leaf);

vaccination_centre_markers = L.layerGroup();
for (item of vaccine_centre_vac_sites) {
  vac_centre_mark_item = L.marker([item.latitude, item.longitude], {
    icon: vac_site_icon,
  })
    .bindPopup(
      "<p><b>" +
        item.Site +
        " (" +
        item.LTLA +
        ")</b></p><p>Address: " +
        item.Address +
        " " +
        item.Postcode +
        "</p><p>This is a vaccination centre site.</p>"
    )
    .addTo(vaccination_centre_markers);
}
vaccination_centre_markers.addTo(sussex_map_vaccine_sites_leaf);

var overlayMaps_sites = {
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
  .layers(null, overlayMaps_sites, { collapsed: false })
  .addTo(sussex_map_vaccine_sites_leaf);

// sussex_map_vaccine_sites_leaf.fitBounds(
//   msoa_vaccine_all_age_1_count_map_layer.getBounds()
// );
