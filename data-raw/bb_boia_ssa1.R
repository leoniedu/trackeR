## code to prepare `bb_boia_ssa1` dataset goes here

# bb_boia_ssa1 <- readr::read_csv("~/Downloads/layer-67bf29e90704c600518107vyk13ec71.csv", col_types = "n")%>%
#   filter(!is.na(Latitude))%>%
#   #mutate(Latitude=round(Latitude,4), Longitude=round(Longitude,4))%>%
#   sf::st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
#   st_bbox()
# constructive::construct_dput(bb_boia_ssa1)
bb_boia_ssa1 <- c(
  xmin = -38.61380,
  ymin = -13.00741,
  xmax = -38.46308,
  ymax = -12.81308
) |>
  structure(
    class = "bbox",
    crs = list(
      input = "EPSG:4326",
      wkt = 'GEOGCRS["WGS 84",\n    ENSEMBLE["World Geodetic System 1984 ensemble",\n        MEMBER["World Geodetic System 1984 (Transit)"],\n        MEMBER["World Geodetic System 1984 (G730)"],\n        MEMBER["World Geodetic System 1984 (G873)"],\n        MEMBER["World Geodetic System 1984 (G1150)"],\n        MEMBER["World Geodetic System 1984 (G1674)"],\n        MEMBER["World Geodetic System 1984 (G1762)"],\n        MEMBER["World Geodetic System 1984 (G2139)"],\n        ELLIPSOID["WGS 84",6378137,298.257223563,\n            LENGTHUNIT["metre",1]],\n        ENSEMBLEACCURACY[2.0]],\n    PRIMEM["Greenwich",0,\n        ANGLEUNIT["degree",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS["geodetic latitude (Lat)",north,\n            ORDER[1],\n            ANGLEUNIT["degree",0.0174532925199433]],\n        AXIS["geodetic longitude (Lon)",east,\n            ORDER[2],\n            ANGLEUNIT["degree",0.0174532925199433]],\n    USAGE[\n        SCOPE["Horizontal component of 3D system."],\n        AREA["World."],\n        BBOX[-90,-180,90,180]],\n    ID["EPSG",4326]]'
    ) |>
      structure(class = "crs")
  )



usethis::use_data(bb_boia_ssa1, overwrite = TRUE)
