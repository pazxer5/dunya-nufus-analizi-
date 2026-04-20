# ============================================================
# CINEMA 3D RENDER - MILOS TARZI
# Gercek dunya DEM (elevatr) + Nufus yogunlugu renk overlay
# Kullanim: Rscript render_cinema.R [metrik] [yil]
# ============================================================
suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(elevatr)
  library(rayshader)
  library(data.table)
})
options(rgl.useNULL = TRUE)
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

args <- commandArgs(trailingOnly = TRUE)
metrik <- if (length(args) >= 1) args[1] else "yog"
yil    <- if (length(args) >= 2) as.integer(args[2]) else 2026L

cat("=== SINEMA 3D RENDER ===\nMetrik:", metrik, "| Yil:", yil, "\n")
t_start <- Sys.time()

# --- 1. VERI YUKLE ---
cat("[1/6] Veri yukleniyor...\n")
ulke_sf <- readRDS("data/ulke_harita.rds")
ref     <- readRDS("data/ulke_ref.rds")
pop_dt  <- readRDS("data/pop_yillik.rds")
tfr_dt  <- readRDS("data/tfr_yillik.rds")
e0_dt   <- readRDS("data/e0_yillik.rds")

ref_sub <- data.frame(country_code = ref$country_code, Alan_km2 = ref$Alan_km2)
ulke_sf <- merge(ulke_sf, ref_sub, by = "country_code", all.x = TRUE)

dt_y <- switch(metrik,
  pop = pop_dt[year == yil, .(country_code, value = pop / 1e6)],
  yog = merge(pop_dt[year == yil, .(country_code, pop)],
              data.table(ref_sub), by = "country_code")[,
              .(country_code, value = pop / Alan_km2)],
  tfr = tfr_dt[year == yil, .(country_code, value = tfr)],
  e0  = e0_dt[year == yil, .(country_code, value = e0B)],
  stop("Bilinmeyen metrik: ", metrik))
ulke_sf <- merge(ulke_sf, dt_y, by = "country_code", all.x = TRUE)

# Log donusumu
if (metrik %in% c("yog", "pop")) {
  ulke_sf$hval <- log10(pmax(ulke_sf$value, 0.5, na.rm = TRUE))
} else {
  ulke_sf$hval <- ulke_sf$value
}
ulke_sf <- ulke_sf[!is.na(ulke_sf$hval), ]

# --- 2. DEM INDIR (elevatr) ---
cache_dem <- "data/world_dem_z3.rds"
if (file.exists(cache_dem)) {
  cat("[2/6] DEM cache'den okunuyor...\n")
  elev_r <- readRDS(cache_dem)
} else {
  cat("[2/6] elevatr ile dunya DEM indiriliyor (z=3)...\n")
  # Dunya bbox (sf polygon olarak - elevatr bunu bekliyor)
  world_bbox_sf <- st_as_sf(
    st_as_sfc(st_bbox(c(xmin = -179, ymin = -58, xmax = 179, ymax = 78), crs = 4326))
  )
  # prj parametresi gerekli
  elev_r <- elevatr::get_elev_raster(
    locations = world_bbox_sf, z = 3, clip = "bbox", expand = 0,
    src = "aws", verbose = FALSE,
    prj = "+proj=longlat +datum=WGS84 +no_defs")
  saveRDS(elev_r, cache_dem)
  cat("   DEM cache'lendi:", cache_dem, "\n")
}
# terra SpatRaster'a cevir
elev_terra <- terra::rast(elev_r)

# --- 3. NUFUS YOGUNLUGU RASTER ---
cat("[3/6] Nufus yogunlugu rasterize ediliyor...\n")
# Ayni resolution'da population raster olustur
v <- terra::vect(ulke_sf)
pop_r <- terra::rasterize(v, elev_terra, field = "hval", fun = "mean")

# --- 4. MATRIS DONUSUMU ---
cat("[4/6] Matrisler hazirlaniyor...\n")
# Elevation matrisi
elmat <- raster_to_matrix(elev_r)
# Nufus yogunlugu matrisi (elmat ile ayni boyut)
pop_mat <- terra::as.matrix(pop_r, wide = TRUE)
pop_mat <- t(apply(pop_mat, 2, rev))

# Elevation normalize: deniz (<=0) icin bati degeri
ocean_mask <- is.na(pop_mat) | elmat < 0
# Kara yuksekligi korumali, deniz negatif
dem <- elmat
# Kucuk olceklemek icin
dem_land <- dem
dem_land[ocean_mask] <- -100

# --- 5. RENK KATMANI ---
cat("[5/6] Renk katmani uretiliyor...\n")
# Nufus yogunlugu renk paleti (Milos tarzi tonlar)
pop_palette <- colorRampPalette(c(
  "#065f46", "#059669", "#10b981", "#84cc16",  # dusuk: yesil tonlari
  "#facc15", "#f59e0b", "#f97316",              # orta: sari/turuncu
  "#dc2626", "#991b1b", "#450a0a"               # yuksek: kirmizi
))(256)

# Pop density normalize (kara icin)
pop_norm <- pop_mat
pr <- range(pop_norm, na.rm = TRUE)
pop_norm <- (pop_norm - pr[1]) / diff(pr)
pop_idx <- pmax(1, pmin(256, round(pop_norm * 255) + 1))

# Kara icin renk haritasi (4 kanal: R,G,B,A)
rgb_land <- t(col2rgb(pop_palette[pop_idx])) / 255
land_col <- array(NA_real_, dim = c(nrow(pop_mat), ncol(pop_mat), 4))
land_col[,,1] <- matrix(rgb_land[,1], nrow = nrow(pop_mat))
land_col[,,2] <- matrix(rgb_land[,2], nrow = nrow(pop_mat))
land_col[,,3] <- matrix(rgb_land[,3], nrow = nrow(pop_mat))
land_col[,,4] <- 1

# Okyanus: koyu mavi -> acik mavi gradient (derinlige gore)
ocean_palette <- colorRampPalette(c("#0c4a6e", "#075985", "#0369a1", "#0284c7", "#38bdf8"))(128)
if (any(ocean_mask)) {
  # Sahil uzakligi
  dist_r <- terra::distance(pop_r)
  dist_m <- terra::as.matrix(dist_r, wide = TRUE)
  dist_m <- t(apply(dist_m, 2, rev))
  dm_rng <- range(dist_m[ocean_mask], na.rm = TRUE)
  if (diff(dm_rng) > 0) {
    dist_norm <- pmin((dist_m - dm_rng[1]) / diff(dm_rng), 1)
    ocean_idx <- pmax(1, pmin(128, round(dist_norm * 127) + 1))
    rgb_ocean <- t(col2rgb(ocean_palette[ocean_idx])) / 255
    land_col[,,1][ocean_mask] <- matrix(rgb_ocean[,1], nrow = nrow(pop_mat))[ocean_mask]
    land_col[,,2][ocean_mask] <- matrix(rgb_ocean[,2], nrow = nrow(pop_mat))[ocean_mask]
    land_col[,,3][ocean_mask] <- matrix(rgb_ocean[,3], nrow = nrow(pop_mat))[ocean_mask]
  }
}

# Isiklandirma
cat("   Hillshade hesaplaniyor...\n")
amb <- ambient_shade(dem_land, zscale = 20, multicore = TRUE)
ray <- ray_shade(dem_land, sunaltitude = 45, sunangle = 295, zscale = 20, multicore = TRUE)

# --- 6. RENDER ---
cat("[6/6] 3D sahne render ediliyor (~1-2 dk)...\n")
if (!dir.exists("renders")) dir.create("renders")
out_path <- sprintf("renders/3d_%s_%d.png", metrik, yil)

dem_land %>%
  sphere_shade(texture = "imhof4", zscale = 20) %>%
  add_overlay(land_col, alphalayer = 0.88) %>%
  add_shadow(ray, 0.35) %>%
  add_shadow(amb, 0.25) %>%
  plot_3d(
    heightmap = dem_land,
    zscale = 25,
    solid = TRUE,
    soliddepth = -200,
    solidcolor = "#0f172a",
    solidlinecolor = "#0f172a",
    shadowdepth = -250,
    shadowcolor = "#64748b",
    background = "#f8fafc",
    windowsize = c(1800, 1100),
    zoom = 0.55, phi = 35, theta = -25,
    fov = 25,
    baseshape = "rectangle"
  )

Sys.sleep(0.8)
render_snapshot(filename = out_path, clear = TRUE)
rgl::close3d()

dt <- round(as.numeric(Sys.time() - t_start), 1)
cat("OK:", out_path, " (", dt, "sn )\n")
