# ============================================================
# VERI HAZIRLAMA SCRIPTI
# WPP2024 + NaturalEarth verilerini RDS olarak on-hesaplar
# Bir kez calistirilir, Shiny app sadece RDS okur
# ============================================================

suppressWarnings({
  lok <- tryCatch(Sys.setlocale("LC_ALL", "Turkish_T\u00fcrkiye.utf8"),
                  error = function(e) NULL, warning = function(w) NULL)
  if (is.null(lok) || lok == "") {
    tryCatch(Sys.setlocale("LC_CTYPE", ".UTF-8"),
             error = function(e) NULL, warning = function(w) NULL)
  }
})
options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 600)

library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(rmapshaper)
library(geodata)
library(terra)
library(exactextractr)
library(wpp2024)
library(data.table)

# Cikti klasoru
dir.create("data", showWarnings = FALSE)

# ============================================================
# 1. ULKE KODLARI
# ============================================================
cat("=== 1. Ulke kodlari yukleniyor ===\n")
data(UNlocations)
ulke_kodlari <- UNlocations$country_code[UNlocations$location_type == 4]
cat("Ulke sayisi:", length(ulke_kodlari), "\n")

# ============================================================
# 2. YILLIK VERI TABLOLARI (1950-2100)
# ============================================================

# --- 2a. Toplam nufus ---
cat("\n=== 2a. Toplam nufus verisi ===\n")
data(pop1dt)
data(popproj1dt)
cat("pop1dt yil araligi:", range(pop1dt$year), "\n")
cat("popproj1dt yil araligi:", range(popproj1dt$year), "\n")

pop_gecmis <- pop1dt[country_code %in% ulke_kodlari,
                      .(pop = sum(pop, na.rm = TRUE) * 1000),
                      by = .(country_code, name, year)]
pop_proj <- popproj1dt[country_code %in% ulke_kodlari,
                        .(pop = sum(pop, na.rm = TRUE) * 1000),
                        by = .(country_code, name, year)]
pop_dt <- rbind(pop_gecmis, pop_proj)
pop_dt <- unique(pop_dt, by = c("country_code", "year"))

# Sureklilik kontrolu: 2023 vs 2024
kontrol_2023 <- pop_dt[year == 2023, .(country_code, pop_2023 = pop)]
kontrol_2024 <- pop_dt[year == 2024, .(country_code, pop_2024 = pop)]
kontrol <- merge(kontrol_2023, kontrol_2024, by = "country_code")
kontrol[, degisim := abs(pop_2024 / pop_2023 - 1)]
cat("2023-2024 gecis: max yillik degisim:", round(max(kontrol$degisim, na.rm = TRUE) * 100, 2), "%\n")
if (max(kontrol$degisim, na.rm = TRUE) > 0.05) {
  warning("UYARI: 2023-2024 gecisinde %5'ten fazla sicrama var!")
}

# 2026 dunya nufusu kontrolu
dunya_2026 <- pop_dt[year == 2026, sum(pop, na.rm = TRUE)]
cat("Dunya nufusu (2026):", format(dunya_2026, big.mark = ",", scientific = FALSE), "\n")
if (dunya_2026 < 7.5e9 | dunya_2026 > 9e9) {
  warning("UYARI: 2026 dunya nufusu beklenen aralikta degil!")
}

cat("pop_dt: ", nrow(pop_dt), "satir,", uniqueN(pop_dt$country_code), "ulke,",
    range(pop_dt$year), "yil araligi\n")
saveRDS(pop_dt, "data/pop_yillik.rds")

# --- 2b. Yas piramidi verisi (cinsiyet ayrimli) ---
cat("\n=== 2b. Yas piramidi verisi ===\n")
data(popAge1dt)
data(popprojAge1dt)

# Sutun kontrolu
cat("popAge1dt sutunlar:", paste(names(popAge1dt), collapse = ", "), "\n")
cat("popprojAge1dt sutunlar:", paste(names(popprojAge1dt), collapse = ", "), "\n")

stopifnot("popM" %in% names(popAge1dt) & "popF" %in% names(popAge1dt))
stopifnot("popM" %in% names(popprojAge1dt) & "popF" %in% names(popprojAge1dt))
cat("popM ve popF sutunlari MEVCUT - piramit verisi hazirlanabilir\n")

age_gecmis <- popAge1dt[country_code %in% ulke_kodlari,
                         .(country_code, name, year, age, popM, popF, pop)]
age_proj <- popprojAge1dt[country_code %in% ulke_kodlari,
                           .(country_code, name, year, age, popM, popF, pop)]
age_dt <- rbind(age_gecmis, age_proj)
age_dt <- unique(age_dt, by = c("country_code", "year", "age"))
# binlik -> gercek
age_dt[, `:=`(popM = popM * 1000, popF = popF * 1000, pop = pop * 1000)]

cat("age_dt:", nrow(age_dt), "satir,", uniqueN(age_dt$country_code), "ulke,",
    "yas araligi:", range(age_dt$age), "\n")
saveRDS(age_dt, "data/age_pyramid.rds")

# --- 2c. Dogurganlik (TFR) ---
cat("\n=== 2c. TFR verisi ===\n")
data(tfr1dt)
data(tfrproj1dt)

tfr_gecmis <- tfr1dt[country_code %in% ulke_kodlari, .(country_code, name, year, tfr)]
tfr_proj <- tfrproj1dt[country_code %in% ulke_kodlari, .(country_code, name, year, tfr)]
tfr_dt <- rbind(tfr_gecmis, tfr_proj)
tfr_dt <- unique(tfr_dt, by = c("country_code", "year"))

cat("tfr_dt:", nrow(tfr_dt), "satir, yil:", range(tfr_dt$year), "\n")
cat("TFR ornek (Turkiye 2026):", tfr_dt[name == "Türkiye" & year == 2026, tfr], "\n")
saveRDS(tfr_dt, "data/tfr_yillik.rds")

# --- 2d. Yasam suresi ---
cat("\n=== 2d. Yasam suresi verisi ===\n")
data(e01dt)
data(e0proj1dt)

e0_gecmis <- e01dt[country_code %in% ulke_kodlari, .(country_code, name, year, e0M, e0F, e0B)]
e0_proj <- e0proj1dt[country_code %in% ulke_kodlari, .(country_code, name, year, e0M, e0F, e0B)]
e0_dt <- rbind(e0_gecmis, e0_proj)
e0_dt <- unique(e0_dt, by = c("country_code", "year"))

cat("e0_dt:", nrow(e0_dt), "satir, yil:", range(e0_dt$year), "\n")
saveRDS(e0_dt, "data/e0_yillik.rds")

# --- 2e. Net goc ---
cat("\n=== 2e. Net goc verisi ===\n")
data(mig1dt)
data(migproj1dt)

mig_gecmis <- mig1dt[country_code %in% ulke_kodlari, .(country_code, name, year, mig)]
# migproj1dt'de mig sutununu sec
mig_proj <- migproj1dt[country_code %in% ulke_kodlari, .(country_code, name, year, mig)]
mig_dt <- rbind(mig_gecmis, mig_proj)
mig_dt <- unique(mig_dt, by = c("country_code", "year"))

cat("mig_dt:", nrow(mig_dt), "satir, yil:", range(mig_dt$year), "\n")
saveRDS(mig_dt, "data/mig_yillik.rds")

# --- 2f. Diger demografik gostergeler (buyume, dogum, olum) ---
cat("\n=== 2f. Misc verisi ===\n")
data(misc1dt)
data(miscproj1dt)

ortak_sutunlar <- intersect(names(misc1dt), names(miscproj1dt))
cat("Ortak sutunlar:", paste(ortak_sutunlar, collapse = ", "), "\n")

misc_gecmis <- misc1dt[country_code %in% ulke_kodlari, ..ortak_sutunlar]
misc_proj <- miscproj1dt[country_code %in% ulke_kodlari, ..ortak_sutunlar]
misc_dt <- rbind(misc_gecmis, misc_proj)
misc_dt <- unique(misc_dt, by = c("country_code", "year"))

cat("misc_dt:", nrow(misc_dt), "satir, yil:", range(misc_dt$year), "\n")
saveRDS(misc_dt, "data/misc_yillik.rds")

# ============================================================
# 3. ULKE HARITA VERISI
# ============================================================
cat("\n=== 3. Ulke haritasi hazirlaniyor ===\n")
dunya_harita <- ne_countries(scale = "large", returnclass = "sf")  # 1:10m yuksek cozunurluk
dunya_harita$country_code_num <- as.integer(dunya_harita$un_a3)

# Kita Turkce isimleri
kita_tr <- c(
  "Asia" = "Asya", "Africa" = "Afrika", "Europe" = "Avrupa",
  "North America" = "Kuzey Amerika", "South America" = "Güney Amerika",
  "Oceania" = "Okyanusya", "Antarctica" = "Antarktika",
  "Seven seas (open ocean)" = "Okyanuslar"
)

# ISO numeric ile eslestir
harita_veri <- dunya_harita %>%
  left_join(
    pop_dt[year == 2026, .(country_code, name_wpp = name, pop_2026 = pop)],
    by = c("country_code_num" = "country_code")
  )

# Manuel isim eslestirme (mevcut scriptten)
isim_eslestirme <- c(
  "Czech Rep." = "Czechia",
  "Dem. Rep. Korea" = "Dem. People's Republic of Korea",
  "Korea" = "Republic of Korea",
  "Dem. Rep. Congo" = "Democratic Republic of the Congo",
  "Bosnia and Herz." = "Bosnia and Herzegovina",
  "Central African Rep." = "Central African Republic",
  "S. Sudan" = "South Sudan",
  "Dominican Rep." = "Dominican Republic",
  "Eq. Guinea" = "Equatorial Guinea",
  "W. Sahara" = "Western Sahara",
  "eSwatini" = "Eswatini",
  "Macedonia" = "North Macedonia",
  "Tanzania" = "United Republic of Tanzania",
  "Palestine" = "State of Palestine",
  "Côte d'Ivoire" = "Côte d'Ivoire",
  "Somaliland" = "Somalia",
  "N. Cyprus" = "Cyprus"
)

for (harita_isim in names(isim_eslestirme)) {
  bm_isim <- isim_eslestirme[harita_isim]
  bm_row <- pop_dt[name == bm_isim & year == 2026]
  if (nrow(bm_row) > 0) {
    idx <- which(harita_veri$name == harita_isim & is.na(harita_veri$pop_2026))
    if (length(idx) > 0) {
      harita_veri$country_code_num[idx] <- bm_row$country_code[1]
      harita_veri$pop_2026[idx] <- bm_row$pop[1]
    }
  }
}

# Alan hesapla
sf::sf_use_s2(FALSE)
on.exit(sf::sf_use_s2(TRUE), add = TRUE)
harita_veri$Alan_km2 <- as.numeric(sf::st_area(harita_veri)) / 1e6
sf::sf_use_s2(TRUE)

# Cifte sayim duzeltmesi: Somaliland/Somalia ve N.Cyprus/Cyprus
# Bu ulkeler ayni BM kodunu paylasir ama farkli poligonlara sahip.
# Nufusu alan oranina gore dagit.
cifte_ciftler <- list(
  list(ana = "Somalia", parca = "Somaliland"),
  list(ana = "Cyprus", parca = "N. Cyprus")
)
for (cift in cifte_ciftler) {
  idx_ana <- which(harita_veri$name == cift$ana & !is.na(harita_veri$pop_2026))
  idx_parca <- which(harita_veri$name == cift$parca & !is.na(harita_veri$pop_2026))
  if (length(idx_ana) == 1 && length(idx_parca) == 1) {
    toplam_nufus <- harita_veri$pop_2026[idx_ana]  # her iki poligona da ayni deger atanmisti
    alan_ana <- harita_veri$Alan_km2[idx_ana]
    alan_parca <- harita_veri$Alan_km2[idx_parca]
    toplam_alan <- alan_ana + alan_parca
    if (toplam_alan > 0) {
      harita_veri$pop_2026[idx_ana] <- toplam_nufus * (alan_ana / toplam_alan)
      harita_veri$pop_2026[idx_parca] <- toplam_nufus * (alan_parca / toplam_alan)
      cat("Cifte sayim duzeltildi:", cift$ana, "/", cift$parca, "\n")
    }
  }
}

eslesen <- sum(!is.na(harita_veri$pop_2026))
eslesmeyen_isimler <- harita_veri$name[is.na(harita_veri$pop_2026)]
cat("Eslesen ulke:", eslesen, "/", nrow(harita_veri), "\n")
if (length(eslesmeyen_isimler) > 0) {
  cat("ESLESMEYEN ULKELER:\n")
  for (u in eslesmeyen_isimler) cat("  -", u, "\n")
}

# Sadece gerekli sutunlari tut ve sadelestir
ulke_sf <- harita_veri %>%
  select(name, iso_a3, adm0_a3, country_code = country_code_num,
         continent, Alan_km2, geometry) %>%
  rmapshaper::ms_simplify(keep = 0.25, keep_shapes = TRUE)

cat("Ulke geometrisi sadellestirildi.\n")
saveRDS(ulke_sf, "data/ulke_harita.rds")

# Ulke referans tablosu (kod -> isim -> kita)
ulke_ref <- data.table(
  country_code = ulke_sf$country_code,
  name = ulke_sf$name,
  iso_a3 = ulke_sf$iso_a3,
  adm0_a3 = ulke_sf$adm0_a3,
  continent = ulke_sf$continent,
  continent_tr = kita_tr[ulke_sf$continent],
  Alan_km2 = ulke_sf$Alan_km2
)
ulke_ref <- ulke_ref[!is.na(country_code)]
ulke_ref <- unique(ulke_ref, by = "country_code")
cat("ulke_ref:", nrow(ulke_ref), "ulke\n")
saveRDS(ulke_ref, "data/ulke_ref.rds")

# ============================================================
# 4. EYALET HARITA VERISI
# ============================================================
cat("\n=== 4. Eyalet haritasi hazirlaniyor ===\n")
eyalet_harita <- ne_states(returnclass = "sf")
cat("Eyalet sayisi:", nrow(eyalet_harita), "\n")

# Raster nufus verisi
cat("Raster verisi yukleniyor...\n")
pop_raster_path <- file.path(getwd(), "geodata_cache")
dir.create(pop_raster_path, showWarnings = FALSE)
pop_raster <- geodata::population(year = 2020, res = 10, path = pop_raster_path)

eyalet_harita <- sf::st_make_valid(eyalet_harita)

cat("Hucre alanlari ve nufus hesaplaniyor...\n")
hucre_alan <- terra::cellSize(pop_raster, unit = "km")
pop_count <- pop_raster * hucre_alan
eyalet_harita$Nufus <- exactextractr::exact_extract(pop_count, eyalet_harita, 'sum')

sf::sf_use_s2(FALSE)
eyalet_harita$alan_km2 <- as.numeric(sf::st_area(eyalet_harita)) / 1e6
sf::sf_use_s2(TRUE)

eyalet_harita$Nufus[!is.finite(eyalet_harita$Nufus) | eyalet_harita$Nufus < 0] <- NA

# 2026'ya olcekle
eyalet_harita$buyume_carpani <- NA_real_
for (i in seq_len(nrow(eyalet_harita))) {
  adm <- eyalet_harita$adm0_a3[i]
  idx <- which(ulke_sf$adm0_a3 == adm)
  if (length(idx) > 0) {
    cc <- ulke_sf$country_code[idx[1]]
    if (!is.na(cc)) {
      p2020 <- pop_dt[country_code == cc & year == 2020, pop][1]
      p2026 <- pop_dt[country_code == cc & year == 2026, pop][1]
      if (length(p2020) == 1 && length(p2026) == 1 &&
          !is.na(p2020) && !is.na(p2026) && p2020 > 0) {
        eyalet_harita$buyume_carpani[i] <- p2026 / p2020
      }
    }
  }
}
eyalet_harita$Nufus <- eyalet_harita$Nufus * eyalet_harita$buyume_carpani
eyalet_harita$Yogunluk <- eyalet_harita$Nufus / eyalet_harita$alan_km2
eyalet_harita$Yogunluk[!is.finite(eyalet_harita$Yogunluk) | eyalet_harita$Yogunluk < 0] <- NA

# Sadelestir ve kaydet
eyalet_sf <- eyalet_harita %>%
  select(name, admin, adm0_a3, type_en, Nufus, alan_km2, Yogunluk, geometry) %>%
  rmapshaper::ms_simplify(keep = 0.01, keep_shapes = TRUE)

cat("Eyalet geometrisi sadellestirildi:", nrow(eyalet_sf), "birim\n")
saveRDS(eyalet_sf, "data/eyalet_harita.rds")

# ============================================================
# 5. DOGRULAMA OZETI
# ============================================================
cat("\n========================================\n")
cat("VERI HAZIRLAMA TAMAMLANDI\n")
cat("========================================\n")

dosyalar <- list.files("data", pattern = "\\.rds$", full.names = TRUE)
for (f in dosyalar) {
  boyut <- file.size(f) / 1024 / 1024
  cat(sprintf("  %-25s  %.1f MB\n", basename(f), boyut))
}

cat("\nDogrulama:\n")
cat("  Dunya nufusu 2026:", formatC(dunya_2026, format = "d", big.mark = ","), "\n")
cat("  Ulke harita eslesmesi:", eslesen, "/", nrow(harita_veri), "\n")
cat("  Yas verisi cinsiyet ayrimli: EVET\n")
cat("  Yil araligi: 1950-2100\n")
cat("========================================\n")
