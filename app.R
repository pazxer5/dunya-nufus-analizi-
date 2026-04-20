# ============================================================
#  DUNYA NUFUS ANALIZI PANELI  —  ANA GIRIS DOSYASI (app.R)
# ============================================================
#  Interaktif cografya/demografi dashboard'u (R Shiny)
#  Veri: BM WPP 2024 (1950-2100) + Worldometer 2025
#
#  Calistirma:
#    R icinde:  shiny::runApp('.')
#    veya terminal:  Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
#
#  Ozellikler:
#    - Interaktif dunya haritasi (6 metrik: nufus, yogunluk, buyume, TFR,
#      yasam suresi, net goc) — yil slider'i 1950-2100
#    - Tematik haritalar (goc, dogurganlik, yasam suresi, buyume,
#      demografik gecis, bagimlilik orani, medyan yas, sehirlesme)
#    - Yas piramidi karsilastirma (2 ulke yan yana)
#    - Ulke karsilastirma (en fazla 4 ulke, zaman serileri + piramitler)
#    - Yogunluk-buyume sacilim grafigi
#    - 3D dunya haritasi (MapLibre) + cinema-kalitesinde rayshader render
#    - Aranabilir/filtrelenebilir veri tablosu (CSV indirme)
#
#  Bagimli paketler: app.R asagida otomatik yukler.
#  Onceden hazirlanmis veri: data/*.rds (data_hazirla.R ile uretildi)
# ============================================================

# UTF-8 encoding garantile (Windows uzerinde Turkce karakter sorunu icin)
suppressWarnings({
  trySetLocale <- tryCatch(Sys.setlocale("LC_CTYPE", "Turkish_Turkey.65001"),
                            error = function(e) NULL, warning = function(w) NULL)
  if (is.null(trySetLocale) || trySetLocale == "") {
    tryCatch(Sys.setlocale("LC_CTYPE", ".UTF-8"),
             error = function(e) NULL, warning = function(w) NULL)
  }
})
options(encoding = "UTF-8")
# Tarayicida ac (RStudio Viewer yerine)
options(shiny.launch.browser = TRUE)
# Dosya degisince otomatik reload
options(shiny.autoreload = TRUE)
# Hatalari kullaniciya gostermeden sessizce gecistir (kullanici istegi uzerine)
options(shiny.sanitize.errors = TRUE)
options(warn = -1)

library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(DT)
library(sf)
library(data.table)
library(viridisLite)
library(dplyr)
library(htmlwidgets)
.has_mapgl <- requireNamespace("mapgl", quietly = TRUE)
if (.has_mapgl) suppressPackageStartupMessages(library(mapgl))

# Opsiyonel paketler - yukse kullan, yoksa sessizce atla
.has_spinner  <- requireNamespace("shinycssloaders", quietly = TRUE)
.has_waiter   <- requireNamespace("waiter", quietly = TRUE)
.has_lfxtras  <- requireNamespace("leaflet.extras", quietly = TRUE)
.has_bsicons  <- requireNamespace("bsicons", quietly = TRUE)
.has_echarts  <- requireNamespace("echarts4r", quietly = TRUE)
if (.has_echarts) suppressPackageStartupMessages(library(echarts4r))

withSpin <- function(x, color = "#6366f1") {
  if (.has_spinner) shinycssloaders::withSpinner(x, type = 6, color = color, size = 0.8) else x
}
bsi <- function(name, ...) {
  if (.has_bsicons) bsicons::bs_icon(name, ...) else icon(name)
}

# ============================================================
# GLOBAL VERI YUKLEME (bir kez, tum sessionlar icin)
# ============================================================
.safe_read <- function(path) tryCatch(readRDS(path), error = function(e) NULL)
ulke_sf   <- .safe_read("data/ulke_harita.rds")
pop_dt    <- .safe_read("data/pop_yillik.rds")
age_dt    <- .safe_read("data/age_pyramid.rds")
tfr_dt    <- .safe_read("data/tfr_yillik.rds")
e0_dt     <- .safe_read("data/e0_yillik.rds")
mig_dt    <- .safe_read("data/mig_yillik.rds")
misc_dt   <- .safe_read("data/misc_yillik.rds")
ulke_ref  <- .safe_read("data/ulke_ref.rds")
eyalet_sf <- tryCatch(readRDS("data/eyalet_harita.rds"), error = function(e) NULL)
if (!is.null(eyalet_sf)) cat("Eyalet verisi yuklendi:", nrow(eyalet_sf), "birim\n")

# Worldometer 2025 verisi (medyan yas, sehir nufus % gibi ek metrikler)
wmeter_raw <- tryCatch(
  fread("guncel_population.csv", encoding = "UTF-8"),
  error = function(e) NULL
)
if (!is.null(wmeter_raw)) {
  # Beklenen sutunlarin varligini dogrula
  beklenen_sutunlar <- c("Country (or dependency)", "Population 2025", "Median Age", "Urban Pop %",
                          "Fert. Rate", "Density (P/Km\u00b2)")
  mevcut <- beklenen_sutunlar %in% names(wmeter_raw)
  if (sum(mevcut) < 3) {
    cat("UYARI: guncel_population.csv beklenen sutunlara sahip degil, atlanıyor.\n")
    wmeter_raw <- NULL
  }
  if (!is.null(wmeter_raw)) setnames(wmeter_raw,
           beklenen_sutunlar[mevcut],
           c("wm_name", "wm_pop", "wm_median_age", "wm_urban_pct", "wm_tfr", "wm_density")[mevcut],
           skip_absent = TRUE)
  # Worldometer ingilizce isim -> WPP country_code eslestirmesi
  wm_isim_eslestir <- c(
    "Turkey" = "Turkiye",
    "United States" = "United States of America",
    "DR Congo" = "Democratic Republic of the Congo",
    "Tanzania" = "United Republic of Tanzania",
    "South Korea" = "Republic of Korea",
    "North Korea" = "Dem. People's Republic of Korea",
    "Czech Republic (Czechia)" = "Czechia",
    "State of Palestine" = "State of Palestine",
    "Bolivia" = "Bolivia (Plurinational State of)",
    "Iran" = "Iran (Islamic Republic of)",
    "Vietnam" = "Viet Nam",
    "Russia" = "Russian Federation",
    "Syria" = "Syrian Arab Republic",
    "Moldova" = "Republic of Moldova",
    "Tanzania" = "United Republic of Tanzania",
    "Brunei" = "Brunei Darussalam",
    "Hong Kong" = "China, Hong Kong SAR",
    "Macao" = "China, Macao SAR",
    "Taiwan" = "China, Taiwan Province of China",
    "Laos" = "Lao People's Democratic Republic",
    "Cabo Verde" = "Cabo Verde",
    "Sao Tome & Principe" = "Sao Tome and Principe",
    "St. Vincent & Grenadines" = "Saint Vincent and the Grenadines",
    "Saint Kitts & Nevis" = "Saint Kitts and Nevis",
    "Falkland Islands" = "Falkland Islands (Malvinas)",
    "Wallis & Futuna" = "Wallis and Futuna Islands",
    "Turks and Caicos" = "Turks and Caicos Islands",
    "British Virgin Islands" = "British Virgin Islands",
    "Micronesia" = "Micronesia (Fed. States of)",
    "Venezuela" = "Venezuela (Bolivarian Republic of)"
  )
  # WPP isim -> country_code map
  wpp_kod <- pop_dt[year == 2025, setNames(country_code, name)]
  # Eslestir
  wmeter_raw[, wpp_isim := ifelse(wm_name %in% names(wm_isim_eslestir),
                                    wm_isim_eslestir[wm_name], wm_name)]
  wmeter_raw[, country_code := wpp_kod[wpp_isim]]
  # Urban pop %'i sayiya cevir
  wmeter_raw[, wm_urban_pct := as.numeric(gsub("%", "", wm_urban_pct))]
  wmeter_dt <- wmeter_raw[!is.na(country_code),
                          .(country_code, wm_pop, wm_median_age, wm_urban_pct, wm_tfr, wm_density)]
  cat("Worldometer veri eslesmesi:", nrow(wmeter_dt), "/", nrow(wmeter_raw), "ulke\n")
} else {
  wmeter_dt <- data.table(country_code = integer(), wm_median_age = numeric(),
                           wm_urban_pct = numeric())
}

# Eksik ulke kodlarini manuel duzelt (Natural Earth -99 dondurdugu icin)
# adm0_a3 (ISO3) -> WPP country_code eslestirmesi
.eksik_kodlar <- list(
  "NOR" = 578L,  # Norway
  "KOS" = 412L,  # Kosovo
  "TWN" = 158L,  # Taiwan (China, Taiwan Province of China)
  "FRA" = 250L,  # France (bazi NE versiyonlarinda -99)
  "PSE" = 275L,  # Palestine (State of Palestine)
  "SDS" = 728L,  # South Sudan (S. Sudan)
  "SOL" = 706L,  # Somaliland -> Somalia
  "CYN" = 196L   # N. Cyprus -> Cyprus
)
for (iso3 in names(.eksik_kodlar)) {
  idx_sf <- which(ulke_sf$adm0_a3 == iso3 & (is.na(ulke_sf$country_code) | ulke_sf$country_code == -99))
  if (length(idx_sf) > 0) ulke_sf$country_code[idx_sf] <- .eksik_kodlar[[iso3]]
  idx_ref <- which(ulke_ref$adm0_a3 == iso3 & (is.na(ulke_ref$country_code) | ulke_ref$country_code == -99))
  if (length(idx_ref) > 0) ulke_ref[idx_ref, country_code := .eksik_kodlar[[iso3]]]
}
# -99'lari NA yap (kalan ulkeler icin)
ulke_sf$country_code[ulke_sf$country_code == -99] <- NA_integer_
ulke_ref[country_code == -99, country_code := NA_integer_]

# Kita Turkce isimleri
kita_tr <- c(
  "Asia" = "Asya", "Africa" = "Afrika", "Europe" = "Avrupa",
  "North America" = "Kuzey Amerika", "South America" = "G\u00fcney Amerika",
  "Oceania" = "Okyanusya", "Antarctica" = "Antarktika",
  "Seven seas (open ocean)" = "Okyanuslar"
)

# Ulke Turkce isim sozlugu (Natural Earth + WPP2024 isimleri -> T\u00fcrk\u00e7e)
ulke_tr <- c(
  "Afghanistan" = "Afganistan", "Albania" = "Arnavutluk", "Algeria" = "Cezayir",
  "Andorra" = "Andorra", "Angola" = "Angola", "Antigua and Barb." = "Antigua ve Barbuda",
  "Argentina" = "Arjantin", "Armenia" = "Ermenistan", "Australia" = "Avustralya",
  "Austria" = "Avusturya", "Azerbaijan" = "Azerbaycan", "Bahamas" = "Bahamalar",
  "Bahrain" = "Bahreyn", "Bangladesh" = "Banglade\u015f", "Barbados" = "Barbados",
  "Belarus" = "Belarus", "Belgium" = "Bel\u00e7ika", "Belize" = "Belize",
  "Benin" = "Benin", "Bhutan" = "Butan", "Bolivia" = "Bolivya",
  "Bosnia and Herz." = "Bosna Hersek", "Bosnia and Herzegovina" = "Bosna Hersek",
  "Botswana" = "Botsvana", "Brazil" = "Brezilya", "Brunei" = "Brunei",
  "Bulgaria" = "Bulgaristan", "Burkina Faso" = "Burkina Faso", "Burundi" = "Burundi",
  "Cambodia" = "Kambo\u00e7ya", "Cameroon" = "Kamerun", "Canada" = "Kanada",
  "Cape Verde" = "Ye\u015fil Burun", "Cabo Verde" = "Ye\u015fil Burun",
  "Central African Rep." = "Orta Afrika Cumhuriyeti",
  "Central African Republic" = "Orta Afrika Cumhuriyeti",
  "Chad" = "\u00c7ad", "Chile" = "\u015eili", "China" = "\u00c7in", "Colombia" = "Kolombiya",
  "Comoros" = "Komorlar", "Congo" = "Kongo",
  "Costa Rica" = "Kosta Rika", "Croatia" = "H\u0131rvatistan", "Cuba" = "K\u00fcba",
  "Cyprus" = "K\u0131br\u0131s", "Czech Rep." = "\u00c7ekya", "Czechia" = "\u00c7ekya",
  "Côte d'Ivoire" = "Fildi\u015fi Sahili", "Cote d'Ivoire" = "Fildi\u015fi Sahili",
  "Dem. Rep. Congo" = "Demokratik Kongo Cumhuriyeti",
  "Democratic Republic of the Congo" = "Demokratik Kongo Cumhuriyeti",
  "Dem. Rep. Korea" = "Kuzey Kore",
  "Dem. People's Republic of Korea" = "Kuzey Kore",
  "Denmark" = "Danimarka", "Djibouti" = "Cibuti", "Dominica" = "Dominika",
  "Dominican Rep." = "Dominik Cumhuriyeti", "Dominican Republic" = "Dominik Cumhuriyeti",
  "Ecuador" = "Ekvador", "Egypt" = "M\u0131s\u0131r", "El Salvador" = "El Salvador",
  "Eq. Guinea" = "Ekvator Ginesi", "Equatorial Guinea" = "Ekvator Ginesi",
  "Eritrea" = "Eritre", "Estonia" = "Estonya", "Eswatini" = "Esvatini",
  "eSwatini" = "Esvatini", "Ethiopia" = "Etiyopya",
  "Falkland Is." = "Falkland Adalar\u0131", "Faeroe Is." = "Faroe Adalar\u0131",
  "Fiji" = "Fiji", "Finland" = "Finlandiya", "France" = "Fransa",
  "Fr. Polynesia" = "Frans\u0131z Polinezyas\u0131", "French Polynesia" = "Frans\u0131z Polinezyas\u0131",
  "Gabon" = "Gabon", "Gambia" = "Gambiya", "Georgia" = "G\u00fcrcistan",
  "Germany" = "Almanya", "Ghana" = "Gana", "Greece" = "Yunanistan",
  "Greenland" = "Gr\u00f6nland", "Grenada" = "Grenada", "Guam" = "Guam",
  "Guatemala" = "Guatemala", "Guinea" = "Gine", "Guinea-Bissau" = "Gine-Bissau",
  "Guyana" = "Guyana", "Haiti" = "Haiti", "Honduras" = "Honduras",
  "Hong Kong" = "Hong Kong", "Hungary" = "Macaristan", "Iceland" = "\u0130zlanda",
  "India" = "Hindistan", "Indonesia" = "Endonezya", "Iran" = "\u0130ran",
  "Iraq" = "Irak", "Ireland" = "\u0130rlanda", "Israel" = "\u0130srail",
  "Italy" = "\u0130talya", "Jamaica" = "Jamaika", "Japan" = "Japonya",
  "Jordan" = "\u00dcrd\u00fcn", "Kazakhstan" = "Kazakistan", "Kenya" = "Kenya",
  "Kiribati" = "Kiribati", "Korea" = "G\u00fcney Kore", "Republic of Korea" = "G\u00fcney Kore",
  "Kosovo" = "Kosova", "Kuwait" = "Kuveyt", "Kyrgyzstan" = "K\u0131rg\u0131zistan",
  "Laos" = "Laos", "Latvia" = "Letonya", "Lebanon" = "L\u00fcbnan",
  "Lesotho" = "Lesoto", "Liberia" = "Liberya", "Libya" = "Libya",
  "Liechtenstein" = "Lihten\u015ftayn", "Lithuania" = "Litvanya", "Luxembourg" = "L\u00fcksemburg",
  "Macao" = "Makao", "Macedonia" = "Kuzey Makedonya", "North Macedonia" = "Kuzey Makedonya",
  "Madagascar" = "Madagaskar", "Malawi" = "Malavi", "Malaysia" = "Malezya",
  "Maldives" = "Maldivler", "Mali" = "Mali", "Malta" = "Malta",
  "Marshall Is." = "Marshall Adalar\u0131", "Mauritania" = "Moritanya",
  "Mauritius" = "Mauritius", "Mexico" = "Meksika", "Micronesia" = "Mikronezya",
  "Moldova" = "Moldova", "Monaco" = "Monako", "Mongolia" = "Mo\u011folistan",
  "Montenegro" = "Karada\u011f", "Morocco" = "Fas", "Mozambique" = "Mozambik",
  "Myanmar" = "Myanmar", "N. Cyprus" = "K\u0131br\u0131s (Kuzey)", "Namibia" = "Namibya",
  "Nauru" = "Nauru", "Nepal" = "Nepal", "Netherlands" = "Hollanda",
  "New Caledonia" = "Yeni Kaledonya", "New Zealand" = "Yeni Zelanda",
  "Nicaragua" = "Nikaragua", "Niger" = "Nijer", "Nigeria" = "Nijerya",
  "Norway" = "Norve\u00e7", "Oman" = "Umman", "Pakistan" = "Pakistan",
  "Palau" = "Palau", "Palestine" = "Filistin", "State of Palestine" = "Filistin",
  "Panama" = "Panama", "Papua New Guinea" = "Papua Yeni Gine",
  "Paraguay" = "Paraguay", "Peru" = "Peru", "Philippines" = "Filipinler",
  "Poland" = "Polonya", "Portugal" = "Portekiz", "Puerto Rico" = "Porto Riko",
  "Qatar" = "Katar", "Romania" = "Romanya", "Russia" = "Rusya",
  "Russian Federation" = "Rusya", "Rwanda" = "Ruanda",
  "S. Sudan" = "G\u00fcney Sudan", "South Sudan" = "G\u00fcney Sudan",
  "Samoa" = "Samoa", "San Marino" = "San Marino", "Saudi Arabia" = "Suudi Arabistan",
  "Senegal" = "Senegal", "Serbia" = "S\u0131rbistan", "Seychelles" = "Sey\u015feller",
  "Sierra Leone" = "Sierra Leone", "Singapore" = "Singapur", "Slovakia" = "Slovakya",
  "Slovenia" = "Slovenya", "Solomon Is." = "Solomon Adalar\u0131",
  "Somalia" = "Somali", "Somaliland" = "Somaliland",
  "South Africa" = "G\u00fcney Afrika", "Spain" = "\u0130spanya", "Sri Lanka" = "Sri Lanka",
  "Sudan" = "Sudan", "Suriname" = "Surinam", "Sweden" = "\u0130sve\u00e7",
  "Switzerland" = "\u0130svi\u00e7re", "Syria" = "Suriye", "Taiwan" = "Tayvan",
  "Tajikistan" = "Tacikistan", "Tanzania" = "Tanzanya",
  "United Republic of Tanzania" = "Tanzanya", "Thailand" = "Tayland",
  "Timor-Leste" = "Do\u011fu Timor", "Togo" = "Togo", "Tonga" = "Tonga",
  "Trinidad and Tobago" = "Trinidad ve Tobago", "Tunisia" = "Tunus",
  "Turkey" = "T\u00fcrkiye", "T\u00fcrkiye" = "T\u00fcrkiye", "Turkmenistan" = "T\u00fcrkmenistan",
  "Uganda" = "Uganda", "Ukraine" = "Ukrayna",
  "United Arab Emirates" = "Birle\u015fik Arap Emirlikleri",
  "United Kingdom" = "Birle\u015fik Krall\u0131k",
  "United States" = "Amerika Birle\u015fik Devletleri",
  "United States of America" = "Amerika Birle\u015fik Devletleri",
  "Uruguay" = "Uruguay", "Uzbekistan" = "\u00d6zbekistan", "Vanuatu" = "Vanuatu",
  "Vatican" = "Vatikan", "Venezuela" = "Venezuela", "Vietnam" = "Vietnam",
  "W. Sahara" = "Bat\u0131 Sahra", "Western Sahara" = "Bat\u0131 Sahra",
  "Yemen" = "Yemen", "Zambia" = "Zambiya", "Zimbabwe" = "Zimbabve",
  # Ek: eksik kalan topraklar ve k\u00fc\u00e7\u00fck \u00fclkeler
  "American Samoa" = "Amerikan Samoas\u0131", "Anguilla" = "Anguilla",
  "Antarctica" = "Antarktika", "Aruba" = "Aruba",
  "Ashmore and Cartier Is." = "Ashmore ve Cartier Adalar\u0131",
  "Bermuda" = "Bermuda",
  "Br. Indian Ocean Ter." = "\u0130ngiliz Hint Okyanusu B\u00f6lgesi",
  "British Virgin Is." = "\u0130ngiliz Virjin Adalar\u0131",
  "Cayman Is." = "Cayman Adalar\u0131", "Cook Is." = "Cook Adalar\u0131",
  "Cura\u00e7ao" = "Cura\u00e7ao",
  "Fr. S. Antarctic Lands" = "Frans\u0131z G\u00fcney ve Antarktika Topraklar\u0131",
  "Guernsey" = "Guernsey",
  "Heard I. and McDonald Is." = "Heard ve McDonald Adalar\u0131",
  "Indian Ocean Ter." = "Hint Okyanusu Topraklar\u0131",
  "Isle of Man" = "Man Adas\u0131", "Jersey" = "Jersey",
  "Montserrat" = "Montserrat",
  "N. Mariana Is." = "Kuzey Mariana Adalar\u0131",
  "Niue" = "Niue", "Norfolk Island" = "Norfolk Adas\u0131",
  "North Korea" = "Kuzey Kore",
  "Pitcairn Is." = "Pitcairn Adalar\u0131",
  "S. Geo. and the Is." = "G\u00fcney Georgia ve G\u00fcney Sandwich",
  "Saint Helena" = "Saint Helena", "Saint Lucia" = "Saint Lucia",
  "Siachen Glacier" = "Siachen Buzulu",
  "Sint Maarten" = "Sint Maarten", "South Korea" = "G\u00fcney Kore",
  "St-Barth\u00e9lemy" = "Saint Barth\u00e9lemy",
  "St-Martin" = "Saint Martin",
  "St. Kitts and Nevis" = "Saint Kitts ve Nevis",
  "St. Pierre and Miquelon" = "Saint Pierre ve Miquelon",
  "St. Vin. and Gren." = "Saint Vincent ve Grenadinler",
  "S\u00e3o Tom\u00e9 and Principe" = "S\u00e3o Tom\u00e9 ve Principe",
  "Turks and Caicos Is." = "Turks ve Caicos Adalar\u0131",
  "Tuvalu" = "Tuvalu",
  "U.S. Virgin Is." = "ABD Virjin Adalar\u0131",
  "Wallis and Futuna Is." = "Wallis ve Futuna",
  "\u00c5land" = "\u00c5land Adalar\u0131"
)

# Ulke isimlerini Turkceye cevir
to_tr <- function(x) {
  out <- character(length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      out[i] <- NA_character_
    } else if (x[i] %in% names(ulke_tr)) {
      out[i] <- unname(ulke_tr[x[i]])
    } else {
      out[i] <- x[i]
    }
  }
  out
}
ulke_ref[, name_tr := to_tr(name)]
ulke_sf$name_tr <- to_tr(ulke_sf$name)

# Ulke secenekleri (Turkce isim -> country_code as character)
.cc <- as.character(ulke_ref$country_code)
.tr <- as.character(ulke_ref$name_tr)
.ord <- order(.tr)
ulke_secenekleri <- setNames(.cc[.ord], .tr[.ord])
# Bos veya NA isimleri filtrele
ulke_secenekleri <- ulke_secenekleri[!is.na(names(ulke_secenekleri)) &
                                      nzchar(names(ulke_secenekleri))]

# Ulke merkezleri (etiket konumlari icin)
local({
  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(TRUE), add = TRUE)
  ulke_centroids <<- sf::st_point_on_surface(sf::st_geometry(ulke_sf))
})
ulke_centroid_coords <- sf::st_coordinates(ulke_centroids)
ulke_etiket_df <- data.frame(
  name_tr = ulke_sf$name_tr,
  country_code = ulke_sf$country_code,
  lng = ulke_centroid_coords[, 1],
  lat = ulke_centroid_coords[, 2],
  alan = ulke_sf$Alan_km2,
  stringsAsFactors = FALSE
)
# Nufus esigi icin 2026 verisini kullan
.pop_2026 <- pop_dt[year == 2026, .(country_code, pop)]
ulke_etiket_df <- merge(ulke_etiket_df, as.data.frame(.pop_2026),
                         by = "country_code", all.x = TRUE)
# Alan > 5000 km2 VEYA nufus > 500k olan ulkeleri dahil et
# (Kuveyt, Katar, Singapur, Luksemburg, Malta gibi kucuk ama onemli ulkeler)
ulke_etiket_df <- ulke_etiket_df[
  (!is.na(ulke_etiket_df$alan) & ulke_etiket_df$alan > 5000) |
  (!is.na(ulke_etiket_df$pop) & ulke_etiket_df$pop > 500000),
]
# Nufus eksikse alana gore fallback (etiket kategorisi icin)
ulke_etiket_df$alan[is.na(ulke_etiket_df$alan)] <- 0

# 3D kure icin: country_code -> lng/lat eslemesi
ulke_coords_dt <- data.table(
  country_code = ulke_sf$country_code,
  name_tr = ulke_sf$name_tr,
  lng = ulke_centroid_coords[, 1],
  lat = ulke_centroid_coords[, 2]
)[!is.na(country_code) & !is.na(lng) & !is.na(lat)]

# 3D extruded harita icin: ulke sinirlari GeoJSON olarak (bir kez hazirla)
.dunya_geojson <- tryCatch({
  if (requireNamespace("geojsonsf", quietly = TRUE)) {
    sf_geom <- ulke_sf[, "name_tr"]
    names(sf_geom)[1] <- "name"  # echarts 'name' property kullanir
    # Polygon hassasiyetini dusurerek hafiflet (performans)
    sf_simpl <- tryCatch(sf::st_simplify(sf_geom, dTolerance = 0.05),
                          error = function(e) sf_geom)
    geojsonsf::sf_geojson(sf_simpl, atomise = FALSE)
  } else NULL
}, error = function(e) { message("GeoJSON hazirlanamadi: ", conditionMessage(e)); NULL })
cat("GeoJSON haz\u0131r:", !is.null(.dunya_geojson), "\n")

# MapLibre basemap stillerini indir, metin katmanlarini cikar
.basemap_urls <- list(
  "dark"    = "https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json",
  "light"   = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
  "voyager" = "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json",
  "liberty" = "https://tiles.openfreemap.org/styles/liberty"
)
.basemap_cache <- new.env()
get_basemap_no_labels <- function(tema) {
  if (!is.null(.basemap_cache[[tema]])) return(.basemap_cache[[tema]])
  url <- .basemap_urls[[tema]]
  if (is.null(url)) return(NULL)
  s <- tryCatch(jsonlite::fromJSON(url, simplifyVector = FALSE),
                error = function(e) { message("Style indirilmedi: ", conditionMessage(e)); NULL })
  if (is.null(s)) return(url)  # fallback: orijinal URL
  # Tum symbol (metin) katmanlarini GIZLE (silme — font baglantilari korunsun)
  s$layers <- lapply(s$layers, function(l) {
    if ((l$type %||% "") == "symbol") {
      if (is.null(l$layout)) l$layout <- list()
      l$layout$visibility <- "none"
    }
    l
  })
  .basemap_cache[[tema]] <- s
  s
}
# 3 kategori: cok buyuk (her zoom'da goster) vs orta (zoom >= 3) vs kucuk (zoom >= 5)
ulke_etiket_buyuk <- ulke_etiket_df[ulke_etiket_df$alan > 1500000, ]
ulke_etiket_orta  <- ulke_etiket_df[ulke_etiket_df$alan <= 1500000 &
                                     ulke_etiket_df$alan > 100000, ]
ulke_etiket_kucuk <- ulke_etiket_df[ulke_etiket_df$alan <= 100000, ]

# Yardimci: sayi formatlama
fmt_sayi <- function(x, big.mark = ",") {
  ifelse(is.na(x), "-", format(round(x), big.mark = big.mark, scientific = FALSE))
}

# Yardimci: hover tooltip HTML'ini modern kart olarak uret
# html_safe server scope'unda oldugu icin burada dogrudan htmltools::htmlEscape kullan
ulke_tooltip_html <- function(isim, kita_tr_adi, deger_metni, deger_etiketi) {
  esc <- htmltools::htmlEscape
  kita_gecerli <- length(kita_tr_adi) > 0 && !is.na(kita_tr_adi) && nzchar(kita_tr_adi)
  kita_str <- if (!kita_gecerli) "" else
    sprintf("<div class='ulke-tooltip-kita'>&#127757; %s</div>", esc(kita_tr_adi))
  deger_gecerli <- length(deger_metni) > 0 && !is.na(deger_metni) && nzchar(deger_metni)
  deger_str <- if (!deger_gecerli) "-" else deger_metni
  htmltools::HTML(sprintf(
    "<div class='ulke-tooltip-baslik'>%s</div>%s<div class='ulke-tooltip-deger'><div class='deger'>%s</div><div class='deger-label'>%s</div></div>",
    esc(if (length(isim) == 0 || is.na(isim)) "-" else isim),
    kita_str, deger_str,
    esc(if (length(deger_etiketi) == 0) "" else deger_etiketi)
  ))
}

# Yardimci: ulke etiketleri (sik tipografi + zoom-bagli filtreleme)
ekle_ulke_etiketleri <- function(map) {
  ortak_stil <- list(
    "color" = "#1e293b",
    "font-family" = "'DM Sans', -apple-system, 'Segoe UI', sans-serif",
    "font-weight" = "500",
    "font-size" = "11px",
    "letter-spacing" = "0.1px",
    "text-shadow" = "0 0 3px #fff, 0 0 6px rgba(255,255,255,0.95), 0 0 10px rgba(255,255,255,0.7)"
  )
  ortak_stil_kucuk <- ortak_stil
  ortak_stil_kucuk[["font-size"]] <- "10px"
  ortak_stil_kucuk[["font-weight"]] <- "400"
  map %>%
    addLabelOnlyMarkers(
      data = ulke_etiket_buyuk, lng = ~lng, lat = ~lat,
      label = ~name_tr, group = "tr_isimler_buyuk",
      labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                  style = ortak_stil, className = "tr-label-buyuk"),
      options = markerOptions(pane = "tr_label_pane", interactive = FALSE)
    ) %>%
    addLabelOnlyMarkers(
      data = ulke_etiket_orta, lng = ~lng, lat = ~lat,
      label = ~name_tr, group = "tr_isimler_orta",
      labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                  style = ortak_stil, className = "tr-label-orta"),
      options = markerOptions(pane = "tr_label_pane", interactive = FALSE)
    ) %>%
    addLabelOnlyMarkers(
      data = ulke_etiket_kucuk, lng = ~lng, lat = ~lat,
      label = ~name_tr, group = "tr_isimler_kucuk",
      labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                  style = ortak_stil_kucuk, className = "tr-label-kucuk"),
      options = markerOptions(pane = "tr_label_pane", interactive = FALSE)
    ) %>%
    htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        function setZoomClass() {
          var z = map.getZoom();
          var c = el.querySelector('.leaflet-container') || el;
          c.classList.remove('zoom-1','zoom-2','zoom-3','zoom-4plus','zoom-5plus');
          if (z <= 1) c.classList.add('zoom-1');
          else if (z === 2) c.classList.add('zoom-2');
          else if (z === 3) c.classList.add('zoom-3');
          else if (z === 4) c.classList.add('zoom-4plus');
          else c.classList.add('zoom-5plus');
        }
        map.on('zoomend', setZoomClass);
        setZoomClass();
      }
    ")
}

# ============================================================
# UI yardimcilari
# ============================================================
info_baslik <- function(baslik, aciklama) {
  tags$div(class = "tab-intro",
    tags$div(class = "tab-intro-title",
      tags$span(style = "margin-right:6px;", HTML("&#9432;")), baslik),
    tags$div(class = "tab-intro-body", aciklama)
  )
}

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  id = "ana_nav",
  title = tags$span(
    tags$span(style = "font-size:22px;margin-right:8px;", HTML("&#127758;")),
    tags$span(style = "font-weight:800;letter-spacing:-0.3px;", "D\u00fcnya N\u00fcfus Analizi")
  ),
  window_title = "D\u00fcnya N\u00fcfus Analizi Paneli",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#6366f1",
    secondary = "#8b5cf6",
    success = "#10b981",
    info = "#06b6d4",
    warning = "#f59e0b",
    danger = "#ef4444",
    base_font = font_google("Plus Jakarta Sans"),
    heading_font = font_google("Outfit"),
    code_font = font_google("JetBrains Mono"),
    "navbar-bg" = "#0f172a",
    "body-bg" = "#f8fafc",
    "card-border-radius" = "14px",
    "card-cap-bg" = "transparent",
    "card-box-shadow" = "0 1px 3px rgba(0,0,0,0.04), 0 4px 20px rgba(0,0,0,0.04)"
  ),
  fillable = TRUE,
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
  nav_item(tags$a(
    href = "https://population.un.org/wpp/",
    target = "_blank",
    rel = "noopener noreferrer",
    style = "color:rgba(255,255,255,0.7);font-size:12px;padding:0 8px;text-decoration:none;",
    title = "Veri Kayna\u011f\u0131: BM WPP 2024 \u2014 resmi sayfaya git",
    HTML("&#128196; WPP 2024")
  )),
  header = tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700;800&family=Outfit:wght@500;600;700;800;900&family=DM+Sans:wght@400;500;600;700&display=swap",
              rel = "stylesheet"),
    tags$style(HTML("
      /* === TEMEL TIPOGRAFI === */
      body { font-family: 'Plus Jakarta Sans', -apple-system, 'Segoe UI', sans-serif; -webkit-font-smoothing: antialiased; font-feature-settings: 'ss01','cv11'; }
      h1,h2,h3,h4,h5,h6,.h1,.h2,.h3,.h4,.h5,.h6 { font-family: 'Outfit', 'Plus Jakarta Sans', sans-serif; letter-spacing: -0.02em; font-weight: 700; }
      .navbar-brand, .card-header, .bslib-value-box .value-box-value { font-family: 'Outfit', 'Plus Jakarta Sans', sans-serif; }

      /* === NAVBAR MODERN GORUNUM === */
      .navbar {
        background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%) !important;
        box-shadow: 0 2px 12px rgba(15,23,42,0.15);
        padding: 10px 20px;
      }
      .navbar-brand { font-size: 17px !important; display: flex; align-items: center; }
      .navbar .nav-link { color: rgba(255,255,255,0.75) !important; font-weight: 500; font-size: 14px; transition: all 0.2s; border-radius: 8px; margin: 0 2px; }
      .navbar .nav-link:hover { color: #fff !important; background: rgba(255,255,255,0.08); }
      .navbar .nav-link.active { color: #fff !important; background: rgba(99,102,241,0.25); }

      /* === KARTLAR === */
      .card, .bslib-card {
        border: none !important;
        border-radius: 14px !important;
        background: #ffffff;
        transition: transform 0.15s ease, box-shadow 0.2s ease;
      }
      .card:hover { box-shadow: 0 4px 16px rgba(0,0,0,0.06), 0 12px 32px rgba(0,0,0,0.06) !important; }
      .card-header {
        background: transparent !important;
        border-bottom: 1px solid #f1f5f9 !important;
        font-weight: 700;
        font-size: 13px;
        letter-spacing: 0.02em;
        color: #334155;
        padding: 14px 18px;
        text-transform: uppercase;
      }
      .card-body { padding: 18px; }

      /* === VALUE BOX MODERNLESTIRME === */
      .bslib-value-box {
        border-radius: 16px !important;
        border: none !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.04), 0 8px 24px rgba(0,0,0,0.04);
        transition: transform 0.2s ease, box-shadow 0.2s ease;
        overflow: hidden;
        position: relative;
      }
      .bslib-value-box:hover { transform: translateY(-3px); box-shadow: 0 6px 20px rgba(0,0,0,0.08), 0 16px 40px rgba(0,0,0,0.08); }
      .bslib-value-box .value-box-title { font-weight: 600; font-size: 12px; letter-spacing: 0.05em; text-transform: uppercase; opacity: 0.95; color: #ffffff !important; }
      .bslib-value-box .value-box-value { font-size: 28px !important; font-weight: 800 !important; letter-spacing: -0.02em; color: #ffffff !important; }
      .bslib-value-box .value-box-value * { color: #ffffff !important; }
      .bslib-value-box p { color: rgba(255,255,255,0.9) !important; }
      .bslib-value-box .value-box-showcase { opacity: 0.85; color: #ffffff !important; }
      .bslib-value-box .value-box-showcase svg, .bslib-value-box .value-box-showcase i { color: #ffffff !important; fill: #ffffff !important; }

      /* Gradient ozel sinflar */
      .vb-grad { height: 100%; }
      .vb-grad > .bslib-value-box, .vb-grad .bslib-value-box { height: 100%; color: #ffffff !important; }
      .vb-grad-1 .bslib-value-box { background: linear-gradient(135deg,#6366f1 0%,#8b5cf6 100%) !important; }
      .vb-grad-2 .bslib-value-box { background: linear-gradient(135deg,#10b981 0%,#06b6d4 100%) !important; }
      .vb-grad-3 .bslib-value-box { background: linear-gradient(135deg,#f59e0b 0%,#ef4444 100%) !important; }
      .vb-grad-4 .bslib-value-box { background: linear-gradient(135deg,#ec4899 0%,#8b5cf6 100%) !important; }
      .vb-grad .bslib-value-box .value-box-area { background: transparent !important; }

      /* === SIDEBAR === */
      .bslib-sidebar-layout > .sidebar {
        background: linear-gradient(180deg, #ffffff 0%, #f8fafc 100%) !important;
        border-right: 1px solid #e2e8f0 !important;
      }

      /* === FORM ELEMANLARI === */
      .form-control, .selectize-input, .selectize-dropdown {
        border-radius: 10px !important;
        border: 1.5px solid #e2e8f0 !important;
        transition: all 0.15s;
      }
      .form-control:focus, .selectize-input.focus {
        border-color: #6366f1 !important;
        box-shadow: 0 0 0 3px rgba(99,102,241,0.15) !important;
      }
      .irs--shiny .irs-bar { background: linear-gradient(90deg,#6366f1,#8b5cf6) !important; border-color: transparent !important; }
      .irs--shiny .irs-handle { border-color: #6366f1 !important; background: #fff !important; }
      .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: #6366f1 !important; }
      .btn-primary, .btn-outline-primary { border-radius: 10px !important; font-weight: 600; letter-spacing: 0.01em; transition: all 0.15s; }
      .btn-outline-primary:hover { transform: translateY(-1px); }

      /* === NAV PILLS (tema haritalar) === */
      .nav-pills .nav-link {
        border-radius: 10px !important;
        font-weight: 600;
        font-size: 13px;
        color: #64748b;
        margin-right: 4px;
        transition: all 0.15s;
      }
      .nav-pills .nav-link.active {
        background: linear-gradient(135deg,#6366f1,#8b5cf6) !important;
        color: #fff !important;
        box-shadow: 0 4px 12px rgba(99,102,241,0.3);
      }
      .nav-pills .nav-link:hover:not(.active) { background: #f1f5f9; color: #6366f1; }

      /* === DT TABLO === */
      table.dataTable thead th { background: #f8fafc !important; color: #334155 !important; font-weight: 700 !important; font-size: 12px; text-transform: uppercase; letter-spacing: 0.04em; border-bottom: 2px solid #e2e8f0 !important; }
      table.dataTable tbody tr:hover { background: #f8fafc !important; }

      /* === PLOTLY & LEAFLET === */
      .leaflet-container { border-radius: 12px; box-shadow: 0 2px 10px rgba(0,0,0,0.06); }
      .leaflet-control-layers, .leaflet-bar { border-radius: 8px !important; border: none !important; box-shadow: 0 2px 8px rgba(0,0,0,0.12) !important; }

      /* === ICIKI SCROLLBAR === */
      ::-webkit-scrollbar { width: 10px; height: 10px; }
      ::-webkit-scrollbar-track { background: #f1f5f9; }
      ::-webkit-scrollbar-thumb { background: #cbd5e1; border-radius: 10px; border: 2px solid #f1f5f9; }
      ::-webkit-scrollbar-thumb:hover { background: #94a3b8; }

      /* === CSS-TABANLI YUKLEME GOSTERGESI (hersam ic shinycssloaders'sizde) === */
      .shiny-bound-output.recalculating,
      .html-widget-output.recalculating,
      .leaflet-container.recalculating {
        opacity: 0.45;
        transition: opacity 0.25s ease;
        pointer-events: none;
        position: relative;
      }
      .shiny-bound-output.recalculating::after,
      .html-widget-output.recalculating::after {
        content: '';
        position: absolute;
        top: 50%; left: 50%;
        width: 44px; height: 44px;
        margin: -22px 0 0 -22px;
        border: 4px solid #e2e8f0;
        border-top-color: #6366f1;
        border-radius: 50%;
        animation: spin-loader 0.8s linear infinite;
        z-index: 1000;
      }
      @keyframes spin-loader {
        to { transform: rotate(360deg); }
      }
      .shiny-spinner-output-container { min-height: 100px; }

      /* Detay panelinde yil animasyonunda spinner/opaklik bozucu — devre disi */
      #harita_detay.recalculating {
        opacity: 1 !important;
        pointer-events: auto !important;
      }
      #harita_detay.recalculating::after { display: none !important; }

      /* === KOYU MOD === */
      [data-bs-theme='dark'] body { background: #0b1120 !important; }
      [data-bs-theme='dark'] .card, [data-bs-theme='dark'] .bslib-card { background: #1e293b !important; color: #e2e8f0; }
      [data-bs-theme='dark'] .card-header { color: #cbd5e1; border-bottom-color: #334155 !important; }
      [data-bs-theme='dark'] .bslib-value-box { background: #1e293b !important; }
      [data-bs-theme='dark'] .bslib-sidebar-layout > .sidebar { background: linear-gradient(180deg,#1e293b 0%,#0f172a 100%) !important; border-right-color: #334155 !important; color: #e2e8f0; }
      [data-bs-theme='dark'] .form-control, [data-bs-theme='dark'] .selectize-input { background: #0f172a !important; color: #e2e8f0 !important; border-color: #334155 !important; }
      [data-bs-theme='dark'] table.dataTable { color: #e2e8f0; }
      [data-bs-theme='dark'] table.dataTable thead th { background: #0f172a !important; color: #cbd5e1 !important; border-bottom-color: #334155 !important; }
      [data-bs-theme='dark'] table.dataTable tbody tr:hover { background: #1e293b !important; }

      /* === GIRIS ANIMASYONU === */
      .card, .bslib-value-box { animation: fadeInUp 0.4s ease-out both; }
      @keyframes fadeInUp { from { opacity: 0; transform: translateY(8px); } to { opacity: 1; transform: translateY(0); } }

      /* Tab tanitim kutusu */
      .tab-intro {
        background: linear-gradient(135deg, rgba(99,102,241,0.08), rgba(139,92,246,0.06));
        border-left: 4px solid #6366f1;
        border-radius: 10px;
        padding: 12px 16px;
        margin-bottom: 12px;
        font-size: 13px;
      }
      .tab-intro-title { font-weight: 700; color: #4338ca; margin-bottom: 2px; font-size: 13px; }
      .tab-intro-body { color: #475569; line-height: 1.5; }
      [data-bs-theme='dark'] .tab-intro { background: linear-gradient(135deg, rgba(99,102,241,0.15), rgba(139,92,246,0.1)); }
      [data-bs-theme='dark'] .tab-intro-title { color: #a5b4fc; }
      [data-bs-theme='dark'] .tab-intro-body { color: #cbd5e1; }

      /* Card icindeki dropdownlarin kirpilmasini onle */
      .card, .card-body, .bslib-card, .html-fill-container { overflow: visible !important; }
      .selectize-dropdown { z-index: 99999 !important; max-height: 300px !important; overflow-y: auto !important; }

      /* Ulke etiketi — saydam kutu, tam ortalama */
      .leaflet-tooltip.tr-label-buyuk,
      .leaflet-tooltip.tr-label-orta,
      .leaflet-tooltip.tr-label-kucuk {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
        padding: 0 !important;
        margin: 0 !important;
        white-space: nowrap !important;
        pointer-events: none !important;
      }

      /* Zoom'a gore etiket gorunurlugu — cakismayi azalt */
      .zoom-1 .tr-label-orta, .zoom-2 .tr-label-orta { display: none !important; }
      .zoom-1 .tr-label-kucuk, .zoom-2 .tr-label-kucuk,
      .zoom-3 .tr-label-kucuk, .zoom-4plus .tr-label-kucuk { display: none !important; }

      /* === HARITA YIL ROZETI === */
      .leaflet-control.yil-kontrol-wrap {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
        padding: 0 !important;
        margin: 14px !important;
      }
      .yil-gosterge {
        display: inline-flex;
        flex-direction: column;
        align-items: center;
        background: linear-gradient(135deg, rgba(15, 23, 42, 0.92) 0%, rgba(30, 41, 59, 0.92) 100%);
        color: #ffffff;
        font-family: 'Outfit', 'Plus Jakarta Sans', sans-serif;
        padding: 6px 16px 8px;
        border-radius: 10px;
        border: 1px solid rgba(255, 255, 255, 0.08);
        box-shadow: 0 4px 14px rgba(0, 0, 0, 0.25), 0 1px 3px rgba(0, 0, 0, 0.15);
        backdrop-filter: blur(6px);
        -webkit-backdrop-filter: blur(6px);
        user-select: none;
        pointer-events: none;
        line-height: 1;
      }
      .yil-gosterge .yil-etiket {
        font-size: 9px;
        font-weight: 600;
        letter-spacing: 2px;
        text-transform: uppercase;
        color: rgba(255, 255, 255, 0.6);
        margin-bottom: 3px;
      }
      .yil-gosterge .yil-deger {
        font-size: 28px;
        font-weight: 800;
        letter-spacing: 1px;
        text-shadow: 0 1px 2px rgba(0, 0, 0, 0.35);
        font-variant-numeric: tabular-nums;
      }

      /* === ULKE HOVER TOOLTIP — MODERN KART === */
      .leaflet-tooltip.ulke-tooltip-wrap {
        background: linear-gradient(135deg, rgba(15, 23, 42, 0.96) 0%, rgba(30, 41, 59, 0.96) 100%) !important;
        color: #ffffff !important;
        border: 1px solid rgba(255, 255, 255, 0.1) !important;
        border-radius: 12px !important;
        padding: 12px 16px !important;
        box-shadow: 0 10px 28px rgba(0, 0, 0, 0.35), 0 2px 6px rgba(0, 0, 0, 0.15) !important;
        font-family: 'Plus Jakarta Sans', -apple-system, 'Segoe UI', sans-serif !important;
        min-width: 180px;
        white-space: normal;
        backdrop-filter: blur(6px);
        -webkit-backdrop-filter: blur(6px);
        pointer-events: none;
      }
      .leaflet-tooltip.ulke-tooltip-wrap:before { display: none !important; }
      .ulke-tooltip-baslik {
        font-size: 14px;
        font-weight: 800;
        letter-spacing: 0.3px;
        color: #ffffff;
        line-height: 1.15;
        text-shadow: 0 1px 2px rgba(0,0,0,0.3);
      }
      .ulke-tooltip-kita {
        display: inline-flex;
        align-items: center;
        gap: 4px;
        font-size: 11px;
        color: rgba(255, 255, 255, 0.65);
        margin-top: 3px;
        font-weight: 500;
      }
      .ulke-tooltip-deger {
        margin-top: 10px;
        padding-top: 8px;
        border-top: 1px solid rgba(255, 255, 255, 0.12);
        display: flex;
        align-items: baseline;
        justify-content: space-between;
        gap: 12px;
      }
      .ulke-tooltip-deger .deger {
        font-family: 'Outfit', 'Plus Jakarta Sans', sans-serif;
        font-size: 20px;
        font-weight: 800;
        color: #22d3ee;
        font-variant-numeric: tabular-nums;
        line-height: 1;
        letter-spacing: 0.2px;
      }
      .ulke-tooltip-deger .deger-label {
        font-size: 9px;
        color: rgba(255, 255, 255, 0.55);
        text-transform: uppercase;
        letter-spacing: 1.2px;
        font-weight: 600;
        white-space: nowrap;
      }
    ")),
    tags$script(HTML("
      // Harita polygonlarini silmeden fillColor + tooltip + yil rozeti guncelle (hizli animasyon)
      $(document).on('shiny:connected', function() {
        if (typeof Shiny === 'undefined') return;
        Shiny.addCustomMessageHandler('updateMapColors', function(msg) {
          var w = HTMLWidgets.find('#' + msg.map_id);
          if (!w || typeof w.getMap !== 'function') return;
          var map = w.getMap();
          if (!map) return;
          if (msg.ids && msg.colors) {
            var byId = {};
            for (var i = 0; i < msg.ids.length; i++) {
              byId[msg.ids[i]] = { color: msg.colors[i], tip: msg.tooltips ? msg.tooltips[i] : null };
            }
            map.eachLayer(function(layer) {
              var lid = layer.options && layer.options.layerId;
              if (lid == null) return;
              var u = byId[lid];
              if (!u) return;
              if (typeof layer.setStyle === 'function') {
                layer.setStyle({ fillColor: u.color });
              }
              if (u.tip != null && typeof layer.getTooltip === 'function' && layer.getTooltip()) {
                layer.setTooltipContent(u.tip);
              }
            });
          }
          if (msg.year != null) {
            var badge = map.getContainer().querySelector('.yil-gosterge .yil-deger');
            if (badge) badge.textContent = msg.year;
          }
        });
      });
    "))
  ),

  # ----------------------------------------------------------
  # SEKME 1: GENEL BAKIS
  # ----------------------------------------------------------
  nav_panel("Genel Bak\u0131\u015f",
    icon = icon("gauge-high"),
    info_baslik("D\u00fcnya N\u00fcfusuna Genel Bak\u0131\u015f",
      "2026 y\u0131l\u0131 i\u00e7in BM WPP 2024 projeksiyonlar\u0131. K\u0131talar\u0131n n\u00fcfus da\u011f\u0131l\u0131m\u0131, en kalabal\u0131k/yo\u011fun/seyrek \u00fclkeler ve toplam g\u00f6stergeler burada \u00f6zetlenir."),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      div(class = "vb-grad vb-grad-1",
        value_box(title = "D\u00fcnya N\u00fcfusu (2026)", value = textOutput("vb_nufus"),
                  showcase = icon("earth-americas"), theme = "primary",
                  p("Toplam insan say\u0131s\u0131", style = "opacity:0.85;font-size:12px;margin:0;"))),
      div(class = "vb-grad vb-grad-2",
        value_box(title = "\u00dclke Say\u0131s\u0131", value = textOutput("vb_ulke"),
                  showcase = icon("flag"), theme = "success",
                  p("Veri mevcut olan", style = "opacity:0.85;font-size:12px;margin:0;"))),
      div(class = "vb-grad vb-grad-3",
        value_box(title = "Ort. Yo\u011funluk", value = textOutput("vb_yogunluk"),
                  showcase = icon("users"), theme = "warning",
                  p("D\u00fcnya geneli", style = "opacity:0.85;font-size:12px;margin:0;"))),
      div(class = "vb-grad vb-grad-4",
        value_box(title = "Ort. TFR (2026)", value = textOutput("vb_tfr"),
                  showcase = icon("baby"), theme = "info",
                  p("Do\u011furganl\u0131k h\u0131z\u0131", style = "opacity:0.85;font-size:12px;margin:0;")))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header(tags$div(style = "display:flex;justify-content:space-between;align-items:center;width:100%;",
             tags$span(icon("ranking-star"), " En Kalabal\u0131k 15 \u00dclke (2026)"),
             actionButton("buyut_top15", "", icon = icon("expand"),
                          class = "btn btn-sm btn-outline-primary",
                          style = "padding:2px 8px;font-size:11px;border-radius:6px;",
                          title = "Tam ekran"))),
           withSpin(plotlyOutput("gb_top15", height = "350px"))),
      card(card_header(tags$div(style = "display:flex;justify-content:space-between;align-items:center;width:100%;",
             tags$span(icon("chart-pie"), " K\u0131talara G\u00f6re N\u00fcfus Da\u011f\u0131l\u0131m\u0131"),
             actionButton("buyut_pasta", "", icon = icon("expand"),
                          class = "btn btn-sm btn-outline-primary",
                          style = "padding:2px 8px;font-size:11px;border-radius:6px;",
                          title = "Tam ekran"))),
           withSpin(plotlyOutput("gb_pasta", height = "350px")))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header(tags$div(style = "display:flex;justify-content:space-between;align-items:center;width:100%;",
             tags$span(icon("layer-group"), " K\u0131talara G\u00f6re N\u00fcfus Yo\u011funlu\u011fu"),
             actionButton("buyut_kita_yogunluk", "", icon = icon("expand"),
                          class = "btn btn-sm btn-outline-primary",
                          style = "padding:2px 8px;font-size:11px;border-radius:6px;",
                          title = "Tam ekran"))),
           withSpin(plotlyOutput("gb_kita_yogunluk", height = "320px"))),
      card(
        card_header(tags$div(style = "display:flex;justify-content:space-between;align-items:center;width:100%;",
          tags$span(icon("scale-balanced"), " En Yo\u011fun ve En Seyrek 10 \u00dclke"),
          tags$div(
            actionButton("buyut_yogun", "Yo\u011fun", icon = icon("expand"),
                         class = "btn btn-sm btn-outline-primary",
                         style = "padding:2px 8px;font-size:11px;border-radius:6px;margin-right:4px;"),
            actionButton("buyut_seyrek", "Seyrek", icon = icon("expand"),
                         class = "btn btn-sm btn-outline-primary",
                         style = "padding:2px 8px;font-size:11px;border-radius:6px;")
          )
        )),
        layout_columns(
          col_widths = c(6, 6),
          withSpin(plotlyOutput("gb_en_yogun", height = "300px")),
          withSpin(plotlyOutput("gb_en_seyrek", height = "300px"))
        )
      )
    )
  ),

  # ----------------------------------------------------------
  # SEKME 2: INTERAKTIF HARITA
  # ----------------------------------------------------------
  nav_panel("\u0130nteraktif Harita",
    icon = icon("location-dot"),
    info_baslik("Etkile\u015fimli D\u00fcnya Haritas\u0131",
      "Y\u0131l kayd\u0131r\u0131c\u0131s\u0131yla 1950-2100 aras\u0131 d\u00f6nemi g\u00f6r\u00fcnt\u00fcle. Bir metrik se\u00e7 (n\u00fcfus, yo\u011funluk, TFR, ya\u015fam s\u00fcresi...). \u00dclkeye t\u0131klay\u0131nca sol tarafta detay kart\u0131 ve ya\u015f piramidi a\u00e7\u0131l\u0131r."),
    layout_sidebar(
      sidebar = sidebar(
        width = 340, padding = 12,
        sliderInput("harita_yil", "Y\u0131l:", min = 1950, max = 2100, value = 2026,
                    step = 1, sep = "", animate = animationOptions(interval = 800)),
        selectInput("harita_metrik", "G\u00f6sterilecek Metrik:",
                    choices = c("N\u00fcfus" = "pop",
                                "Yo\u011funluk (ki\u015fi/km\u00b2)" = "yogunluk",
                                "B\u00fcy\u00fcme Oran\u0131 (%)" = "buyume",
                                "Do\u011furganl\u0131k (TFR)" = "tfr",
                                "Net G\u00f6\u00e7" = "goc",
                                "Ya\u015fam S\u00fcresi" = "e0"),
                    selected = "yogunluk"),
        if (!is.null(eyalet_sf)) checkboxInput("eyalet_goster", "Eyalet/\u0130l Katman\u0131", value = FALSE),
        htmlOutput("harita_detay"),
        plotlyOutput("harita_piramit", height = "180px")
      ),
      leafletOutput("ana_harita", height = "85vh")
    )
  ),

  # ----------------------------------------------------------
  # SEKME 3: NUFUS PROJEKSIYONU
  # ----------------------------------------------------------
  nav_panel("N\u00fcfus Projeksiyonu",
    icon = icon("chart-line"),
    info_baslik("N\u00fcfus Projeksiyonu (1950-2100)",
      "Se\u00e7ti\u011fin \u00fclkelerin zaman i\u00e7indeki seyri. 2023 \u00f6ncesi ger\u00e7ekle\u015fen veri, sonras\u0131 BM projeksiyonudur (kesik \u00e7izgi). Birden fazla metri\u011fi ayn\u0131 anda kar\u015f\u0131la\u015ft\u0131rabilirsin."),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectizeInput("proj_ulkeler", "\u00dclke Se\u00e7 (birden fazla):",
                       choices = ulke_secenekleri, multiple = TRUE,
                       selected = unname(ulke_secenekleri[c("Hindistan", "\u00c7in",
                                          "Amerika Birle\u015fik Devletleri",
                                          "T\u00fcrkiye", "Nijerya")]),
                       options = list(maxItems = 10,
                                      placeholder = "\u00dclke ara...",
                                      plugins = list("remove_button"))),
        helpText("Eklemek i\u00e7in t\u0131klay\u0131p ara, \u00e7\u0131karmak i\u00e7in (\u00d7) butonuna bas."),
        checkboxGroupInput("proj_metrik", "G\u00f6sterilecek Metrikler:",
                           choices = c("N\u00fcfus" = "pop", "Do\u011furganl\u0131k (TFR)" = "tfr",
                                       "Ya\u015fam S\u00fcresi" = "e0", "Net G\u00f6\u00e7" = "goc"),
                           selected = "pop")
      ),
      card(card_header("N\u00fcfus Projeksiyonu (1950-2100)"), plotlyOutput("proj_grafik", height = "70vh"))
    )
  ),

  # ----------------------------------------------------------
  # SEKME 4: YAS PIRAMIDI KARSILASTIRMA
  # ----------------------------------------------------------
  nav_panel("Ya\u015f Piramidi",
    icon = icon("chart-column"),
    info_baslik("Ya\u015f Piramidi Kar\u015f\u0131la\u015ft\u0131rmas\u0131",
      "\u0130ki \u00fclkenin ayn\u0131 y\u0131ldaki ya\u015f-cinsiyet da\u011f\u0131l\u0131m\u0131n\u0131 yan yana g\u00f6r. Geni\u015f taban = gen\u00e7 n\u00fcfus, dar taban = ya\u015flanan toplum. Y\u0131l\u0131 animasyonla oynatabilirsin."),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        selectizeInput("pir_ulke1", "\u00dclke 1:", choices = ulke_secenekleri,
                       selected = unname(ulke_secenekleri["Hindistan"]),
                       options = list(placeholder = "\u00dclke ara...",
                                      searchField = c("text", "value"),
                                      dropdownParent = "body"))
      ),
      card(
        selectizeInput("pir_ulke2", "\u00dclke 2:", choices = ulke_secenekleri,
                       selected = unname(ulke_secenekleri["Japonya"]),
                       options = list(placeholder = "\u00dclke ara...",
                                      searchField = c("text", "value"),
                                      dropdownParent = "body"))
      ),
      card(
        sliderInput("pir_yil", "Y\u0131l:", min = 1950, max = 2100, value = 2026, step = 1, sep = "",
                    animate = animationOptions(interval = 400))
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header(textOutput("pir_baslik1")), plotlyOutput("piramit1", height = "55vh")),
      card(card_header(textOutput("pir_baslik2")), plotlyOutput("piramit2", height = "55vh"))
    )
  ),

  # ----------------------------------------------------------
  # SEKME 5: TEMATIK HARITALAR (birlesik slider)
  # ----------------------------------------------------------
  nav_panel("Tematik Haritalar",
    icon = icon("palette"),
    info_baslik("Tematik D\u00fcnya Haritalar\u0131",
      "G\u00f6\u00e7, do\u011furganl\u0131k, ya\u015fam s\u00fcresi, b\u00fcy\u00fcme ve demografik ge\u00e7i\u015f gibi farkl\u0131 g\u00f6stergelerin \u00fclke baz\u0131nda da\u011f\u0131l\u0131m\u0131. Sekmeler aras\u0131 ge\u00e7i\u015ste y\u0131l se\u00e7imi ortakt\u0131r."),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        sliderInput("tema_yil", "Y\u0131l (t\u00fcm haritalar):", min = 1950, max = 2100, value = 2026,
                    step = 1, sep = "", animate = animationOptions(interval = 500))
      ),
      navset_pill(
        id = "tema_sub_nav",
        nav_panel("Net G\u00f6\u00e7", leafletOutput("tema_goc_harita", height = "65vh")),
        nav_panel("Do\u011furganl\u0131k (TFR)", leafletOutput("tema_tfr_harita", height = "65vh")),
        nav_panel("Ya\u015fam S\u00fcresi", leafletOutput("tema_e0_harita", height = "65vh")),
        nav_panel("B\u00fcy\u00fcme Oran\u0131", leafletOutput("tema_buy_harita", height = "65vh")),
        nav_panel("Demografik Ge\u00e7i\u015f", leafletOutput("tema_gecis_harita", height = "65vh")),
        nav_panel("Ba\u011f\u0131ms\u0131zl\u0131k Oran\u0131", leafletOutput("tema_bag_harita", height = "65vh")),
        nav_panel("Medyan Ya\u015f (2025)",
          helpText("Worldometer 2025 verisi - tek y\u0131l snapshot"),
          leafletOutput("tema_yas_harita", height = "65vh")
        ),
        nav_panel("\u015eehir N\u00fcfus % (2025)",
          helpText("Worldometer 2025 verisi - kentle\u015fme oran\u0131"),
          leafletOutput("tema_kent_harita", height = "65vh")
        )
      )
    )
  ),

  # ----------------------------------------------------------
  # SEKME: 3D GORUNUM (echarts4r)
  # ----------------------------------------------------------
  # ----------------------------------------------------------
  # SEKME 5b: 3D INTERAKTIF KURE (WebGL)
  # ----------------------------------------------------------
  nav_panel("3D Harita",
    icon = icon("cubes"),
    info_baslik("3D Extruded D\u00fcnya Haritas\u0131 (MapLibre GL)",
      "Vector tile tabanl\u0131 modern 3D harita. \u00dclkeler n\u00fcfusa g\u00f6re yukar\u0131 \u00e7\u0131k\u0131yor. \U0001F446 Fare ile \u00fczerine gel = \u00fclke ad\u0131, t\u0131kla = detayl\u0131 n\u00fcfus/yo\u011funluk/TFR bilgileri."),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        sliderInput("globe_yil", "Y\u0131l:", min = 1950, max = 2100,
                    value = 2026, step = 1, sep = ""),
        selectInput("globe_yukseklik", "Y\u00fckseklik (kule boyu):",
                    choices = c("N\u00fcfus" = "pop",
                                "Yo\u011funluk (ki\u015fi/km\u00b2)" = "yogunluk",
                                "B\u00fcy\u00fcme oran\u0131 (%)" = "growth"),
                    selected = "pop"),
        selectInput("globe_renk", "Renklendirme:",
                    choices = c("N\u00fcfus Yo\u011funlu\u011fu" = "yogunluk",
                                "N\u00fcfus" = "pop",
                                "B\u00fcy\u00fcme oran\u0131 (%)" = "growth"),
                    selected = "yogunluk"),
        selectInput("globe_tema", "Harita Temas\u0131:",
                    choices = c("Koyu (Dark Matter)" = "dark",
                                "A\u00e7\u0131k (Positron)" = "light",
                                "Voyager" = "voyager",
                                "Liberty" = "liberty"),
                    selected = "dark"),
        sliderInput("globe_pitch", "Kamera e\u011fimi:",
                    min = 0, max = 75, value = 55, step = 5),
        tags$hr(),
        tags$div(style = "font-size:11px;color:#64748b;background:#f8fafc;padding:8px 10px;border-radius:8px;",
          tags$b("\U0001F5B1 Kontroller"), tags$br(),
          "\u2022 Sol t\u0131k + s\u00fcr\u00fckle: d\u00f6nd\u00fcr/kayd\u0131r", tags$br(),
          "\u2022 Sa\u011f t\u0131k (veya Ctrl+s\u00fcr\u00fckle): e\u011f", tags$br(),
          "\u2022 Tekerlek: yak\u0131nla\u015ft\u0131r/uzakla\u015ft\u0131r"
        )
      ),
      card(full_screen = TRUE,
           card_header(tags$span(icon("mountain"), " 3D D\u00fcnya Haritas\u0131 (MapLibre)")),
           mapgl::maplibreOutput("map3d", height = "720px"))
    )
  ),

  # ----------------------------------------------------------
  # SEKME 6: YOGUNLUK-BUYUME ILISKISI
  # ----------------------------------------------------------
  nav_panel("Yo\u011funluk-B\u00fcy\u00fcme",
    icon = icon("braille"),
    info_baslik("Yo\u011funluk vs. B\u00fcy\u00fcme \u0130li\u015fkisi",
      "X ekseni log \u00f6l\u00e7ekte yo\u011funluk, Y ekseni y\u0131ll\u0131k de\u011fi\u015fim. Balon boyutu n\u00fcfusu, renk k\u0131tay\u0131 g\u00f6sterir. S\u0131f\u0131r \u00e7izgisinin \u00fcst\u00fc b\u00fcy\u00fcyen, alt\u0131 k\u00fc\u00e7\u00fclen \u00fclkelerdir."),
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        sliderInput("scatter_yil", "Y\u0131l:", min = 1950, max = 2100, value = 2026, step = 1, sep = "")
      ),
      card(
        card_header("N\u00fcfus Yo\u011funlu\u011fu ve Y\u0131ll\u0131k De\u011fi\u015fim Oran\u0131 \u0130li\u015fkisi"),
        plotlyOutput("scatter_grafik", height = "75vh")
      )
    )
  ),

  # ----------------------------------------------------------
  # SEKME 7: VERI TABLOSU
  # ----------------------------------------------------------
  # ----------------------------------------------------------
  # SEKME 7: ULKE KARSILASTIRMA
  # ----------------------------------------------------------
  nav_panel("Kar\u015f\u0131la\u015ft\u0131rma",
    icon = icon("scale-balanced"),
    info_baslik("\u00dclke Kar\u015f\u0131la\u015ft\u0131rmas\u0131",
      "2-4 \u00fclke se\u00e7. T\u00fcm temel g\u00f6stergeler yan yana kart olarak \u00f6zetlenir; alt\u0131nda zaman serileri ve mini ya\u015f piramitleri ile kar\u015f\u0131la\u015ft\u0131rabilirsin."),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectizeInput("kars_ulkeler", "\u00dclkeler (2-4):",
                       choices = ulke_secenekleri, multiple = TRUE,
                       selected = unname(ulke_secenekleri[c("T\u00fcrkiye", "Almanya", "Japonya", "Brezilya")]),
                       options = list(maxItems = 4,
                                      placeholder = "\u00dclke ara...",
                                      plugins = list("remove_button"))),
        sliderInput("kars_yil", "Y\u0131l:", min = 1950, max = 2100, value = 2026,
                    step = 1, sep = "", animate = animationOptions(interval = 800)),
        helpText("En fazla 4 \u00fclke kar\u015f\u0131la\u015ft\u0131rabilirsin.")
      ),
      uiOutput("kars_ozet_kartlar"),
      tags$div(style = "margin:10px 0 6px;font-size:13px;color:#64748b;font-weight:600;",
               "\U0001F4C8 Zaman Serisi (t\u00fcm y\u0131llar) \u2014 her kart\u0131n sa\u011f \u00fcst\u00fcndeki butonla tam ekran a\u00e7abilirsin"),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             card_header(tags$span(icon("users"), " N\u00fcfus (Milyon)")),
             plotlyOutput("kars_zaman_pop", height = "460px")),
        card(full_screen = TRUE,
             card_header(tags$span(icon("baby"), " Do\u011furganl\u0131k (TFR)")),
             plotlyOutput("kars_zaman_tfr", height = "460px")),
        card(full_screen = TRUE,
             card_header(tags$span(icon("heart-pulse"), " Ya\u015fam S\u00fcresi (y\u0131l)")),
             plotlyOutput("kars_zaman_e0", height = "460px")),
        card(full_screen = TRUE,
             card_header(tags$span(icon("chart-line"), " B\u00fcy\u00fcme Oran\u0131 (%)")),
             plotlyOutput("kars_zaman_gr", height = "460px"))
      ),
      card(card_header(tags$span(icon("people-group"), " Ya\u015f Piramitleri")),
           uiOutput("kars_piramitler"))
    )
  ),

  nav_panel("Veri Tablosu",
    icon = icon("table-cells"),
    info_baslik("Ham Veri Tablosu",
      "T\u00fcm metrikleri i\u00e7eren aranabilir ve s\u0131ralanabilir tablo. K\u0131ta filtresiyle daralt, sa\u011f-\u00fcstten arat, CSV olarak indir."),
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        sliderInput("tablo_yil", "Y\u0131l:", min = 1950, max = 2100, value = 2026, step = 1, sep = ""),
        selectInput("tablo_kita", "K\u0131ta Filtresi:",
                    choices = c("T\u00fcm\u00fc" = "hepsi", "Asya" = "Asia", "Afrika" = "Africa",
                                "Avrupa" = "Europe", "Kuzey Amerika" = "North America",
                                "G\u00fcney Amerika" = "South America", "Okyanusya" = "Oceania"),
                    selected = "hepsi"),
        downloadButton("tablo_indir_csv", "CSV \u0130ndir", class = "btn-sm btn-outline-primary w-100")
      ),
      card(card_header(textOutput("tablo_baslik")), DTOutput("veri_tablosu"))
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ---- YARDIMCI FONKSIYONLAR ----

  # Bos durum grafigi (plotly_empty yerine bilgi mesajli)
  bos_grafik <- function(mesaj = "Veri bulunamad\u0131", ikon = "\U0001F4CA") {
    plotly_empty() %>%
      layout(
        annotations = list(list(
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          text = sprintf("<span style='font-size:36px;'>%s</span><br><span style='font-size:14px;color:#64748b;'>%s</span>",
                         ikon, mesaj),
          showarrow = FALSE, font = list(family = "Plus Jakarta Sans")
        )),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        plot_bgcolor = "transparent", paper_bgcolor = "transparent"
      ) %>% config(displayModeBar = FALSE)
  }

  # Bir yil icin ulke verilerini birlestir (environment cache ile)
  .veri_cache_env <- new.env(parent = emptyenv())
  ulke_veri_yil <- function(yil) {
    cache_key <- as.character(yil)
    if (exists(cache_key, envir = .veri_cache_env)) return(get(cache_key, envir = .veri_cache_env))
    p <- pop_dt[year == yil, .(country_code, pop)]
    t <- tfr_dt[year == yil, .(country_code, tfr)]
    e <- e0_dt[year == yil, .(country_code, e0B, e0M, e0F)]
    m <- mig_dt[year == yil, .(country_code, mig)]
    ms <- misc_dt[year == yil, .(country_code, growthrate, births, deaths)]

    dt <- p
    dt <- merge(dt, t, by = "country_code", all.x = TRUE)
    dt <- merge(dt, e, by = "country_code", all.x = TRUE)
    dt <- merge(dt, m, by = "country_code", all.x = TRUE)
    dt <- merge(dt, ms, by = "country_code", all.x = TRUE)
    assign(cache_key, dt, envir = .veri_cache_env)
    dt
  }

  # Yas piramidi olustur (plotly) - kompakt mod (sidebar icin)
  piramit_ciz <- function(cc, yil, kompakt = FALSE) {
    if (is.null(cc) || is.na(cc) || is.null(yil))
      return(bos_grafik("Bir \u00fclke se\u00e7in", "\U0001F464"))
    ad <- age_dt[country_code == cc & year == yil]
    if (nrow(ad) == 0) return(bos_grafik("Bu y\u0131l i\u00e7in veri yok", "\U0001F4C5"))
    ad <- copy(ad)  # data.table reference sorununu onle
    ad[, yas_grubu := cut(age, breaks = c(seq(0, 100, 5), Inf),
                          right = FALSE, include.lowest = TRUE,
                          labels = c(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), "100+"))]
    ozet <- ad[, .(Erkek = sum(popM, na.rm = TRUE), Kadin = sum(popF, na.rm = TRUE)),
               by = yas_grubu]
    ozet[, Erkek := -Erkek]
    # Plotly horizontal bar'da first level bottom'a gider — yas gruplarini dogal sirayla birak
    ozet$yas_grubu <- factor(as.character(ozet$yas_grubu),
                              levels = levels(ad$yas_grubu))
    # data.frame'e cevir
    ozet_df <- as.data.frame(ozet)

    max_val <- max(abs(c(ozet_df$Erkek, ozet_df$Kadin)), na.rm = TRUE)
    if (!is.finite(max_val) || max_val <= 0) return(bos_grafik("Veri yok", "\U0001F4CA"))
    tick_vals <- pretty(c(0, max_val), n = if (kompakt) 3 else 4)
    tick_vals <- sort(unique(c(-tick_vals, tick_vals)))

    margin_l <- if (kompakt) 38 else 60
    font_size <- if (kompakt) 8 else 12
    margin_b <- if (kompakt) 22 else 40

    suppressWarnings(plot_ly()) %>%
      add_bars(data = ozet_df, y = ~yas_grubu, x = ~Erkek, name = "Erkek",
               marker = list(color = "#3b82f6"), orientation = "h",
               hovertemplate = paste0("Erkek: %{x:,.0f}<extra></extra>")) %>%
      add_bars(data = ozet_df, y = ~yas_grubu, x = ~Kadin, name = "Kad\u0131n",
               marker = list(color = "#ec4899"), orientation = "h",
               hovertemplate = paste0("Kad\u0131n: %{x:,.0f}<extra></extra>")) %>%
      layout(
        barmode = "overlay",
        bargap = 0.05,
        dragmode = FALSE,
        xaxis = list(title = "", tickvals = tick_vals,
                     ticktext = fmt_sayi(abs(tick_vals)),
                     tickfont = list(size = font_size),
                     showgrid = TRUE, gridcolor = "#f1f5f9",
                     fixedrange = TRUE),
        yaxis = list(title = "", tickfont = list(size = font_size),
                     showgrid = FALSE, fixedrange = TRUE),
        legend = if (kompakt) list(visible = FALSE)
                 else list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center"),
        showlegend = !kompakt,
        margin = list(l = margin_l, r = 5, t = 5, b = margin_b),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff"
      ) %>% config(displayModeBar = FALSE, scrollZoom = FALSE)
  }

  # ============================================================
  # SEKME 1: GENEL BAKIS
  # ============================================================
  gb_veri <- ulke_veri_yil(2026)
  gb_veri <- merge(gb_veri, ulke_ref, by = "country_code", all.x = TRUE)
  gb_veri[, yogunluk := ifelse(!is.na(Alan_km2) & Alan_km2 > 0, pop / Alan_km2, NA_real_)]

  output$vb_nufus <- renderText(fmt_sayi(sum(gb_veri$pop, na.rm = TRUE)))
  output$vb_ulke <- renderText(as.character(sum(!is.na(gb_veri$pop))))
  output$vb_yogunluk <- renderText({
    toplam_alan <- sum(gb_veri$Alan_km2, na.rm = TRUE)
    if (toplam_alan > 0) {
      paste0(round(sum(gb_veri$pop, na.rm = TRUE) / toplam_alan, 1), " ki\u015fi/km\u00b2")
    } else "-"
  })
  output$vb_tfr <- renderText(as.character(round(mean(gb_veri$tfr, na.rm = TRUE), 2)))

  # Grafik \u00fcretici fonksiyonlar (hem normal hem modal b\u00fcy\u00fck versiyonda kullan\u0131l\u0131r)
  ciz_top15 <- function(font_size = 13) {
    top15 <- gb_veri[!is.na(pop)][order(-pop)][1:15]
    top15$milyon <- top15$pop / 1e6
    plot_ly(top15, y = ~reorder(name_tr, pop), x = ~milyon, type = "bar", orientation = "h",
            marker = list(color = viridis(15, option = "plasma")),
            text = ~paste0(round(milyon, 1), " M"), textposition = "outside",
            textfont = list(size = font_size, family = "Plus Jakarta Sans", color = "#1a1a2e"),
            cliponaxis = FALSE) %>%
      layout(xaxis = list(title = list(text = "N\u00fcfus (Milyon)", font = list(size = font_size)),
                          fixedrange = TRUE, tickfont = list(size = font_size - 1),
                          range = c(0, max(top15$milyon) * 1.18)),
             yaxis = list(title = "", fixedrange = TRUE, automargin = TRUE,
                          tickfont = list(size = font_size, family = "Plus Jakarta Sans")),
             dragmode = FALSE, uniformtext = list(minsize = font_size, mode = "show"),
             margin = list(l = 10, r = 70, t = 10, b = 40)) %>%
      config(displayModeBar = FALSE, scrollZoom = FALSE)
  }

  ciz_pasta <- function(font_size = 14) {
    kd <- gb_veri[!is.na(continent) & continent != "Antarctica" & continent != "Seven seas (open ocean)",
                  .(toplam = sum(pop, na.rm = TRUE)), by = continent]
    kd$kita_tr <- kita_tr[kd$continent]
    kd$yuzde <- round(kd$toplam / sum(kd$toplam) * 100, 1)
    plot_ly(kd, labels = ~kita_tr, values = ~toplam, type = "pie",
            marker = list(colors = viridis(nrow(kd), option = "turbo"), line = list(color = "#fff", width = 2)),
            textinfo = "label+percent",
            textfont = list(size = font_size, family = "Plus Jakarta Sans")) %>%
      layout(showlegend = FALSE, margin = list(l = 20, r = 20, t = 20, b = 20)) %>%
      config(displayModeBar = FALSE)
  }

  ciz_kita_yogunluk <- function(font_size = 14) {
    kd <- gb_veri[!is.na(continent) & continent != "Antarctica" & continent != "Seven seas (open ocean)",
                  .(toplam_nufus = sum(pop, na.rm = TRUE), toplam_alan = sum(Alan_km2, na.rm = TRUE)),
                  by = continent]
    kd$yogunluk <- kd$toplam_nufus / kd$toplam_alan
    kd$kita_tr <- kita_tr[kd$continent]
    kd <- kd[order(yogunluk)]
    plot_ly(kd, y = ~reorder(kita_tr, yogunluk), x = ~yogunluk, type = "bar", orientation = "h",
            marker = list(color = viridis(nrow(kd), option = "turbo")),
            text = ~paste0(round(yogunluk, 1), " ki\u015fi/km\u00b2"), textposition = "outside",
            textfont = list(size = font_size, family = "Plus Jakarta Sans", color = "#1a1a2e"),
            cliponaxis = FALSE) %>%
      layout(xaxis = list(title = list(text = "Yo\u011funluk (ki\u015fi/km\u00b2)", font = list(size = font_size)),
                          fixedrange = TRUE, tickfont = list(size = font_size - 1),
                          range = c(0, max(kd$yogunluk) * 1.25)),
             yaxis = list(title = "", fixedrange = TRUE, automargin = TRUE,
                          tickfont = list(size = font_size + 1, family = "Plus Jakarta Sans")),
             dragmode = FALSE, uniformtext = list(minsize = font_size, mode = "show"),
             margin = list(l = 10, r = 90, t = 10, b = 40)) %>%
      config(displayModeBar = FALSE, scrollZoom = FALSE)
  }

  ciz_en_yogun <- function(font_size = 12) {
    yogun <- gb_veri[!is.na(yogunluk) & yogunluk > 0][order(-yogunluk)][1:10]
    plot_ly(yogun, y = ~reorder(name_tr, yogunluk), x = ~yogunluk,
            type = "bar", orientation = "h",
            marker = list(color = ~yogunluk,
                          colorscale = "Viridis", reversescale = TRUE,
                          showscale = FALSE),
            text = ~format(round(yogunluk), big.mark = ","),
            textposition = "outside", textfont = list(size = font_size, family = "Plus Jakarta Sans", color = "#1a1a2e"),
            cliponaxis = FALSE,
            hovertemplate = "%{y}<br>%{x:,.0f} ki\u015fi/km\u00b2<extra></extra>") %>%
      layout(
        title = list(text = "<b>En Yo\u011fun</b>", font = list(size = font_size + 3, family = "Plus Jakarta Sans"), x = 0.05),
        yaxis = list(title = "", tickfont = list(size = font_size, family = "Plus Jakarta Sans"), fixedrange = TRUE, automargin = TRUE),
        xaxis = list(title = "", tickfont = list(size = font_size - 1), fixedrange = TRUE,
                     range = c(0, max(yogun$yogunluk) * 1.2)),
        dragmode = FALSE, uniformtext = list(minsize = font_size, mode = "show"),
        margin = list(l = 10, r = 70, t = 35, b = 25),
        showlegend = FALSE
      ) %>% config(displayModeBar = FALSE, scrollZoom = FALSE)
  }

  ciz_en_seyrek <- function(font_size = 12) {
    seyrek <- gb_veri[!is.na(yogunluk) & yogunluk > 0][order(yogunluk)][1:10]
    plot_ly(seyrek, y = ~reorder(name_tr, -yogunluk), x = ~yogunluk,
            type = "bar", orientation = "h",
            marker = list(color = ~yogunluk,
                          colorscale = "Viridis",
                          showscale = FALSE),
            text = ~round(yogunluk, 1),
            textposition = "outside", textfont = list(size = font_size, family = "Plus Jakarta Sans", color = "#1a1a2e"),
            cliponaxis = FALSE,
            hovertemplate = "%{y}<br>%{x:.1f} ki\u015fi/km\u00b2<extra></extra>") %>%
      layout(
        title = list(text = "<b>En Seyrek</b>", font = list(size = font_size + 3, family = "Plus Jakarta Sans"), x = 0.05),
        yaxis = list(title = "", tickfont = list(size = font_size, family = "Plus Jakarta Sans"), fixedrange = TRUE, automargin = TRUE),
        xaxis = list(title = "", tickfont = list(size = font_size - 1), fixedrange = TRUE,
                     range = c(0, max(seyrek$yogunluk) * 1.25)),
        dragmode = FALSE, uniformtext = list(minsize = font_size, mode = "show"),
        margin = list(l = 10, r = 70, t = 35, b = 25),
        showlegend = FALSE
      ) %>% config(displayModeBar = FALSE, scrollZoom = FALSE)
  }

  # Genel Bakis grafiklerini server scope'ta tek sefer hesapla (2026 sabit)
  .gb_cache <- new.env(parent = emptyenv())
  gb_p <- function(key, fn) {
    if (is.null(.gb_cache[[key]])) .gb_cache[[key]] <- fn
    .gb_cache[[key]]
  }
  output$gb_top15         <- renderPlotly(gb_p("top15",         ciz_top15(13)))
  output$gb_pasta         <- renderPlotly(gb_p("pasta",         ciz_pasta(14)))
  output$gb_kita_yogunluk <- renderPlotly(gb_p("kita_yogunluk", ciz_kita_yogunluk(14)))
  output$gb_en_yogun      <- renderPlotly(gb_p("en_yogun",      ciz_en_yogun(12)))
  output$gb_en_seyrek     <- renderPlotly(gb_p("en_seyrek",     ciz_en_seyrek(12)))

  # ---- Modal b\u00fcy\u00fcltme ----
  ac_modal <- function(baslik, plot_fn) {
    output$buyut_modal_plot <- renderPlotly(plot_fn(16))
    showModal(modalDialog(
      title = baslik, size = "xl", easyClose = TRUE, fade = TRUE,
      footer = modalButton("Kapat"),
      tags$div(style = "height:72vh;",
               plotlyOutput("buyut_modal_plot", height = "100%"))
    ))
  }
  observeEvent(input$buyut_top15,         ac_modal("En Kalabal\u0131k 15 \u00dclke (2026)", ciz_top15))
  observeEvent(input$buyut_pasta,         ac_modal("K\u0131talara G\u00f6re N\u00fcfus Da\u011f\u0131l\u0131m\u0131", ciz_pasta))
  observeEvent(input$buyut_kita_yogunluk, ac_modal("K\u0131talara G\u00f6re N\u00fcfus Yo\u011funlu\u011fu", ciz_kita_yogunluk))
  observeEvent(input$buyut_yogun,         ac_modal("En Yo\u011fun 10 \u00dclke", ciz_en_yogun))
  observeEvent(input$buyut_seyrek,        ac_modal("En Seyrek 10 \u00dclke", ciz_en_seyrek))

  # ============================================================
  # SEKME 2: INTERAKTIF HARITA (LEAFLET - MODERN)
  # ============================================================
  # Acilista Turkiye secili gelsin (WPP country_code 792)
  secili_ulke <- reactiveVal(792L)

  # Tum yillar boyunca metriklerin global aralikleri (yil degisince renk tutarli kalmasi icin)
  # Outlier'lari kirpmak icin sabit/percentile-tabanli araliklar
  global_pop_range <- range(pop_dt$pop[pop_dt$pop > 0], na.rm = TRUE)
  # Buyume orani: outlier extreme degerleri (-71, +37) atla, makul sabit aralik
  global_buy_range <- c(-3, 3)
  # TFR: real range OK (0.66 - 8.86)
  global_tfr_range <- c(1, 7)
  # Yasam suresi: outlier (10.9) atla, makul aralik
  global_e0_range <- c(45, 90)
  # Net goc: 99. percentile mutlak degeri
  global_mig_range <- as.numeric(quantile(abs(mig_dt$mig), 0.99, na.rm = TRUE))

  # Baslangic verisi fonksiyonu (2026, yogunluk)
  harita_renk_hesapla <- function(sf_data, dt, alan, metrik) {
    idx <- match(sf_data$country_code, dt$country_code)
    # Defaults — gecersiz metrik gelirse fallback
    legend_baslik <- "De\u011fer"
    pal <- colorNumeric(viridis(256), domain = c(0, 1), na.color = "#e0e0e0")
    sf_data$deger <- rep(NA_real_, nrow(sf_data))
    fill_renk <- rep("#e0e0e0", nrow(sf_data))

    if (metrik == "pop") {
      sf_data$deger <- dt$pop[idx]
      legend_baslik <- "N\u00fcfus"
      pal <- colorNumeric(viridis(256, option = "plasma"),
                          domain = log10(pmax(global_pop_range, 1)),
                          na.color = "#e0e0e0")
      fill_renk <- pal(log10(pmax(sf_data$deger, 1)))
    } else if (metrik == "yogunluk") {
      sf_data$deger <- ifelse(!is.na(alan) & alan > 0, dt$pop[idx] / alan, NA_real_)
      legend_baslik <- "Yo\u011funluk (ki\u015fi/km\u00b2)"
      yog_esikler <- c(0, 25, 50, 100, 200, 500, 1000, Inf)
      yog_renkler <- c("#fff5b3", "#ffd500", "#ffaa00", "#ff7f00", "#ff5500", "#e31a1c", "#a50f15")
      yog_etiketler <- c("< 25", "25-50", "50-100", "100-200", "200-500", "500-1K", "1K+")
      sf_data$yog_grup <- cut(sf_data$deger, breaks = yog_esikler, right = FALSE,
                              labels = yog_etiketler, include.lowest = TRUE)
      pal <- colorFactor(yog_renkler, levels = yog_etiketler, na.color = "#e0e0e0")
      fill_renk <- pal(sf_data$yog_grup)
    } else if (metrik == "buyume") {
      sf_data$deger <- dt$growthrate[idx]
      legend_baslik <- "B\u00fcy\u00fcme (%)"
      pal <- colorNumeric(c("#d73027", "#ffffbf", "#1a9850"),
                          domain = global_buy_range,
                          na.color = "#e0e0e0")
      # Aralik disi degerleri kirp
      kirp <- pmax(pmin(sf_data$deger, global_buy_range[2]), global_buy_range[1])
      fill_renk <- pal(kirp)
    } else if (metrik == "tfr") {
      sf_data$deger <- dt$tfr[idx]
      legend_baslik <- "TFR"
      pal <- colorNumeric(c("#2166ac", "#f7f7f7", "#b2182b"),
                          domain = global_tfr_range,
                          na.color = "#e0e0e0")
      kirp <- pmax(pmin(sf_data$deger, global_tfr_range[2]), global_tfr_range[1])
      fill_renk <- pal(kirp)
    } else if (metrik == "goc") {
      sf_data$deger <- dt$mig[idx]
      legend_baslik <- "Net G\u00f6\u00e7"
      pal <- colorNumeric(c("#d73027", "#f7f7f7", "#4575b4"),
                          domain = c(-global_mig_range, global_mig_range), na.color = "#e0e0e0")
      kirp <- pmax(pmin(sf_data$deger, global_mig_range), -global_mig_range)
      fill_renk <- pal(kirp)
    } else if (metrik == "e0") {
      sf_data$deger <- dt$e0B[idx]
      legend_baslik <- "Ya\u015fam S\u00fcresi"
      pal <- colorNumeric(viridis(256, option = "viridis"),
                          domain = global_e0_range,
                          na.color = "#e0e0e0")
      kirp <- pmax(pmin(sf_data$deger, global_e0_range[2]), global_e0_range[1])
      fill_renk <- pal(kirp)
    }
    fill_renk[is.na(fill_renk)] <- "#e0e0e0"
    # Tooltip olustur — modern kart
    kita_tr_vec <- kita_tr[sf_data$continent]
    etiketler <- lapply(seq_len(nrow(sf_data)), function(i) {
      deger_str <- if (is.na(sf_data$deger[i])) "-"
                   else if (metrik == "pop") fmt_sayi(sf_data$deger[i])
                   else if (metrik == "yogunluk") paste0(round(sf_data$deger[i], 1), " ki\u015fi/km\u00b2")
                   else if (metrik == "buyume") paste0(round(sf_data$deger[i], 2), "%")
                   else if (metrik == "tfr") round(sf_data$deger[i], 2)
                   else if (metrik == "goc") fmt_sayi(sf_data$deger[i])
                   else if (metrik == "e0") paste0(round(sf_data$deger[i], 1), " y\u0131l")
                   else round(sf_data$deger[i], 1)
      ulke_tooltip_html(sf_data$name_tr[i], kita_tr_vec[i], as.character(deger_str), legend_baslik)
    })
    list(sf_data = sf_data, fill_renk = fill_renk, pal = pal, legend_baslik = legend_baslik,
         metrik = metrik, etiketler = etiketler)
  }

  # Base harita — ilk acilista 2026 yogunluk ile birlikte render
  output$ana_harita <- renderLeaflet({
    # Baslangic verisi: 2026 + yogunluk
    dt0 <- ulke_veri_yil(2026)
    sf0 <- ulke_sf
    ref_idx0 <- match(sf0$country_code, ulke_ref$country_code)
    alan0 <- ulke_ref$Alan_km2[ref_idx0]
    r0 <- harita_renk_hesapla(sf0, dt0, alan0, "yogunluk")

    leaflet(options = leafletOptions(minZoom = 2, maxZoom = 8, worldCopyJump = FALSE,
                                   maxBounds = list(list(-85, -180), list(85, 180)),
                                   maxBoundsViscosity = 1.0)) %>%
      addMapPane("zemin_pane", zIndex = 200) %>%
      addMapPane("polygon_pane", zIndex = 400) %>%
      addMapPane("cartodb_label_pane", zIndex = 450) %>%
      addMapPane("tr_label_pane", zIndex = 460) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "zemin",
                       options = providerTileOptions(pane = "zemin_pane", noWrap = TRUE)) %>%
      setView(lng = 30, lat = 25, zoom = 2) %>%
      addPolygons(
        data = r0$sf_data, group = "ulkeler",
        layerId = r0$sf_data$country_code,
        fillColor = r0$fill_renk,
        weight = 0.8, color = "#334155", opacity = 0.55, fillOpacity = 1,
        options = pathOptions(pane = "polygon_pane"),
        label = r0$etiketler,
        labelOptions = labelOptions(className = "ulke-tooltip-wrap", textsize = "12px"),
        highlightOptions = highlightOptions(weight = 3, color = "#ffffff", fillOpacity = 1, bringToFront = FALSE)
      ) %>%
      ekle_ulke_etiketleri() %>%
      addLegend(
        layerId = "legend",
        position = "bottomright", pal = r0$pal,
        values = factor(c("< 25", "25-50", "50-100", "100-200", "200-500", "500-1K", "1K+"),
                        levels = c("< 25", "25-50", "50-100", "100-200", "200-500", "500-1K", "1K+")),
        title = r0$legend_baslik, opacity = 0.9
      ) %>%
      addControl(
        html = "<div class='yil-gosterge'><div class='yil-etiket'>YIL</div><div class='yil-deger'>2026</div></div>",
        position = "topright",
        layerId = "yil_kontrol",
        className = "yil-kontrol-wrap"
      )
  })

  # Harita JS hizli guncelleme yardimcisi: polygonu silmeden renk+tooltip+yil rozeti yolla
  harita_hizli_guncelle <- function(map_id, sf_data, fill_renk, etiketler, yil = NULL) {
    mesaj <- list(
      map_id   = map_id,
      ids      = as.character(sf_data$country_code),
      colors   = as.character(fill_renk),
      tooltips = vapply(etiketler, as.character, character(1))
    )
    if (!is.null(yil)) mesaj$year <- as.character(yil)
    session$sendCustomMessage("updateMapColors", mesaj)
  }

  # Sadece yil rozetini guncelle (color/tooltip dokunmayan observer'lar icin)
  harita_yil_rozeti <- function(map_id, yil) {
    session$sendCustomMessage("updateMapColors", list(
      map_id = map_id,
      year   = as.character(yil)
    ))
  }

  # Harita guncelleme — yil veya metrik degistiginde (throttled)
  harita_yil_r <- reactive(input$harita_yil) %>% throttle(300)
  .harita_prev_metrik <- reactiveVal("yogunluk")  # renderLeaflet ilk metrik
  observe({
    yil <- harita_yil_r()
    metrik <- input$harita_metrik
    if (is.null(yil) || is.null(metrik)) return()

    dt <- ulke_veri_yil(yil)
    sf_data <- ulke_sf
    ref_idx <- match(sf_data$country_code, ulke_ref$country_code)
    alan <- ulke_ref$Alan_km2[ref_idx]

    r <- harita_renk_hesapla(sf_data, dt, alan, metrik)

    # Hizli yol: polygonlari silmeden sadece fillColor + tooltip + yil rozeti guncelle (JS)
    harita_hizli_guncelle("ana_harita", r$sf_data, r$fill_renk, r$etiketler, yil = yil)

    # Legend sadece metrik degisince yenilenir (yil degisimi global aralikta legend'i etkilemez)
    prev_m <- isolate(.harita_prev_metrik())
    if (!identical(prev_m, metrik)) {
      .harita_prev_metrik(metrik)
      leafletProxy("ana_harita") %>%
        removeControl("legend") %>%
        addLegend(
          layerId = "legend",
          position = "bottomright", pal = r$pal,
          values = if (metrik == "yogunluk") {
                     factor(c("< 25","25-50","50-100","100-200","200-500","500-1K","1K+"),
                            levels = c("< 25","25-50","50-100","100-200","200-500","500-1K","1K+"))
                   } else if (metrik == "pop") log10(pmax(global_pop_range, 1))
                   else if (metrik == "buyume") global_buy_range
                   else if (metrik == "tfr") global_tfr_range
                   else if (metrik == "e0") global_e0_range
                   else if (metrik == "goc") c(-global_mig_range, global_mig_range)
                   else r$sf_data$deger[!is.na(r$sf_data$deger)],
          title = r$legend_baslik, opacity = 0.9,
          labFormat = if (metrik == "pop") labelFormat(transform = function(x) round(10^x))
                      else labelFormat()
        )
    }
  })

  # Ulke tiklama ile sol panel guncellenir
  observeEvent(input$ana_harita_shape_click, {
    click <- input$ana_harita_shape_click
    if (!is.null(click$id)) secili_ulke(as.integer(click$id))
  })

  # Detay paneli (modern kart tasarimi)
  # Animasyon sirasinda kart yeniden insa edilmesin diye debounce — slider durunca guncelle
  harita_yil_detay <- reactive(input$harita_yil) |> debounce(400)
  detay_veri <- reactive({
    cc <- secili_ulke()
    yil <- harita_yil_detay()
    if (is.null(cc) || is.na(cc) || is.null(yil)) return(NULL)
    dt <- ulke_veri_yil(yil)
    row <- dt[country_code == cc]
    ref <- ulke_ref[country_code == cc]
    if (nrow(row) == 0 || nrow(ref) == 0) return(list(bulunamadi = TRUE))
    alan_val <- ref$Alan_km2[1]
    yogunluk <- if (!is.na(alan_val) && alan_val > 0) round(row$pop / alan_val, 1) else NA
    age_row <- age_dt[country_code == cc & year == yil]
    if (nrow(age_row) > 0) {
      total_pop <- sum(age_row$pop, na.rm = TRUE)
      if (total_pop > 0) {
        genc_pct <- round(100 * sum(age_row$pop[age_row$age < 15], na.rm = TRUE) / total_pop, 1)
        yetiskin_pct <- round(100 * sum(age_row$pop[age_row$age >= 15 & age_row$age < 65], na.rm = TRUE) / total_pop, 1)
        yasli_pct <- round(100 * sum(age_row$pop[age_row$age >= 65], na.rm = TRUE) / total_pop, 1)
      } else genc_pct <- yetiskin_pct <- yasli_pct <- NA
    } else genc_pct <- yetiskin_pct <- yasli_pct <- NA
    wm_row <- wmeter_dt[country_code == cc]
    list(
      row = row, ref = ref, alan_val = alan_val, yogunluk = yogunluk,
      genc_pct = genc_pct, yetiskin_pct = yetiskin_pct, yasli_pct = yasli_pct,
      medyan_yas = if (nrow(wm_row) > 0) wm_row$wm_median_age[1] else NA,
      sehir_pct = if (nrow(wm_row) > 0) wm_row$wm_urban_pct[1] else NA,
      yil = yil, bulunamadi = FALSE
    )
  })

  output$harita_detay <- renderUI({
    cc <- secili_ulke()
    if (is.null(cc) || is.na(cc)) {
      return(tags$div(
        style = "padding:14px;background:#f8f9fa;border-radius:10px;border:1px dashed #cbd5e1;text-align:center;margin:6px 0;",
        tags$div(style="font-size:24px;margin-bottom:4px;", HTML("&#127757;")),
        tags$div(style="color:#64748b;font-size:12px;font-weight:500;",
                 "Bir \u00fclkeye t\u0131klay\u0131n")
      ))
    }
    d <- detay_veri()
    if (is.null(d)) return(NULL)
    if (isTRUE(d$bulunamadi)) return(tags$p("Veri bulunamad\u0131"))
    # Veriyi paketten ac
    row <- d$row; ref <- d$ref
    alan_val <- d$alan_val; yogunluk <- d$yogunluk
    genc_pct <- d$genc_pct; yetiskin_pct <- d$yetiskin_pct; yasli_pct <- d$yasli_pct
    medyan_yas <- d$medyan_yas; sehir_pct <- d$sehir_pct
    yil <- d$yil

    # Yardimci: stat satiri
    stat_row <- function(label, value, color = "#1a1a2e") {
      tags$div(
        style = "display:flex;justify-content:space-between;align-items:center;padding:3px 0;font-size:12px;",
        tags$span(style="color:#64748b;", label),
        tags$span(style=paste0("color:", color, ";font-weight:700;"), value)
      )
    }

    tags$div(
      style = "background:#fff;border-radius:10px;box-shadow:0 1px 4px rgba(0,0,0,0.08);overflow:hidden;margin:6px 0;",
      # Header
      tags$div(
        style = "background:linear-gradient(135deg,#1a1a2e 0%,#3b3b5c 100%);color:#fff;padding:10px 14px;",
        tags$div(style="font-size:15px;font-weight:800;letter-spacing:0.3px;", ref$name_tr),
        tags$div(style="font-size:10px;opacity:0.7;margin-top:2px;", paste0("Y\u0131l ", yil)),
        # Demografik Scorecard — yaslanan/genc/stabil/azalan
        {
          kategori <- NA_character_
          renk <- "#64748b"
          ikon <- HTML("&#9898;")
          if (!is.na(row$growthrate) && row$growthrate < 0) {
            kategori <- "N\u00fcfus Azalmas\u0131"; renk <- "#dc2626"; ikon <- HTML("&#128315;")
          } else if (!is.na(yasli_pct) && yasli_pct >= 20) {
            kategori <- "Ya\u015flanan"; renk <- "#a855f7"; ikon <- HTML("&#127795;")
          } else if (!is.na(row$tfr) && row$tfr >= 3 && !is.na(genc_pct) && genc_pct >= 30) {
            kategori <- "Gen\u00e7 Nufus"; renk <- "#22c55e"; ikon <- HTML("&#127919;")
          } else if (!is.na(row$tfr) && row$tfr < 2.1 && !is.na(row$e0B) && row$e0B >= 70) {
            kategori <- "Olgun/Stabil"; renk <- "#3b82f6"; ikon <- HTML("&#9878;")
          } else {
            kategori <- "Ge\u00e7i\u015f Halinde"; renk <- "#f59e0b"; ikon <- HTML("&#9203;")
          }
          tags$div(
            style = paste0("display:inline-flex;align-items:center;gap:5px;margin-top:6px;padding:4px 9px;",
                            "background:", renk, ";color:#fff;border-radius:999px;font-size:10px;",
                            "font-weight:700;letter-spacing:0.3px;"),
            tags$span(ikon), kategori
          )
        }
      ),
      # Ana metrikler
      tags$div(style="padding:10px 14px;",
        tags$div(
          style="display:flex;gap:8px;margin-bottom:8px;",
          tags$div(
            style="flex:1;background:#fef2f6;border-radius:6px;padding:6px 8px;",
            tags$div(style="font-size:9px;color:#cc4778;font-weight:600;text-transform:uppercase;", "N\u00fcfus"),
            tags$div(style="font-size:13px;color:#1a1a2e;font-weight:800;",
                     ifelse(is.na(row$pop), "-", fmt_sayi(row$pop)))
          ),
          tags$div(
            style="flex:1;background:#f5f3ff;border-radius:6px;padding:6px 8px;",
            tags$div(style="font-size:9px;color:#7e03a8;font-weight:600;text-transform:uppercase;", "Yo\u011funluk"),
            tags$div(style="font-size:13px;color:#1a1a2e;font-weight:800;",
                     ifelse(is.na(yogunluk), "-", paste0(yogunluk)))
          )
        ),
        # Diger metrikler
        stat_row("Alan", ifelse(is.na(alan_val), "-", paste0(fmt_sayi(alan_val), " km\u00b2"))),
        stat_row("B\u00fcy\u00fcme",
                 ifelse(is.na(row$growthrate), "-", paste0(round(row$growthrate, 2), "%")),
                 color = ifelse(is.na(row$growthrate), "#64748b",
                                ifelse(row$growthrate >= 0, "#16a34a", "#dc2626"))),
        stat_row("Do\u011furganl\u0131k (TFR)", ifelse(is.na(row$tfr), "-", round(row$tfr, 2))),
        stat_row("Ya\u015fam S\u00fcresi",
                 ifelse(is.na(row$e0B), "-", paste0(round(row$e0B, 1), " y\u0131l"))),
        stat_row("Net G\u00f6\u00e7", fmt_sayi(row$mig)),
        # Yas dagilimi bar
        tags$div(style="margin-top:8px;padding-top:6px;border-top:1px solid #f0f0f0;",
          tags$div(style="font-size:10px;color:#64748b;font-weight:600;text-transform:uppercase;margin-bottom:4px;",
                   "Ya\u015f Da\u011f\u0131l\u0131m\u0131"),
          tags$div(
            style="display:flex;height:18px;border-radius:4px;overflow:hidden;",
            if (!is.na(genc_pct)) tags$div(
              style=paste0("background:#22c55e;width:", genc_pct, "%;display:flex;align-items:center;justify-content:center;color:#fff;font-size:9px;font-weight:700;"),
              title = paste0("Gen\u00e7 (0-14): %", genc_pct),
              if (genc_pct >= 8) paste0(genc_pct, "%") else ""
            ),
            if (!is.na(yetiskin_pct)) tags$div(
              style=paste0("background:#3b82f6;width:", yetiskin_pct, "%;display:flex;align-items:center;justify-content:center;color:#fff;font-size:9px;font-weight:700;"),
              title = paste0("Yeti\u015fkin (15-64): %", yetiskin_pct),
              if (yetiskin_pct >= 8) paste0(yetiskin_pct, "%") else ""
            ),
            if (!is.na(yasli_pct)) tags$div(
              style=paste0("background:#a855f7;width:", yasli_pct, "%;display:flex;align-items:center;justify-content:center;color:#fff;font-size:9px;font-weight:700;"),
              title = paste0("Ya\u015fl\u0131 (65+): %", yasli_pct),
              if (yasli_pct >= 8) paste0(yasli_pct, "%") else ""
            )
          ),
          tags$div(
            style="display:flex;justify-content:space-around;font-size:9px;color:#64748b;margin-top:3px;",
            tags$span(style="color:#22c55e;", HTML("&#x25CF; Gen\u00e7")),
            tags$span(style="color:#3b82f6;", HTML("&#x25CF; Yeti\u015fkin")),
            tags$span(style="color:#a855f7;", HTML("&#x25CF; Ya\u015fl\u0131"))
          )
        ),
        # Worldometer 2025 ek bilgiler
        if (!is.na(medyan_yas) || !is.na(sehir_pct)) tags$div(
          style="margin-top:8px;padding-top:6px;border-top:1px solid #f0f0f0;",
          tags$div(style="font-size:9px;color:#94a3b8;font-weight:600;text-transform:uppercase;margin-bottom:4px;",
                   "Worldometer 2025"),
          stat_row("Medyan Ya\u015f",
                   ifelse(is.na(medyan_yas), "-", paste0(round(medyan_yas, 1), " y\u0131l"))),
          stat_row("\u015eehirli N\u00fcfus",
                   ifelse(is.na(sehir_pct), "-", paste0("%", round(sehir_pct, 1))))
        ),
        # Sekmeler arasi gecis butonlari
        tags$div(style = "margin-top:10px;display:flex;gap:6px;flex-wrap:wrap;",
          actionButton("git_piramit", tagList(icon("chart-bar"), " Ya\u015f Piramidi"),
                       class = "btn btn-sm btn-outline-primary",
                       style = "flex:1;font-size:11px;border-radius:8px;"),
          actionButton("git_karsilastir", tagList(icon("scale-balanced"), " Kar\u015f\u0131la\u015ft\u0131r"),
                       class = "btn btn-sm btn-outline-secondary",
                       style = "flex:1;font-size:11px;border-radius:8px;")
        )
      )
    )
  })

  # Haritadan secili ulkeyi Yas Piramidi sekmesine tasi
  observeEvent(input$git_piramit, {
    cc <- secili_ulke()
    if (is.null(cc) || is.na(cc)) return()
    updateSelectizeInput(session, "pir_ulke1", selected = as.character(cc))
    bslib::nav_select("ana_nav", "Ya\u015f Piramidi")
  })

  # Haritadan secili ulkeyi Karsilastirma sekmesine ekle
  observeEvent(input$git_karsilastir, {
    cc <- secili_ulke()
    if (is.null(cc) || is.na(cc)) return()
    mevcut <- input$kars_ulkeler
    if (is.null(mevcut)) mevcut <- character(0)
    if (!as.character(cc) %in% mevcut) {
      mevcut <- c(mevcut, as.character(cc))
    }
    updateSelectizeInput(session, "kars_ulkeler", selected = mevcut)
    bslib::nav_select("ana_nav", "Kar\u015f\u0131la\u015ft\u0131rma")
  })

  output$harita_piramit <- renderPlotly({
    cc <- secili_ulke()
    if (is.null(cc)) return(plotly_empty() %>%
                            layout(plot_bgcolor = "transparent",
                                   paper_bgcolor = "transparent",
                                   xaxis = list(visible = FALSE),
                                   yaxis = list(visible = FALSE)) %>%
                            config(displayModeBar = FALSE))
    # Animasyon sirasinda yeniden cizim yok — slider durunca guncelle
    piramit_ciz(cc, harita_yil_detay(), kompakt = TRUE)
  })

  # ============================================================
  # SEKME 3: NUFUS PROJEKSIYONU
  # ============================================================
  output$proj_grafik <- renderPlotly({
    ulkeler <- input$proj_ulkeler
    metrikler <- input$proj_metrik
    if (is.null(ulkeler) || length(ulkeler) == 0)
      return(bos_grafik("En az bir \u00fclke se\u00e7in", "\U0001F30D"))
    if (is.null(metrikler) || length(metrikler) == 0)
      return(bos_grafik("En az bir metrik se\u00e7in", "\U0001F4C8"))

    renkler <- viridis(length(ulkeler), option = "turbo")
    metrik_baslik <- c(pop = "N\u00fcfus (Milyon)", tfr = "TFR",
                       e0 = "Ya\u015fam S\u00fcresi (y\u0131l)", goc = "Net G\u00f6\u00e7")

    # Tek metrik: normal cizgi grafik
    if (length(metrikler) == 1) {
      p <- suppressWarnings(plot_ly())
      for (i in seq_along(ulkeler)) {
        cc <- suppressWarnings(as.integer(ulkeler[i]))
        if (is.na(cc)) next
        ulke_adi <- ulke_ref[country_code == cc, name_tr]
        if (length(ulke_adi) == 0 || is.na(ulke_adi[1])) ulke_adi <- as.character(cc)
        met <- metrikler[1]
        dt <- if (met == "pop") pop_dt[country_code == cc, .(year, deger = pop / 1e6)]
              else if (met == "tfr") tfr_dt[country_code == cc, .(year, deger = tfr)]
              else if (met == "e0") e0_dt[country_code == cc, .(year, deger = e0B)]
              else mig_dt[country_code == cc, .(year, deger = mig)]
        if (nrow(dt) == 0) next
        gecmis <- dt[year <= 2023]; proj <- dt[year >= 2023]
        p <- p %>%
          add_trace(data = gecmis, x = ~year, y = ~deger, type = "scatter", mode = "lines",
                    line = list(color = renkler[i], width = 2), name = ulke_adi,
                    legendgroup = ulke_adi, showlegend = TRUE) %>%
          add_trace(data = proj, x = ~year, y = ~deger, type = "scatter", mode = "lines",
                    line = list(color = renkler[i], width = 2, dash = "dash"),
                    name = paste0(ulke_adi, " (proj)"), legendgroup = ulke_adi, showlegend = FALSE)
      }
      p %>% layout(
        dragmode = "pan",
        hovermode = "x unified",
        xaxis = list(title = "Y\u0131l", fixedrange = TRUE),
        yaxis = list(title = metrik_baslik[metrikler[1]], fixedrange = TRUE),
        shapes = list(list(type = "line", x0 = 2023, x1 = 2023, y0 = 0, y1 = 1, yref = "paper",
                           line = list(color = "red", width = 1, dash = "dot"))),
        annotations = list(list(x = 2023, y = 1.02, yref = "paper", text = "2023", showarrow = FALSE,
                                font = list(size = 10, color = "red")))
      ) %>% config(displayModeBar = FALSE, scrollZoom = FALSE)

    } else {
      # Coklu metrik: facet subplot (her metrik ayri panel)
      alt_grafikler <- lapply(metrikler, function(met) {
        p <- suppressWarnings(plot_ly())
        for (i in seq_along(ulkeler)) {
          cc <- suppressWarnings(as.integer(ulkeler[i]))
          if (is.na(cc)) next
          ulke_adi <- ulke_ref[country_code == cc, name_tr]
          if (length(ulke_adi) == 0 || is.na(ulke_adi[1])) ulke_adi <- as.character(cc)
          dt <- if (met == "pop") pop_dt[country_code == cc, .(year, deger = pop / 1e6)]
                else if (met == "tfr") tfr_dt[country_code == cc, .(year, deger = tfr)]
                else if (met == "e0") e0_dt[country_code == cc, .(year, deger = e0B)]
                else mig_dt[country_code == cc, .(year, deger = mig)]
          if (nrow(dt) == 0) next
          gecmis <- dt[year <= 2023]; proj <- dt[year >= 2023]
          goster_legend <- (met == metrikler[1])
          p <- p %>%
            add_trace(data = gecmis, x = ~year, y = ~deger, type = "scatter", mode = "lines",
                      line = list(color = renkler[i], width = 2), name = ulke_adi,
                      legendgroup = ulke_adi, showlegend = goster_legend) %>%
            add_trace(data = proj, x = ~year, y = ~deger, type = "scatter", mode = "lines",
                      line = list(color = renkler[i], width = 2, dash = "dash"),
                      name = paste0(ulke_adi, " (proj)"), legendgroup = ulke_adi, showlegend = FALSE)
        }
        p %>% layout(
          dragmode = "pan",
          yaxis = list(title = metrik_baslik[met], fixedrange = TRUE),
          shapes = list(list(type = "line", x0 = 2023, x1 = 2023, y0 = 0, y1 = 1, yref = "paper",
                             line = list(color = "red", width = 1, dash = "dot")))
        )
      })
      subplot(alt_grafikler, nrows = length(metrikler), shareX = TRUE, titleY = TRUE) %>%
        layout(dragmode = "pan", xaxis = list(title = "Y\u0131l", fixedrange = TRUE)) %>%
        config(displayModeBar = FALSE, scrollZoom = FALSE)
    }
  }) |> bindCache(input$proj_ulkeler, input$proj_metrik)

  # ============================================================
  # SEKME 4: YAS PIRAMIDI KARSILASTIRMA
  # ============================================================
  # Piramit animasyonunda her frame'de yeniden cizim yapilmasin — debounce
  pir_yil_d <- reactive(input$pir_yil) |> debounce(400)

  output$pir_baslik1 <- renderText({
    yil <- pir_yil_d()
    if (is.null(input$pir_ulke1) || is.null(yil)) return("")
    cc <- suppressWarnings(as.integer(input$pir_ulke1))
    if (is.na(cc)) return("")
    ref <- ulke_ref[country_code == cc]
    if (nrow(ref) > 0) paste0(ref$name_tr, " (", yil, ")") else ""
  })
  output$pir_baslik2 <- renderText({
    yil <- pir_yil_d()
    if (is.null(input$pir_ulke2) || is.null(yil)) return("")
    cc <- suppressWarnings(as.integer(input$pir_ulke2))
    if (is.na(cc)) return("")
    ref <- ulke_ref[country_code == cc]
    if (nrow(ref) > 0) paste0(ref$name_tr, " (", yil, ")") else ""
  })

  output$piramit1 <- renderPlotly({
    yil <- pir_yil_d()
    if (is.null(input$pir_ulke1) || is.null(yil)) return(bos_grafik("\u00dclke se\u00e7in", "\U0001F464"))
    cc <- suppressWarnings(as.integer(input$pir_ulke1))
    if (is.na(cc)) return(bos_grafik("Ge\u00e7ersiz \u00fclke", "\u26A0\uFE0F"))
    piramit_ciz(cc, yil)
  })
  output$piramit2 <- renderPlotly({
    yil <- pir_yil_d()
    if (is.null(input$pir_ulke2) || is.null(yil)) return(bos_grafik("\u00dclke se\u00e7in", "\U0001F464"))
    cc <- suppressWarnings(as.integer(input$pir_ulke2))
    if (is.na(cc)) return(bos_grafik("Ge\u00e7ersiz \u00fclke", "\u26A0\uFE0F"))
    piramit_ciz(cc, yil)
  })

  # ============================================================
  # SEKME 5: TEMATIK HARITALAR
  # ============================================================

  # Helper: HTML-safe ülke ismi
  html_safe <- function(x) htmltools::htmlEscape(x)

  # Helper: tematik harita verisi hazirla (renkler + etiketler)
  tema_hazirla <- function(yil, tip) {
    sf_data <- ulke_sf
    deger_fmt <- function(v) ifelse(is.na(v), "-", as.character(v))
    deger_etiketi <- ""
    if (tip == "goc") {
      d <- mig_dt[year == yil, .(country_code, val = mig)]
      legend_baslik <- "Net G\u00f6\u00e7"
      deger_etiketi <- "Net G\u00f6\u00e7"
      deger_fmt <- function(v) ifelse(is.na(v), "-", fmt_sayi(v))
      sf_data$deger <- d$val[match(sf_data$country_code, d$country_code)]
      degerler <- sf_data$deger[!is.na(sf_data$deger)]
      if (length(degerler) == 0) return(NULL)
      max_abs <- max(abs(degerler))
      pal <- colorNumeric(c("#d73027", "#f7f7f7", "#4575b4"),
                          domain = c(-max_abs, max_abs), na.color = "#e0e0e0")
    } else if (tip == "tfr") {
      d <- tfr_dt[year == yil, .(country_code, val = tfr)]
      legend_baslik <- "Do\u011furganl\u0131k (TFR)"
      deger_etiketi <- "TFR"
      deger_fmt <- function(v) ifelse(is.na(v), "-", as.character(round(v, 2)))
      sf_data$deger <- d$val[match(sf_data$country_code, d$country_code)]
      degerler <- sf_data$deger[!is.na(sf_data$deger)]
      if (length(degerler) == 0) return(NULL)
      pal <- colorNumeric(c("#313695", "#ffffbf", "#a50026"),
                          domain = range(degerler), na.color = "#e0e0e0")
    } else if (tip == "e0") {
      d <- e0_dt[year == yil, .(country_code, val = e0B)]
      legend_baslik <- "Ya\u015fam S\u00fcresi (y\u0131l)"
      deger_etiketi <- "Ya\u015fam S\u00fcresi"
      deger_fmt <- function(v) ifelse(is.na(v), "-", paste0(round(v, 1), " y\u0131l"))
      sf_data$deger <- d$val[match(sf_data$country_code, d$country_code)]
      degerler <- sf_data$deger[!is.na(sf_data$deger)]
      if (length(degerler) == 0) return(NULL)
      pal <- colorNumeric(viridis(256), domain = range(degerler), na.color = "#e0e0e0")
    } else if (tip == "buyume") {
      d <- misc_dt[year == yil, .(country_code, val = growthrate)]
      legend_baslik <- "B\u00fcy\u00fcme (%)"
      deger_etiketi <- "B\u00fcy\u00fcme"
      deger_fmt <- function(v) ifelse(is.na(v), "-", paste0(round(v, 2), "%"))
      sf_data$deger <- d$val[match(sf_data$country_code, d$country_code)]
      degerler <- sf_data$deger[!is.na(sf_data$deger)]
      if (length(degerler) == 0) return(NULL)
      max_abs <- max(abs(degerler))
      pal <- colorNumeric(c("#d73027", "#ffffbf", "#1a9850"),
                          domain = c(-max_abs, max_abs), na.color = "#e0e0e0")
    } else if (tip == "yas") {
      legend_baslik <- "Medyan Ya\u015f (2025)"
      deger_etiketi <- "Medyan Ya\u015f"
      deger_fmt <- function(v) ifelse(is.na(v), "-", paste0(round(v, 1), " y\u0131l"))
      sf_data$deger <- wmeter_dt$wm_median_age[match(sf_data$country_code, wmeter_dt$country_code)]
      degerler <- sf_data$deger[!is.na(sf_data$deger)]
      if (length(degerler) == 0) return(NULL)
      pal <- colorNumeric(c("#1a9850", "#ffffbf", "#d73027"),
                          domain = c(15, 50), na.color = "#e0e0e0")
    } else if (tip == "kent") {
      legend_baslik <- "\u015eehir N\u00fcfus % (2025)"
      deger_etiketi <- "\u015eehirli N\u00fcfus"
      deger_fmt <- function(v) ifelse(is.na(v), "-", paste0(round(v, 1), "%"))
      sf_data$deger <- wmeter_dt$wm_urban_pct[match(sf_data$country_code, wmeter_dt$country_code)]
      degerler <- sf_data$deger[!is.na(sf_data$deger)]
      if (length(degerler) == 0) return(NULL)
      pal <- colorNumeric(c("#f7fcb9", "#41ab5d", "#005a32"),
                          domain = c(0, 100), na.color = "#e0e0e0")
    }
    fill_renk <- pal(sf_data$deger)
    fill_renk[is.na(fill_renk)] <- "#e0e0e0"
    kita_tr_vec <- kita_tr[sf_data$continent]
    etiketler <- lapply(seq_len(nrow(sf_data)), function(i) {
      ulke_tooltip_html(sf_data$name_tr[i], kita_tr_vec[i],
                         deger_fmt(sf_data$deger[i]), deger_etiketi)
    })
    list(sf_data = sf_data, fill_renk = fill_renk, pal = pal, degerler = degerler,
         etiketler = etiketler, legend_baslik = legend_baslik)
  }

  # Helper: tematik haritayi base + polygonlar ile render et (acilista veriyle)
  tema_render <- function(tip) {
    r <- tema_hazirla(2026, tip)
    if (is.null(r)) {
      return(leaflet(ulke_sf, options = leafletOptions(minZoom = 2, maxZoom = 8, worldCopyJump = FALSE,
                                   maxBounds = list(list(-85, -180), list(85, 180)),
                                   maxBoundsViscosity = 1.0)) %>%
               addProviderTiles(providers$CartoDB.PositronNoLabels,
                                options = providerTileOptions(noWrap = TRUE)) %>%
               setView(lng = 30, lat = 25, zoom = 2))
    }
    base_map <- leaflet(options = leafletOptions(minZoom = 2, maxZoom = 8, worldCopyJump = FALSE,
                                   maxBounds = list(list(-85, -180), list(85, 180)),
                                   maxBoundsViscosity = 1.0)) %>%
      addMapPane("zemin_pane", zIndex = 200) %>%
      addMapPane("polygon_pane", zIndex = 400) %>%
      addMapPane("cartodb_label_pane", zIndex = 450) %>%
      addMapPane("tr_label_pane", zIndex = 460) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       options = providerTileOptions(pane = "zemin_pane", noWrap = TRUE)) %>%
      setView(lng = 30, lat = 25, zoom = 2) %>%
      addPolygons(data = r$sf_data, group = "veri",
                  layerId = r$sf_data$country_code,
                  fillColor = r$fill_renk,
                  weight = 0.8, color = "#334155", opacity = 0.55, fillOpacity = 1,
                  options = pathOptions(pane = "polygon_pane"),
                  label = r$etiketler,
                  labelOptions = labelOptions(className = "ulke-tooltip-wrap", textsize = "12px"),
                  highlightOptions = highlightOptions(weight = 2, color = "#fff",
                                                      fillOpacity = 1, bringToFront = FALSE)) %>%
      ekle_ulke_etiketleri() %>%
      addLegend(layerId = "legend", position = "bottomright", pal = r$pal, values = r$degerler,
                title = r$legend_baslik, opacity = 0.85)
    # Yil rozeti — sadece yila bagli tematik haritalarda
    if (tip %in% c("goc", "tfr", "e0", "buyume")) {
      base_map <- base_map %>% addControl(
        html = "<div class='yil-gosterge'><div class='yil-etiket'>YIL</div><div class='yil-deger'>2026</div></div>",
        position = "topright",
        layerId = "yil_kontrol",
        className = "yil-kontrol-wrap"
      )
    }
    base_map
  }

  # Helper: tematik haritayi guncelle (hizli yol — polygonlari silmeden)
  tema_guncelle <- function(map_id, yil, tip) {
    r <- tema_hazirla(yil, tip)
    if (is.null(r)) return()
    harita_hizli_guncelle(map_id, r$sf_data, r$fill_renk, r$etiketler, yil = yil)
    # Legend her yil degisiminde yenilenmeli (pal domain yil verisine gore degisir)
    leafletProxy(map_id) %>%
      removeControl("legend") %>%
      addLegend(layerId = "legend", position = "bottomright", pal = r$pal, values = r$degerler,
                title = r$legend_baslik, opacity = 0.85)
  }

  # Tematik haritalar — sadece gorunen sub-tab'i guncelle (6 observe -> 1)
  output$tema_goc_harita <- renderLeaflet({ tema_render("goc") })
  output$tema_tfr_harita <- renderLeaflet({ tema_render("tfr") })
  output$tema_e0_harita  <- renderLeaflet({ tema_render("e0") })
  output$tema_buy_harita <- renderLeaflet({ tema_render("buyume") })

  # Sub-tab adi -> (map_id, metrik)
  .tema_dispatch <- list(
    "Net G\u00f6\u00e7"       = list(id = "tema_goc_harita", metrik = "goc"),
    "Do\u011furganl\u0131k (TFR)" = list(id = "tema_tfr_harita", metrik = "tfr"),
    "Ya\u015fam S\u00fcresi"      = list(id = "tema_e0_harita",  metrik = "e0"),
    "B\u00fcy\u00fcme Oran\u0131"  = list(id = "tema_buy_harita", metrik = "buyume")
  )
  observe({
    yil <- input$tema_yil
    sekme <- input$tema_sub_nav
    if (is.null(yil) || is.null(sekme)) return()
    d <- .tema_dispatch[[sekme]]
    if (is.null(d)) return()  # Gecis/Bag/Yas/Kent ayri ele aliniyor
    tema_guncelle(d$id, yil, d$metrik)
  })

  # --- Medyan Ya\u015f (Worldometer 2025) ---
  output$tema_yas_harita <- renderLeaflet({ tema_render("yas") })

  # --- \u015eehir N\u00fcfus % (Worldometer 2025) ---
  output$tema_kent_harita <- renderLeaflet({ tema_render("kent") })

  # ============================================================
  # SEKME: ULKE KARSILASTIRMA
  # ============================================================
  kars_renkler <- c("#6366f1", "#10b981", "#f59e0b", "#ec4899")

  kars_ccs <- reactive({
    u <- input$kars_ulkeler
    if (is.null(u) || length(u) == 0) return(integer(0))
    suppressWarnings(as.integer(u))
  })

  # Karsilastirma animasyonunda kartlar/piramitler her frame'de yeniden cizilmesin
  kars_yil_d <- reactive(input$kars_yil) |> debounce(400)

  output$kars_ozet_kartlar <- renderUI({
    ccs <- kars_ccs()
    yil <- kars_yil_d()
    if (length(ccs) == 0 || is.null(yil)) {
      return(tags$div(class = "tab-intro",
        tags$div(class = "tab-intro-body", "Kenar \u00e7ubu\u011funda en az 2 \u00fclke se\u00e7in.")))
    }
    dt <- ulke_veri_yil(yil)
    dt <- merge(dt, ulke_ref, by = "country_code", all.x = TRUE)

    kartlar <- lapply(seq_along(ccs), function(i) {
      cc <- ccs[i]
      row <- dt[country_code == cc]
      if (nrow(row) == 0) return(NULL)
      renk <- kars_renkler[((i - 1) %% 4) + 1]
      yogunluk <- if (!is.na(row$Alan_km2) && row$Alan_km2 > 0) round(row$pop / row$Alan_km2, 1) else NA

      tags$div(style = "flex:1;min-width:220px;",
        tags$div(
          style = paste0("background:#fff;border-radius:14px;overflow:hidden;box-shadow:0 2px 10px rgba(0,0,0,0.05);border-top:4px solid ", renk, ";"),
          tags$div(
            style = paste0("padding:14px 18px;background:linear-gradient(135deg,", renk, "15,", renk, "08);"),
            tags$div(style = paste0("font-size:16px;font-weight:800;color:", renk, ";"), row$name_tr),
            tags$div(style = "font-size:11px;color:#64748b;margin-top:2px;", paste0(row$continent, " \u2022 ", yil))
          ),
          tags$div(style = "padding:12px 18px;",
            lapply(list(
              list(label = "N\u00fcfus", val = ifelse(is.na(row$pop), "-", fmt_sayi(row$pop))),
              list(label = "Yo\u011funluk", val = ifelse(is.na(yogunluk), "-", paste0(yogunluk, " ki\u015fi/km\u00b2"))),
              list(label = "B\u00fcy\u00fcme", val = ifelse(is.na(row$growthrate), "-", paste0(round(row$growthrate, 2), "%"))),
              list(label = "TFR", val = ifelse(is.na(row$tfr), "-", round(row$tfr, 2))),
              list(label = "Ya\u015fam S\u00fcresi", val = ifelse(is.na(row$e0B), "-", paste0(round(row$e0B, 1), " y\u0131l"))),
              list(label = "Net G\u00f6\u00e7", val = fmt_sayi(row$mig))
            ), function(m) tags$div(
              style = "display:flex;justify-content:space-between;padding:4px 0;font-size:12px;border-bottom:1px dashed #f1f5f9;",
              tags$span(style = "color:#64748b;", m$label),
              tags$span(style = "font-weight:700;color:#1a1a2e;", m$val)
            ))
          )
        )
      )
    })
    tags$div(style = "display:flex;gap:12px;flex-wrap:wrap;margin-bottom:16px;", kartlar)
  })

  # Karsilastirma — zaman serisi metrik tanimlari (4 ayri grafik icin paylasilir)
  .kars_zaman_metrikler <- list(
    pop = list(dt = pop_dt,  col = "pop",        name = "N\u00fcfus (Milyon)",      olcek = 1e6, deger_fmt = "%{y:.2f}"),
    tfr = list(dt = tfr_dt,  col = "tfr",        name = "TFR",                       olcek = 1,   deger_fmt = "%{y:.2f}"),
    e0  = list(dt = e0_dt,   col = "e0B",        name = "Ya\u015fam S\u00fcresi (y\u0131l)", olcek = 1, deger_fmt = "%{y:.1f}"),
    gr  = list(dt = misc_dt, col = "growthrate", name = "B\u00fcy\u00fcme (%)",         olcek = 1,   deger_fmt = "%{y:.2f}")
  )

  kars_zaman_ciz <- function(m_key) {
    ccs <- kars_ccs()
    if (length(ccs) == 0) return(bos_grafik("En az 1 \u00fclke se\u00e7in", "\U0001F30D"))
    m <- .kars_zaman_metrikler[[m_key]]
    p <- suppressWarnings(plot_ly())
    for (i in seq_along(ccs)) {
      cc <- ccs[i]
      ulke_adi <- ulke_ref[country_code == cc, name_tr]
      if (length(ulke_adi) == 0) next
      veri <- m$dt[country_code == cc, .(year, deger = get(m$col) / m$olcek)]
      if (nrow(veri) == 0) next
      p <- p %>% add_trace(data = veri, x = ~year, y = ~deger, type = "scatter", mode = "lines",
                           line = list(color = kars_renkler[((i - 1) %% 4) + 1], width = 3),
                           name = ulke_adi,
                           hovertemplate = paste0("<b>", ulke_adi, "</b><br>",
                                                   "Y\u0131l: %{x}<br>",
                                                   m$name, ": ", m$deger_fmt, "<extra></extra>"))
    }
    p %>% layout(
      yaxis = list(title = list(text = m$name, font = list(size = 13, family = "Plus Jakarta Sans"),
                                  standoff = 15),
                    fixedrange = FALSE, tickfont = list(size = 11),
                    gridcolor = "#f1f5f9", zerolinecolor = "#e2e8f0", automargin = TRUE),
      xaxis = list(title = list(text = "Y\u0131l", font = list(size = 12)),
                    range = c(1950, 2100), dtick = 25,
                    tickfont = list(size = 11), gridcolor = "#f1f5f9"),
      dragmode = "pan",
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.18, x = 0.5, xanchor = "center",
                     font = list(size = 12, family = "Plus Jakarta Sans")),
      margin = list(l = 75, r = 25, t = 10, b = 80),
      plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff"
    ) %>% config(displayModeBar = FALSE, scrollZoom = FALSE)
  }

  output$kars_zaman_pop <- renderPlotly({ kars_zaman_ciz("pop") }) |> bindCache(input$kars_ulkeler)
  output$kars_zaman_tfr <- renderPlotly({ kars_zaman_ciz("tfr") }) |> bindCache(input$kars_ulkeler)
  output$kars_zaman_e0  <- renderPlotly({ kars_zaman_ciz("e0")  }) |> bindCache(input$kars_ulkeler)
  output$kars_zaman_gr  <- renderPlotly({ kars_zaman_ciz("gr")  }) |> bindCache(input$kars_ulkeler)

  output$kars_piramitler <- renderUI({
    ccs <- kars_ccs()
    yil <- kars_yil_d()
    if (length(ccs) == 0) return(tags$div(style = "padding:24px;text-align:center;color:#64748b;",
                                          "\u00dclke se\u00e7in"))
    genislik <- paste0(round(100 / length(ccs), 1), "%")
    tags$div(style = "display:flex;gap:8px;flex-wrap:wrap;",
      lapply(seq_along(ccs), function(i) {
        cc <- ccs[i]
        ulke_adi <- ulke_ref[country_code == cc, name_tr]
        plotid <- paste0("kars_pir_", i)
        tags$div(style = paste0("flex:1;min-width:240px;"),
          tags$div(style = paste0("font-weight:700;color:", kars_renkler[((i-1)%%4)+1], ";font-size:13px;margin-bottom:4px;"),
                   paste0(ulke_adi, " (", yil, ")")),
          plotlyOutput(plotid, height = "320px")
        )
      })
    )
  })

  observe({
    ccs <- kars_ccs()
    yil <- kars_yil_d()
    for (i in seq_along(ccs)) {
      local({
        idx <- i; cc_local <- ccs[idx]
        output[[paste0("kars_pir_", idx)]] <- renderPlotly(piramit_ciz(cc_local, yil, kompakt = TRUE))
      })
    }
  })

  # ============================================================
  # CINEMA 3D RENDER (rayshader - async via R script)
  # ============================================================
  if (!dir.exists("renders")) dir.create("renders")
  shiny::addResourcePath("renders", normalizePath("renders", mustWork = TRUE))
  d3_cinema_sonuc <- reactiveVal(list(path = NULL, msg = "", rendering = FALSE))
  d3_cinema_trigger <- reactiveVal(0)  # yeniden render sonras\u0131 UI g\u00fcncellemek i\u00e7in

  render_cinema_path <- function(metrik, yil) {
    file.path("renders", sprintf("3d_%s_%d.png", metrik, yil))
  }

  output$d3_cinema_status_text <- renderText({
    d3_cinema_sonuc()$msg
  })

  output$d3_cinema_render_view <- renderUI({
    d3_cinema_trigger()  # yeniden render tetiklenirse bu UI de yenilensin
    metrik <- input$d3_metrik; yil <- input$d3_yil
    if (is.null(metrik) || is.null(yil)) return(NULL)
    path <- render_cinema_path(metrik, yil)

    if (file.exists(path)) {
      url_path <- sprintf("renders/3d_%s_%d.png?t=%d", metrik, yil, as.integer(Sys.time()))
      tags$div(style = "padding:10px;text-align:center;",
        tags$img(src = url_path,
                 style = "max-width:100%;max-height:65vh;border-radius:12px;box-shadow:0 6px 30px rgba(0,0,0,0.15);"),
        tags$p(style = "font-size:11px;color:#64748b;margin-top:8px;",
               paste0("rayshader ile render edildi \u2022 ", basename(path)))
      )
    } else {
      tags$div(style = "padding:50px;text-align:center;color:#64748b;",
        tags$div(style = "font-size:48px;margin-bottom:10px;", HTML("&#x1F3AC;")),
        tags$h5(style = "color:#6366f1;", "Sinema render yok"),
        tags$p("Soldaki ", tags$b("Sinema Kalitesinde Render"),
               " butonuna t\u0131klay\u0131n. \u0130\u015flem ~30-60 saniye s\u00fcrer.")
      )
    }
  })

  observeEvent(input$d3_cinema_render, {
    metrik <- input$d3_metrik; yil <- input$d3_yil
    if (is.null(metrik) || is.null(yil)) return()
    path <- render_cinema_path(metrik, yil)
    if (file.exists(path)) file.remove(path)

    d3_cinema_sonuc(list(path = NULL, msg = "Render ediliyor...", rendering = TRUE))

    showNotification("\U0001F3AC Sinema render ba\u015flad\u0131 (30-60 sn)...",
                     type = "message", duration = 5, id = "d3_cinema_notif")

    # Rscript arka planda
    r_exec <- normalizePath(file.path(R.home(), "bin", "Rscript.exe"), mustWork = FALSE)
    if (!file.exists(r_exec)) r_exec <- "Rscript"
    cmd <- sprintf('"%s" render_cinema.R %s %d', r_exec, metrik, yil)
    tryCatch({
      system(cmd, wait = TRUE, intern = FALSE)
      if (file.exists(path)) {
        d3_cinema_sonuc(list(path = path, msg = "\u2713 Render tamamland\u0131!", rendering = FALSE))
        d3_cinema_trigger(d3_cinema_trigger() + 1)
        showNotification("\u2713 Sinema render haz\u0131r!", type = "default", duration = 4, id = "d3_cinema_notif")
      } else {
        d3_cinema_sonuc(list(path = NULL, msg = "\u2717 Render ba\u015far\u0131s\u0131z (konsolu kontrol edin)", rendering = FALSE))
        showNotification("Render ba\u015far\u0131s\u0131z oldu. R konsolu logunu kontrol edin.",
                         type = "error", duration = 8)
      }
    }, error = function(e) {
      d3_cinema_sonuc(list(path = NULL, msg = paste0("Hata: ", conditionMessage(e)), rendering = FALSE))
    })
  })

  # 3D Dot Density sekmesi ve rayshader render altyapisi kaldirildi
  # (mapgl tabanli "3D Harita" sekmesi bunun yerine kullaniliyor)

  # ============================================================
  # Eski 3D Kabartma yan tablosu (kullan\u0131lm\u0131yor ama output tan\u0131m\u0131 lazim olursa diye)
  # ============================================================
  output$d3_ulke_tablo <- DT::renderDT({
    metrik <- input$d3_metrik; yil <- input$d3_yil
    if (is.null(metrik) || is.null(yil)) return(NULL)

    dt <- ulke_veri_yil(yil)
    dt <- merge(dt, ulke_ref, by = "country_code", all.x = TRUE)
    dt$yog <- dt$pop / dt$Alan_km2

    etiket <- switch(metrik,
      yog = "Yo\u011funluk (ki\u015fi/km\u00b2)",
      pop = "N\u00fcfus (Milyon)",
      tfr = "TFR",
      e0  = "Ya\u015fam S\u00fcresi")

    dt$deger <- switch(metrik,
      yog = round(dt$yog, 1),
      pop = round(dt$pop / 1e6, 2),
      tfr = round(dt$tfr, 2),
      e0  = round(dt$e0B, 1))

    df <- dt[!is.na(deger), .(Ulke = name_tr, Kita = continent, Deger = deger)]
    setnames(df, c("Ulke", "Kita", "Deger"), c("\u00dclke", "K\u0131ta", etiket))
    df <- df[order(-df[[etiket]])]

    DT::datatable(
      df, rownames = FALSE, selection = "single",
      options = list(pageLength = 15, scrollX = FALSE,
                     searchHighlight = TRUE,
                     language = list(
                       search = "Ara:",
                       lengthMenu = "Sayfa ba\u015f\u0131: _MENU_",
                       info = "_START_-_END_ / _TOTAL_",
                       paginate = list(previous = "\u00d6nceki", "next" = "Sonraki")
                     )),
      class = "compact stripe hover"
    ) %>%
      DT::formatStyle(etiket,
        background = DT::styleColorBar(range(df[[etiket]], na.rm = TRUE), "#6366f115"),
        backgroundSize = "95% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center",
        fontWeight = "600", color = "#4338ca"
      )
  })

  if (FALSE) {  # echarts globe devre di\u015f\u0131 - sadece rayshader kullan\u0131l\u0131yor

    # echarts dunya harita ismi eslemesi (Natural Earth -> echarts world map)
    ne_to_echarts <- c(
      "United States of America" = "United States",
      "Russia" = "Russia", "T\u00fcrkiye" = "Turkey", "Turkey" = "Turkey",
      "Czechia" = "Czech Rep.", "Bosnia and Herz." = "Bosnia and Herz.",
      "Dem. Rep. Congo" = "Dem. Rep. Congo", "Congo" = "Congo",
      "S. Sudan" = "S. Sudan", "South Sudan" = "S. Sudan",
      "Central African Rep." = "Central African Rep.",
      "Dominican Rep." = "Dominican Rep.", "Eq. Guinea" = "Eq. Guinea",
      "Western Sahara" = "W. Sahara", "W. Sahara" = "W. Sahara",
      "North Korea" = "Dem. Rep. Korea", "Dem. Rep. Korea" = "Dem. Rep. Korea",
      "South Korea" = "Korea", "Republic of Korea" = "Korea",
      "Iran" = "Iran", "Laos" = "Lao PDR", "Syria" = "Syria",
      "Viet Nam" = "Vietnam", "Myanmar" = "Myanmar",
      "United Kingdom" = "United Kingdom"
    )

    output$d3_globe <- echarts4r::renderEcharts4r({
      tryCatch({
        yil <- input$d3_yil
        metrik <- input$d3_metrik
        if (is.null(yil) || is.null(metrik)) return(NULL)

        dt <- ulke_veri_yil(yil)
        dt <- merge(dt, ulke_ref, by = "country_code", all.x = TRUE)
        dt$yog <- dt$pop / dt$Alan_km2

        dt$value <- switch(metrik,
          pop = dt$pop / 1e6,
          yog = dt$yog,
          tfr = dt$tfr,
          e0  = dt$e0B,
          dt$pop / 1e6)

        # Echarts world map icin isim eslemesi
        dt$echarts_name <- ifelse(
          dt$name %in% names(ne_to_echarts),
          ne_to_echarts[dt$name],
          dt$name
        )

        df <- dt[!is.na(dt$value) & !is.na(dt$echarts_name) & dt$value > 0,
                 c("echarts_name", "value", "name_tr")]
        df <- df[!duplicated(df$echarts_name), ]
        if (nrow(df) == 0) return(NULL)

        birim <- switch(metrik,
          pop = " M ki\u015fi", yog = " ki\u015fi/km\u00b2",
          tfr = "", e0 = " y\u0131l", "")
        baslik <- switch(metrik,
          pop = "N\u00fcfus (Milyon)", yog = "Yo\u011funluk (ki\u015fi/km\u00b2)",
          tfr = "TFR", e0 = "Ya\u015fam S\u00fcresi (y\u0131l)", "De\u011fer")

        is_dark <- isTRUE(input$d3_dark)
        bg <- if (is_dark) "#0f172a" else "#f1f5f9"
        text_renk <- if (is_dark) "#ffffff" else "#0f172a"
        kenar_renk <- if (is_dark) "#334155" else "#cbd5e1"
        v <- d3_view()

        df %>%
          echarts4r::e_charts(echarts_name) %>%
          echarts4r::e_map_3d(
            value,
            map = "world",
            shading = "lambert",
            environment = bg,
            light = list(
              main = list(intensity = 1.4, alpha = 50, beta = 30, shadow = FALSE),
              ambient = list(intensity = 0.6)
            ),
            viewControl = list(
              autoRotate = isTRUE(input$d3_rotate),
              autoRotateSpeed = 6,
              distance = v$distance,
              alpha = v$alpha, beta = v$beta,
              minAlpha = 0, maxAlpha = 90,
              minDistance = 40, maxDistance = 400,
              panSensitivity = 1.2
            ),
            boxDepth = 120,
            boxHeight = if (is.null(input$d3_yukseklik)) 80 else input$d3_yukseklik,
            itemStyle = list(borderWidth = 0.3, borderColor = kenar_renk, opacity = 1),
            label = list(show = FALSE),
            emphasis = list(
              label = list(show = TRUE,
                           textStyle = list(color = text_renk, fontSize = 14, fontWeight = "bold",
                                            backgroundColor = if (is_dark) "rgba(15,23,42,0.85)" else "rgba(255,255,255,0.9)",
                                            padding = c(4, 8), borderRadius = 6)),
              itemStyle = list(color = "#fbbf24")
            )
          ) %>%
          echarts4r::e_visual_map(
            value,
            type = "continuous",
            calculable = TRUE,
            inRange = list(color = c("#dbeafe", "#6366f1", "#8b5cf6", "#ec4899", "#f59e0b", "#ef4444")),
            textStyle = list(color = text_renk),
            text = c(paste0("Y\u00fcksek", birim), paste0("D\u00fc\u015f\u00fck", birim)),
            bottom = 30, left = 20, itemWidth = 14, itemHeight = 140
          ) %>%
          echarts4r::e_tooltip(
            backgroundColor = if (is_dark) "rgba(15,23,42,0.95)" else "rgba(255,255,255,0.98)",
            borderColor = "#6366f1", borderWidth = 1,
            textStyle = list(color = text_renk, fontFamily = "Plus Jakarta Sans", fontSize = 13),
            formatter = htmlwidgets::JS(sprintf(
              "function(p){ var v = (p && p.value != null) ? p.value : 0; return '<b style=\"font-size:15px;\">' + (p.name || '') + '</b><br/><span style=\"opacity:0.7;font-size:11px;\">%s</span><br/><b style=\"font-size:16px;color:#6366f1;\">' + Number(v).toFixed(2) + '%s</b>'; }",
              baslik, birim
            ))
          ) %>%
          echarts4r::e_title(
            text = paste0(baslik, " \u2014 ", yil),
            subtext = "BM WPP 2024 \u2022 Fareyle s\u00fcr\u00fckle / tekerlek ile yak\u0131nla\u015ft\u0131r",
            textStyle = list(color = text_renk, fontFamily = "Outfit", fontSize = 18, fontWeight = "bold"),
            subtextStyle = list(color = if (is_dark) "#94a3b8" else "#64748b", fontSize = 11),
            left = "center", top = 10
          )
      }, error = function(e) {
        message("3D map hata: ", conditionMessage(e))
        NULL
      })
    })
  }

  # ============================================================
  # SEKME 6: YOGUNLUK-BUYUME SACILIM
  # ============================================================
  output$scatter_grafik <- renderPlotly({
    yil <- input$scatter_yil
    if (is.null(yil)) return(bos_grafik("Y\u0131l se\u00e7in", "\U0001F4C5"))

    dt <- ulke_veri_yil(yil)
    dt <- merge(dt, ulke_ref, by = "country_code", all.x = TRUE)
    dt[, yogunluk := ifelse(!is.na(Alan_km2) & Alan_km2 > 0, pop / Alan_km2, NA_real_)]
    dt <- dt[!is.na(yogunluk) & yogunluk > 0 & !is.na(growthrate) &
             !is.na(continent) & continent != "Antarctica" & continent != "Seven seas (open ocean)"]
    dt$kita_tr <- kita_tr[dt$continent]

    plot_ly(dt, x = ~yogunluk, y = ~growthrate, color = ~kita_tr,
            size = ~pop, sizes = c(5, 50),
            type = "scatter", mode = "markers",
            marker = list(opacity = 0.7, line = list(width = 0.5, color = "#333")),
            text = ~paste0("<b>", name_tr, "</b><br>",
                          "Yo\u011funluk: ", round(yogunluk, 1), " ki\u015fi/km\u00b2<br>",
                          "B\u00fcy\u00fcme: ", round(growthrate, 2), "%<br>",
                          "N\u00fcfus: ", fmt_sayi(pop)),
            hoverinfo = "text",
            colors = viridis(length(unique(dt$kita_tr)), option = "turbo")) %>%
      layout(
        dragmode = "pan",
        xaxis = list(title = "N\u00fcfus Yo\u011funlu\u011fu (ki\u015fi/km\u00b2 - log \u00f6l\u00e7ek)", type = "log", fixedrange = TRUE),
        yaxis = list(title = "Y\u0131ll\u0131k De\u011fi\u015fim (%)", fixedrange = TRUE),
        shapes = list(
          list(type = "line", x0 = 0.01, x1 = 100000, y0 = 0, y1 = 0,
               line = list(color = "red", width = 1, dash = "dash"))
        )
      ) %>% config(displayModeBar = FALSE, scrollZoom = FALSE)
  }) |> bindCache(input$scatter_yil)
  # ============================================================
  # SEKME 7: VERI TABLOSU
  # ============================================================
  tablo_veri <- reactive({
    yil <- input$tablo_yil
    if (is.null(yil)) return(data.table())
    dt <- ulke_veri_yil(yil)
    dt <- merge(dt, ulke_ref, by = "country_code", all.x = TRUE)
    dt[, yogunluk := ifelse(!is.na(Alan_km2) & Alan_km2 > 0, pop / Alan_km2, NA_real_)]
    dt[, kita_tr := kita_tr[continent]]

    # Yas verisi
    age_yil <- age_dt[year == yil & country_code %in% dt$country_code]
    if (nrow(age_yil) > 0) {
      age_ozet <- age_yil[, {
        toplam <- sum(pop, na.rm = TRUE)
        genc <- sum(pop[age < 15], na.rm = TRUE)
        yasli <- sum(pop[age >= 65], na.rm = TRUE)
        calisma <- toplam - genc - yasli
        bag_orani <- if (calisma > 0) round((genc + yasli) / calisma * 100, 1) else NA_real_
        list(bag_orani = bag_orani)
      }, by = country_code]
      dt <- merge(dt, age_ozet, by = "country_code", all.x = TRUE)
    } else {
      dt[, bag_orani := NA_real_]
    }

    # Demografik gecis asamasi — vektorize (onceden FOR loop'tu)
    t_val <- dt$tfr; e_val <- dt$e0B; g_val <- dt$growthrate
    asama <- rep(NA_character_, nrow(dt))
    gecerli <- !is.na(t_val) & !is.na(e_val)
    # Her asamaya vectorize kosul (once secildi = oncelik sirasi)
    a5 <- gecerli & t_val < 2 & e_val >= 70 & !is.na(g_val) & g_val < 0
    a4 <- gecerli & is.na(asama) & t_val < 2.1 & e_val >= 70 & !a5
    a3 <- gecerli & !a5 & !a4 & t_val >= 2.1 & t_val < 3.5 & e_val >= 60
    a2 <- gecerli & !a5 & !a4 & !a3 & t_val >= 3.5 & e_val >= 50
    a1 <- gecerli & !a5 & !a4 & !a3 & !a2 & t_val >= 3.5 & e_val < 50
    a2b <- gecerli & !a5 & !a4 & !a3 & !a2 & !a1 & t_val < 3.5 & e_val < 60
    a3b <- gecerli & !a5 & !a4 & !a3 & !a2 & !a1 & !a2b
    asama[a1]  <- "1 - Y\u00fcksek Do\u011fum/\u00d6l\u00fcm"
    asama[a2 | a2b] <- "2 - D\u00fc\u015fen \u00d6l\u00fcm"
    asama[a3 | a3b] <- "3 - D\u00fc\u015fen Do\u011fum"
    asama[a4]  <- "4 - D\u00fc\u015f\u00fck Do\u011fum/\u00d6l\u00fcm"
    asama[a5]  <- "5 - N\u00fcfus Azalmas\u0131"
    dt[, gecis_asamasi := asama]

    # Kita filtresi
    kita_sec <- input$tablo_kita
    if (!is.null(kita_sec) && kita_sec != "hepsi") {
      dt <- dt[continent == kita_sec]
    }
    dt
  }) |> bindCache(input$tablo_yil, input$tablo_kita)

  output$tablo_baslik <- renderText({
    yil <- input$tablo_yil
    if (is.null(yil)) return("")
    paste0("\u00dclke Verileri (", yil, ")")
  })

  output$veri_tablosu <- renderDT({
    dt <- tablo_veri()
    if (nrow(dt) == 0) return(datatable(data.frame()))
    # Sade gorunum: 7 anahtar metrik. Niche olanlar (Net Goc, Bagimlilik, Dem. Gecis)
    # CSV indirmede hala mevcut.
    gosterim <- dt[, .(
      "\u00dclke" = name_tr,
      "K\u0131ta" = kita_tr,
      "N\u00fcfus" = pop,
      "Yo\u011funluk" = round(yogunluk, 1),
      "B\u00fcy\u00fcme (%)" = round(growthrate, 2),
      "TFR" = round(tfr, 2),
      "Ya\u015fam S\u00fcresi" = round(e0B, 1)
    )]
    datatable(
      gosterim,
      filter = "top",
      rownames = FALSE,
      class = "compact stripe hover nowrap",
      options = list(
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100),
        scrollX = TRUE,
        order = list(list(2, "desc")),  # Nufus'a gore cokten aza
        columnDefs = list(
          list(className = "dt-left",   targets = c(0, 1)),
          list(className = "dt-right",  targets = c(2, 3, 4, 5, 6))
        ),
        language = list(
          search = "Ara:", lengthMenu = "_MENU_ sat\u0131r g\u00f6ster",
          info = "_TOTAL_ \u00fclkeden _START_\u2013_END_ aras\u0131",
          infoEmpty = "Veri yok",
          infoFiltered = "(_MAX_ \u00fclkeden filtrelendi)",
          paginate = list(previous = "\u00d6nceki", `next` = "Sonraki"),
          zeroRecords = "Sonu\u00e7 bulunamad\u0131"
        )
      )
    ) %>%
      formatRound("N\u00fcfus",         digits = 0, mark = ".") %>%
      formatRound("Yo\u011funluk",     digits = 1, mark = ".", dec.mark = ",") %>%
      formatRound("B\u00fcy\u00fcme (%)", digits = 2, dec.mark = ",") %>%
      formatRound("TFR",                digits = 2, dec.mark = ",") %>%
      formatRound("Ya\u015fam S\u00fcresi", digits = 1, dec.mark = ",")
  })

  output$tablo_indir_csv <- downloadHandler(
    filename = function() paste0("nufus_verileri_", input$tablo_yil, ".csv"),
    content = function(file) {
      dt <- tablo_veri()
      cikti <- dt[, .(Ulke = name_tr, Kita = kita_tr, Nufus = pop,
                       Yogunluk = round(yogunluk, 1), Buyume_Pct = round(growthrate, 2),
                       TFR = round(tfr, 2), Yasam_Suresi = round(e0B, 1),
                       Net_Goc = mig, Bag_Orani = bag_orani,
                       Demografik_Gecis = gecis_asamasi)]
      fwrite(cikti, file, bom = TRUE)
    }
  )

  # ============================================================
  # EYALET KATMANI (Interaktif Harita)
  # ============================================================
  observe({
    if (is.null(eyalet_sf)) return()
    goster <- input$eyalet_goster
    if (is.null(goster)) return()
    proxy <- leafletProxy("ana_harita")
    if (goster) {
      # Eyalet yogunlugu icin renk paleti
      yog_vals <- eyalet_sf$Yogunluk[!is.na(eyalet_sf$Yogunluk) & eyalet_sf$Yogunluk > 0]
      if (length(yog_vals) == 0) return()
      pal <- colorNumeric(viridis(256, option = "inferno"),
                          domain = log10(pmax(range(yog_vals), 1)),
                          na.color = "transparent")
      fill_renk <- pal(log10(pmax(eyalet_sf$Yogunluk, 1)))
      fill_renk[is.na(fill_renk)] <- "transparent"
      eyalet_etiketler <- lapply(seq_len(nrow(eyalet_sf)), function(i) {
        htmltools::HTML(sprintf("<b>%s</b> (%s)<br/>N\u00fcfus: %s<br/>Yo\u011funluk: %s ki\u015fi/km\u00b2",
                                html_safe(eyalet_sf$name[i]),
                                html_safe(eyalet_sf$admin[i]),
                                fmt_sayi(eyalet_sf$Nufus[i]),
                                ifelse(is.na(eyalet_sf$Yogunluk[i]), "-", round(eyalet_sf$Yogunluk[i], 1))))
      })
      proxy %>%
        addPolygons(data = eyalet_sf, group = "eyaletler",
                    fillColor = fill_renk, weight = 0.5, color = "#333", opacity = 0.5, fillOpacity = 0.6,
                    label = eyalet_etiketler,
                    labelOptions = labelOptions(textsize = "11px"),
                    highlightOptions = highlightOptions(weight = 1.5, color = "#fff", fillOpacity = 0.8))
    } else {
      proxy %>% clearGroup("eyaletler")
    }
  })

  # ============================================================
  # TEMATIK: DEMOGRAFIK GECIS HARITASI
  # ============================================================
  tema_hazirla_gecis <- function(yil) {
    sf_data <- ulke_sf
    dt <- ulke_veri_yil(yil)
    idx <- match(sf_data$country_code, dt$country_code)
    tfr_val <- dt$tfr[idx]
    e0_val <- dt$e0B[idx]
    grow_val <- dt$growthrate[idx]

    asama <- rep(NA_character_, nrow(sf_data))
    for (i in seq_len(nrow(sf_data))) {
      t_v <- tfr_val[i]; e_v <- e0_val[i]; g_v <- grow_val[i]
      if (is.na(t_v) || is.na(e_v)) next
      if (t_v > 5 && e_v < 50) asama[i] <- "A\u015fama 1"
      else if (t_v > 3 && e_v >= 50 && e_v < 65) asama[i] <- "A\u015fama 2"
      else if (t_v >= 2 && t_v <= 3 && e_v >= 65) asama[i] <- "A\u015fama 3"
      else if (t_v < 2 && e_v >= 75) {
        if (!is.na(g_v) && g_v < 0) asama[i] <- "A\u015fama 5"
        else asama[i] <- "A\u015fama 4"
      }
      else if (t_v >= 2 && t_v <= 5 && e_v < 50) asama[i] <- "A\u015fama 1"
      else if (t_v >= 2 && t_v <= 3) asama[i] <- "A\u015fama 3"
      else if (t_v < 2) asama[i] <- "A\u015fama 4"
      else asama[i] <- "A\u015fama 2"
    }
    sf_data$asama <- asama
    seviyeler <- c("A\u015fama 1", "A\u015fama 2", "A\u015fama 3", "A\u015fama 4", "A\u015fama 5")
    renkler <- c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#1a9850")
    sf_data$asama_f <- factor(asama, levels = seviyeler)
    pal <- colorFactor(renkler, levels = seviyeler, na.color = "#e0e0e0")
    fill_renk <- pal(sf_data$asama_f)
    fill_renk[is.na(fill_renk)] <- "#e0e0e0"
    kita_tr_vec <- kita_tr[sf_data$continent]
    etiketler <- lapply(seq_len(nrow(sf_data)), function(i) {
      alt_bilgi <- sprintf("TFR: %s \u2022 Ya\u015fam: %s",
                           ifelse(is.na(tfr_val[i]), "-", round(tfr_val[i], 2)),
                           ifelse(is.na(e0_val[i]), "-", round(e0_val[i], 1)))
      htmltools::HTML(sprintf(
        "<div class='ulke-tooltip-baslik'>%s</div>%s<div class='ulke-tooltip-deger'><div class='deger'>%s</div><div class='deger-label'>Ge\u00e7i\u015f A\u015famas\u0131</div></div><div style='margin-top:6px;font-size:10px;color:rgba(255,255,255,0.55);letter-spacing:0.3px;'>%s</div>",
        html_safe(sf_data$name_tr[i]),
        if (is.na(kita_tr_vec[i])) "" else sprintf("<div class='ulke-tooltip-kita'>&#127757; %s</div>", html_safe(kita_tr_vec[i])),
        ifelse(is.na(asama[i]), "Veri yok", html_safe(asama[i])),
        alt_bilgi
      ))
    })
    list(sf_data = sf_data, fill_renk = fill_renk, pal = pal, etiketler = etiketler, seviyeler = seviyeler)
  }

  output$tema_gecis_harita <- renderLeaflet({
    r <- tema_hazirla_gecis(2026)
    leaflet(options = leafletOptions(minZoom = 2, maxZoom = 8, worldCopyJump = FALSE,
                                   maxBounds = list(list(-85, -180), list(85, 180)),
                                   maxBoundsViscosity = 1.0)) %>%
      addMapPane("zemin_pane", zIndex = 200) %>%
      addMapPane("polygon_pane", zIndex = 400) %>%
      addMapPane("tr_label_pane", zIndex = 460) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       options = providerTileOptions(pane = "zemin_pane", noWrap = TRUE)) %>%
      setView(lng = 30, lat = 25, zoom = 2) %>%
      addPolygons(data = r$sf_data, group = "veri",
                  fillColor = r$fill_renk,
                  weight = 0.8, color = "#334155", opacity = 0.55, fillOpacity = 1,
                  options = pathOptions(pane = "polygon_pane"),
                  label = r$etiketler, labelOptions = labelOptions(className = "ulke-tooltip-wrap", textsize = "12px"),
                  highlightOptions = highlightOptions(weight = 2, color = "#fff", fillOpacity = 1, bringToFront = FALSE)) %>%
      ekle_ulke_etiketleri() %>%
      addLegend(layerId = "legend", position = "bottomright", pal = r$pal,
                values = factor(r$seviyeler, levels = r$seviyeler),
                title = "Demografik Ge\u00e7i\u015f", opacity = 0.85) %>%
      addControl(html = "<div class='yil-gosterge'><div class='yil-etiket'>YIL</div><div class='yil-deger'>2026</div></div>",
                 position = "topright", layerId = "yil_kontrol",
                 className = "yil-kontrol-wrap")
  })
  observe({
    yil <- input$tema_yil
    if (is.null(yil)) return()
    if (!identical(input$tema_sub_nav, "Demografik Ge\u00e7i\u015f")) return()
    r <- tema_hazirla_gecis(yil)
    leafletProxy("tema_gecis_harita") %>%
      clearGroup("veri") %>% clearGroup("tr_isimler_buyuk") %>% clearGroup("tr_isimler_orta") %>% clearGroup("tr_isimler_kucuk") %>%
      removeControl("legend") %>%
      addPolygons(data = r$sf_data, group = "veri",
                  fillColor = r$fill_renk,
                  weight = 0.8, color = "#334155", opacity = 0.55, fillOpacity = 1,
                  options = pathOptions(pane = "polygon_pane"),
                  label = r$etiketler, labelOptions = labelOptions(className = "ulke-tooltip-wrap", textsize = "12px"),
                  highlightOptions = highlightOptions(weight = 2, color = "#fff", fillOpacity = 1, bringToFront = FALSE)) %>%
      ekle_ulke_etiketleri() %>%
      addLegend(layerId = "legend", position = "bottomright", pal = r$pal,
                values = factor(r$seviyeler, levels = r$seviyeler),
                title = "Demografik Ge\u00e7i\u015f", opacity = 0.85)
    harita_yil_rozeti("tema_gecis_harita", yil)
  })

  # ============================================================
  # TEMATIK: BAGIMSIZLIK ORANI HARITASI
  # ============================================================
  tema_hazirla_bag <- function(yil) {
    sf_data <- ulke_sf
    age_yil <- age_dt[year == yil]
    if (nrow(age_yil) == 0) return(NULL)
    age_ozet <- age_yil[, {
      toplam <- sum(pop, na.rm = TRUE)
      genc <- sum(pop[age < 15], na.rm = TRUE)
      yasli <- sum(pop[age >= 65], na.rm = TRUE)
      calisma <- toplam - genc - yasli
      bag <- if (calisma > 0) round((genc + yasli) / calisma * 100, 1) else NA_real_
      list(bag_orani = bag)
    }, by = country_code]
    idx <- match(sf_data$country_code, age_ozet$country_code)
    sf_data$deger <- age_ozet$bag_orani[idx]
    degerler <- sf_data$deger[!is.na(sf_data$deger)]
    if (length(degerler) == 0) return(NULL)
    pal <- colorNumeric(c("#2166ac", "#f7f7f7", "#b2182b"),
                        domain = c(20, 120), na.color = "#e0e0e0")
    fill_renk <- pal(pmax(pmin(sf_data$deger, 120), 20))
    fill_renk[is.na(fill_renk)] <- "#e0e0e0"
    kita_tr_vec <- kita_tr[sf_data$continent]
    etiketler <- lapply(seq_len(nrow(sf_data)), function(i) {
      ulke_tooltip_html(sf_data$name_tr[i], kita_tr_vec[i],
                         ifelse(is.na(sf_data$deger[i]), "-", paste0(sf_data$deger[i], "%")),
                         "Ba\u011f\u0131ml\u0131l\u0131k Oran\u0131")
    })
    list(sf_data = sf_data, fill_renk = fill_renk, pal = pal, degerler = degerler, etiketler = etiketler)
  }

  output$tema_bag_harita <- renderLeaflet({
    r <- tema_hazirla_bag(2026)
    if (is.null(r)) return(leaflet() %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>% setView(30, 25, 2))
    leaflet(options = leafletOptions(minZoom = 2, maxZoom = 8, worldCopyJump = FALSE,
                                   maxBounds = list(list(-85, -180), list(85, 180)),
                                   maxBoundsViscosity = 1.0)) %>%
      addMapPane("zemin_pane", zIndex = 200) %>%
      addMapPane("polygon_pane", zIndex = 400) %>%
      addMapPane("tr_label_pane", zIndex = 460) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       options = providerTileOptions(pane = "zemin_pane", noWrap = TRUE)) %>%
      setView(lng = 30, lat = 25, zoom = 2) %>%
      addPolygons(data = r$sf_data, group = "veri",
                  fillColor = r$fill_renk,
                  weight = 0.8, color = "#334155", opacity = 0.55, fillOpacity = 1,
                  options = pathOptions(pane = "polygon_pane"),
                  label = r$etiketler, labelOptions = labelOptions(className = "ulke-tooltip-wrap", textsize = "12px"),
                  highlightOptions = highlightOptions(weight = 2, color = "#fff", fillOpacity = 1, bringToFront = FALSE)) %>%
      ekle_ulke_etiketleri() %>%
      addLegend(layerId = "legend", position = "bottomright", pal = r$pal, values = r$degerler,
                title = "Ba\u011f\u0131ms\u0131zl\u0131k Oran\u0131 (%)", opacity = 0.85) %>%
      addControl(html = "<div class='yil-gosterge'><div class='yil-etiket'>YIL</div><div class='yil-deger'>2026</div></div>",
                 position = "topright", layerId = "yil_kontrol",
                 className = "yil-kontrol-wrap")
  })
  observe({
    yil <- input$tema_yil
    if (is.null(yil)) return()
    if (!identical(input$tema_sub_nav, "Ba\u011f\u0131ms\u0131zl\u0131k Oran\u0131")) return()
    r <- tema_hazirla_bag(yil)
    if (is.null(r)) return()
    leafletProxy("tema_bag_harita") %>%
      clearGroup("veri") %>% clearGroup("tr_isimler_buyuk") %>% clearGroup("tr_isimler_orta") %>% clearGroup("tr_isimler_kucuk") %>%
      removeControl("legend") %>%
      addPolygons(data = r$sf_data, group = "veri",
                  fillColor = r$fill_renk,
                  weight = 0.8, color = "#334155", opacity = 0.55, fillOpacity = 1,
                  options = pathOptions(pane = "polygon_pane"),
                  label = r$etiketler, labelOptions = labelOptions(className = "ulke-tooltip-wrap", textsize = "12px"),
                  highlightOptions = highlightOptions(weight = 2, color = "#fff", fillOpacity = 1, bringToFront = FALSE)) %>%
      ekle_ulke_etiketleri() %>%
      addLegend(layerId = "legend", position = "bottomright", pal = r$pal, values = r$degerler,
                title = "Ba\u011f\u0131ms\u0131zl\u0131k Oran\u0131 (%)", opacity = 0.85)
    harita_yil_rozeti("tema_bag_harita", yil)
  })

  # ----------------------------------------------------------
  # 3D EXTRUDED HARITA (mapgl / MapLibre GL)
  # ----------------------------------------------------------
  output$map3d <- mapgl::renderMaplibre({
    req(.has_mapgl)
    yil <- input$globe_yil %||% 2026
    met_h <- input$globe_yukseklik %||% "pop"
    met_r <- input$globe_renk %||% "yogunluk"
    tema <- input$globe_tema %||% "dark"
    pitch_val <- input$globe_pitch %||% 55

    # Tum metrikleri birden topla
    pop_y <- pop_dt[year == yil, .(country_code, pop)]
    gr_y  <- misc_dt[year == yil, .(country_code, growthrate, births, deaths)]
    tfr_y <- tfr_dt[year == yil, .(country_code, tfr)]
    e0_y  <- e0_dt[year == yil,  .(country_code, e0B)]
    mig_y <- mig_dt[year == yil, .(country_code, mig)]
    ref_y <- as.data.table(ulke_ref)[, .(country_code, Alan_km2, continent)]
    veri <- Reduce(function(a, b) merge(a, b, by = "country_code", all.x = TRUE),
                    list(pop_y, gr_y, tfr_y, e0_y, mig_y, ref_y))
    veri[, yogunluk := ifelse(!is.na(Alan_km2) & Alan_km2 > 0, pop / Alan_km2, NA_real_)]

    # sf ile birlestir
    sf_data <- merge(ulke_sf[, c("country_code", "name_tr")],
                      as.data.frame(veri), by = "country_code", all.x = TRUE)
    sf_data <- sf_data[!is.na(sf_data$pop), ]
    if (nrow(sf_data) == 0) return(NULL)

    # Yukseklik hesabi (metrige gore)
    hesapla_deger <- function(met, d) {
      switch(met,
        "pop"      = d$pop,
        "yogunluk" = d$yogunluk,
        "growth"   = d$growthrate,
        d$pop)
    }
    deger_h <- hesapla_deger(met_h, sf_data)
    deger_r <- hesapla_deger(met_r, sf_data)

    # Yukseklik: log olcek, maksimumu 250km ile sinirla (arkadaki ulkeleri ezmesin)
    if (met_h == "growth") {
      # Buyume orani icin dogrusal ama offsetli
      h_raw <- pmax(deger_h + 3, 0.1)
    } else {
      # Nufus ve yogunluk icin log
      h_raw <- log10(pmax(deger_h, 1) + 1)
    }
    h_max <- max(h_raw, na.rm = TRUE)
    if (!is.finite(h_max) || h_max <= 0) h_max <- 1
    sf_data$yukseklik <- (h_raw / h_max) * 250000  # 0 - 250 km range
    sf_data$yukseklik[is.na(sf_data$yukseklik)] <- 0

    # Renk deger sutununu ekle
    sf_data$renk_deger <- deger_r
    sf_data$renk_deger[is.na(sf_data$renk_deger)] <- 0

    # Renk paleti (renklendirme metrigine gore)
    renk_duraklari <- if (met_r == "growth") {
      list(
        c(-5, "#7f1d1d"), c(-2, "#dc2626"), c(0, "#fbbf24"),
        c(2, "#22c55e"), c(5, "#065f46")
      )
    } else if (met_r == "yogunluk") {
      list(
        c(0, "#1e1b4b"), c(25, "#4c1d95"), c(100, "#be185d"),
        c(500, "#f59e0b"), c(2000, "#fef08a")
      )
    } else {
      list(
        c(0, "#1e1b4b"), c(5e6, "#312e81"), c(5e7, "#7c3aed"),
        c(2e8, "#ec4899"), c(1.4e9, "#fef08a")
      )
    }

    # Duraklari alternatifli liste olarak ekle (sayi-string tiplerini koru)
    stop_list <- list()
    for (pair in renk_duraklari) {
      stop_list <- c(stop_list, list(as.numeric(pair[1]), as.character(pair[2])))
    }
    color_expr <- c(
      list("interpolate", list("linear"), list("get", "renk_deger")),
      stop_list
    )

    # Zengin HTML tooltip — kartsilastirma kartlari stilinde
    fmt_num <- function(x, d = 0) ifelse(is.na(x), "-",
      format(round(x, d), big.mark = ",", scientific = FALSE, trim = TRUE))
    buyume_val <- ifelse(is.na(sf_data$growthrate), "-",
                          paste0(round(sf_data$growthrate, 2), "%"))
    buyume_col <- ifelse(is.na(sf_data$growthrate), "#64748b",
                          ifelse(sf_data$growthrate >= 0, "#16a34a", "#dc2626"))
    kita_str <- ifelse(is.na(sf_data$continent), "", paste0(sf_data$continent, " \u2022 "))

    sat_css <- "style='display:flex;justify-content:space-between;padding:5px 0;font-size:12px;border-bottom:1px dashed #f1f5f9;'"
    lab_css <- "style='color:#64748b;'"
    val_css <- "style='font-weight:700;color:#1a1a2e;'"

    sf_data$tooltip_html <- paste0(
      "<div style='font-family:Plus Jakarta Sans,Inter,sans-serif;min-width:230px;background:#ffffff;border-radius:14px;overflow:hidden;box-shadow:0 4px 18px rgba(0,0,0,0.12);border-top:4px solid #6366f1;margin:-10px -10px -11px -10px;'>",
        # Gradient header
        "<div style='padding:12px 16px;background:linear-gradient(135deg,#6366f115,#8b5cf608);'>",
          "<div style='font-size:16px;font-weight:800;color:#4338ca;letter-spacing:-0.2px;'>",
            ifelse(is.na(sf_data$name_tr), "-", sf_data$name_tr),
          "</div>",
          "<div style='font-size:11px;color:#64748b;margin-top:2px;'>",
            kita_str, yil,
          "</div>",
        "</div>",
        # Metrikler
        "<div style='padding:10px 16px 12px 16px;'>",
          "<div ", sat_css, "><span ", lab_css, ">N\u00fcfus</span><span ", val_css, ">",
            fmt_num(sf_data$pop), "</span></div>",
          "<div ", sat_css, "><span ", lab_css, ">Yo\u011funluk</span><span ", val_css, ">",
            fmt_num(sf_data$yogunluk, 1), " ki\u015fi/km\u00b2</span></div>",
          "<div ", sat_css, "><span ", lab_css, ">B\u00fcy\u00fcme</span><span style='font-weight:700;color:", buyume_col, ";'>",
            buyume_val, "</span></div>",
          "<div ", sat_css, "><span ", lab_css, ">TFR</span><span ", val_css, ">",
            fmt_num(sf_data$tfr, 2), "</span></div>",
          "<div ", sat_css, "><span ", lab_css, ">Ya\u015fam S\u00fcresi</span><span ", val_css, ">",
            fmt_num(sf_data$e0B, 1), " y\u0131l</span></div>",
          "<div style='display:flex;justify-content:space-between;padding:5px 0;font-size:12px;'><span ", lab_css, ">Net G\u00f6\u00e7</span><span ", val_css, ">",
            fmt_num(sf_data$mig), "</span></div>",
        "</div>",
      "</div>"
    )

    # Basemap style secimi — etiketleri gizlenmis versiyonunu kullan
    style_map <- get_basemap_no_labels(tema)
    if (is.null(style_map)) {
      style_map <- switch(tema,
        "dark"    = mapgl::carto_style("dark-matter"),
        "light"   = mapgl::carto_style("positron"),
        "voyager" = mapgl::carto_style("voyager"),
        "liberty" = mapgl::openfreemap_style("liberty"),
        mapgl::carto_style("dark-matter")
      )
    }

    # Etiketler icin centroid noktalari olustur (kule yuksekligini koru)
    cent_coords <- sf::st_coordinates(
      sf::st_point_on_surface(sf::st_geometry(sf_data))
    )
    sf_points <- sf::st_as_sf(
      data.frame(
        name_tr   = sf_data$name_tr,
        yukseklik = sf_data$yukseklik,
        pop       = sf_data$pop,
        # MapLibre'de DUSUK sort_key = ONCELIKLI. Nufusun negatifini kullanalim
        oncelik   = -as.numeric(sf_data$pop),
        lng = cent_coords[, 1], lat = cent_coords[, 2],
        stringsAsFactors = FALSE
      ),
      coords = c("lng", "lat"), crs = 4326
    )

    mapgl::maplibre(
      style  = style_map,
      center = c(20, 25), zoom = 1.3,
      pitch  = pitch_val, bearing = 0
    ) |>
    mapgl::add_fill_extrusion_layer(
      id = "pop_3d",
      source = sf_data,
      fill_extrusion_color = color_expr,
      fill_extrusion_height = list("get", "yukseklik"),
      fill_extrusion_base = 0,
      fill_extrusion_opacity = 0.85,
      hover_options = list(
        fill_extrusion_color = "#fbbf24",
        fill_extrusion_opacity = 1
      )
    ) |>
    # Gorunmez duz katman — hover algilamasi icin (kulenin yan yuzunu de yakalar)
    mapgl::add_fill_layer(
      id = "hover_katmani",
      source = sf_data,
      fill_color = "#000000",
      fill_opacity = 0,
      tooltip = "tooltip_html"
    ) |>
    mapgl::add_symbol_layer(
      id = "ulke_isim",
      source = sf_points,
      text_field = list("get", "name_tr"),
      text_size = 13,
      text_color = if (tema == "light") "#0f172a" else "#f8fafc",
      text_halo_color = if (tema == "light") "#ffffff" else "#0f172a",
      text_halo_width = 2,
      text_font = list("Open Sans Regular", "Arial Unicode MS Regular"),
      text_padding = 4,
      symbol_sort_key = list("get", "oncelik"),
      min_zoom = 2
    ) |>
    mapgl::add_legend(
      legend_title = switch(met_r,
        "pop" = "N\u00fcfus",
        "yogunluk" = "Yo\u011funluk (ki\u015fi/km\u00b2)",
        "growth" = "B\u00fcy\u00fcme (%)"
      ),
      values = sapply(renk_duraklari, function(x) x[[1]]),
      colors = sapply(renk_duraklari, function(x) x[[2]]),
      type = "continuous",
      position = "top-right"
    )
  })
}

# ============================================================
# CALISTIR
# ============================================================
shinyApp(ui = ui, server = server)
