# Teknoloji Yığını (Tech Stack)

> Proje: Dünya Nüfus Analizi Paneli
> Tür: R Shiny Dashboard
> Güncel: Nisan 2026

---

## 1. Runtime

| Bileşen | Sürüm | Rol |
|---|---|---|
| **R** | 4.5.2 | Temel dil çalışma zamanı |
| **Shiny Server / RStudio / `shiny::runApp()`** | R paketi üzerinden | Uygulama sunucusu |
| **Platform** | Windows 10/11, Linux (WSL2 üzerinde geliştirilmiş) | OS |
| **Encoding** | UTF-8 (`options(encoding = "UTF-8")`) | Türkçe karakter bütünlüğü |
| **Locale** | `Turkish_Turkey.65001` → `.UTF-8` fallback | Sys.setlocale LC_CTYPE |

---

## 2. Zorunlu R Paketleri (`app.R` runtime)

| Paket | Rol | Notlar |
|---|---|---|
| **shiny** | Web uygulama çatısı | `shinyApp()`, reactive |
| **bslib** | Bootstrap 5 teması, `page_navbar`, `value_box`, `card`, `layout_sidebar`, `input_dark_mode` | Tema: Flatly + özelleştirme |
| **leaflet** | İnteraktif choropleth haritalar | Sekme 2 ve 5 |
| **plotly** | Çizgi grafikleri, yaş piramitleri, scatter | `plot_ly`, `ggplotly` kullanılmıyor |
| **DT** | Sekme 9'da aranabilir tablo | DataTables JS köprüsü |
| **sf** | Vektör coğrafi veri (ülke/eyalet poligonları) | `.rds` olarak önceden kaydedilmiş |
| **data.table** | Hızlı in-memory tablo işlemleri | Tüm metrik tabloları |
| **viridisLite** | Renk skalaları (plasma, turbo, viridis) | Grafiklerde |
| **dplyr** | Boru hattı (`%>%`) ve bazı veri manipülasyonları | Sınırlı kullanım |
| **htmlwidgets** | `onRender` ile özel JS (ülke etiketi zoom filtresi) | Leaflet JS köprüsü |

---

## 3. Opsiyonel R Paketleri (graceful degrade)

`requireNamespace(..., quietly = TRUE)` ile kontrol edilip bayrak değişkenlerine yazılır:

| Paket | Bayrak | Yoksa ne olur |
|---|---|---|
| **shinycssloaders** | `.has_spinner` | Spinner gösterilmez, içerik doğrudan render edilir |
| **waiter** | `.has_waiter` | Shuffle/yükleme ekranı atlanır |
| **leaflet.extras** | `.has_lfxtras` | Ekstra leaflet kontrolleri (arama vb.) atlanır |
| **bsicons** | `.has_bsicons` | FontAwesome icon'a düşer |
| **echarts4r** | `.has_echarts` | Sekme 6 (3D Globe) yerine kurulum uyarısı gösterilir |

---

## 4. Offline Veri Hazırlama (`data_hazirla.R`)

Ek paketler sadece ETL sırasında gereklidir; runtime'da yoktur.

| Paket | Rol |
|---|---|
| **wpp2024** | BM WPP 2024 verisi (pop1dt, popAge1dt, popproj1dt, TFR, e0, göç, misc) |
| **rnaturalearth**, **rnaturalearthdata**, **rnaturalearthhires** | Dünya ülke/eyalet poligonları |
| **rmapshaper** | `ms_simplify()` ile poligon sadeleştirme |
| **geodata** | GADM, WorldPop gibi ek coğrafi dataların cache indirmesi |
| **terra** | Raster işleme |
| **exactextractr** | Raster-poligon zonal istatistik |

---

## 5. Ön-yüz / Web Teknolojileri (tarayıcıda)

| Katman | Kullanım |
|---|---|
| **Bootstrap 5** (bslib üzerinden) | Grid, kart, value box, navbar |
| **Bootswatch Flatly** | Base tema |
| **Custom CSS** (~200 satır, inline) | Gradient navbar, value box, koyu mod, spinner |
| **Google Fonts** | `Plus Jakarta Sans`, `Outfit`, `JetBrains Mono` |
| **FontAwesome** | Shiny `icon()` varsayılanı |
| **bsicons (Bootstrap Icons)** | Varsa öncelikli |
| **Leaflet.js** | Harita, OSM tile'ları |
| **Plotly.js** | Grafikler (istemci tarafı etkileşim) |
| **DataTables** | Tablo (arama, sıralama, sayfalama) |
| **ECharts (via echarts4r)** | 3D küre |
| **Custom JS** | `htmlwidgets::onRender` ile zoom-bağımlı label görünürlük filtresi |

---

## 6. Veri Formatları

| Format | Kullanım | Dosya sayısı |
|---|---|---|
| `.rds` (R serialize) | Shiny'nin tek okuma biçimi — tablolar + sf | 9 |
| `.csv` (UTF-8) | Worldometer snapshot, kullanıcı indirme | 2 girdi + 1 indirme |
| Google Fonts (remote) | Tipografi | 3 aile |

---

## 7. Geliştirme Araçları / Ortam

| Araç | Rol |
|---|---|
| **RStudio** | Birincil IDE |
| **Git** | Sürüm kontrolü |
| **WSL2 / Linux** | Alternatif çalıştırma ortamı |

---

## 8. Bağımlılık Kurulum Komutları (referans)

```r
# Runtime zorunlu
install.packages(c("shiny", "bslib", "leaflet", "plotly", "DT",
                   "sf", "data.table", "viridisLite", "dplyr",
                   "htmlwidgets"))

# Runtime opsiyonel (önerilir)
install.packages(c("shinycssloaders", "waiter", "leaflet.extras",
                   "bsicons", "echarts4r"))

# Offline ETL (data_hazirla.R için)
install.packages(c("rnaturalearth", "rnaturalearthdata",
                   "rmapshaper", "geodata", "terra",
                   "exactextractr", "wpp2024"))
# rnaturalearthhires CRAN'de değil:
install.packages("rnaturalearthhires",
                 repos = "https://ropensci.r-universe.dev",
                 type = "source")
```

---

## 9. Çalıştırma

```r
# Uygulama:
setwd("/mnt/c/Users/xamki/coğrafi")
shiny::runApp("app.R", launch.browser = TRUE)

# Veriyi yeniden üret:
Rscript data_hazirla.R
```

---

## 10. Sürüm Kilidi / Yeniden Üretilebilirlik

Proje şu anda `renv` veya benzeri kilit dosyası kullanmıyor. Yeniden üretilebilirlik için eklenmesi önerilir:

```r
renv::init()
renv::snapshot()
```

Bu, `renv.lock` dosyasını üretir ve paket sürümlerini sabitler.
