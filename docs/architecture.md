# Dünya Nüfus Analizi Paneli — Mimarî Dokümanı (Brownfield)

> Tür: Tek-dosyalı R Shiny dashboard
> Ana giriş: `app.R` (~2300 satır)
> Veri kaynağı: BM WPP 2024 + Worldometer 2025 + Natural Earth
> Dil: Türkçe (UTF-8)
> Sürüm tabanı: Nisan 2026 kod tabanı fotoğrafı

---

## 1. Genel Bakış

Bu proje, **BM Dünya Nüfus İhtimalleri (WPP) 2024** veri setinin 1950–2100 dönemi için 9 sekmeli, Türkçe arayüzlü, interaktif bir keşif panelidir. Uygulama **tek bir `app.R`** dosyasında yaşayan, monolitik bir Shiny uygulamasıdır; `bslib` ile modern (Bootstrap 5 / Flatly) bir tema, harita katmanı için `leaflet`, grafikler için `plotly`, 3D küre için `echarts4r` ve ham veri görünümü için `DT` kullanır.

Brownfield perspektifinden iki ayrı katman vardır:

1. **Veri hazırlama katmanı** (`data_hazirla.R`) — tek seferlik ya da veri güncellemesi gerektikçe çalıştırılan, WPP2024 ve Natural Earth kaynaklarından ön-hesaplanmış `.rds` dosyaları üretir.
2. **Sunum / etkileşim katmanı** (`app.R`) — önceden üretilmiş `.rds` dosyalarını okur, runtime'da harita ve grafik üretir.

Bu ayrım, Shiny sunucusunun soğuk başlatma süresini kabul edilebilir tutar: ağır `sf` basitleştirme, raster örnekleme ve `wpp2024` paketi çözümlemesi `data_hazirla.R` tarafında gerçekleşir.

---

## 2. Yüksek Seviye Mimari

```
+------------------------------------------------------------+
|                    VERI HAZIRLAMA (offline)                |
|  data_hazirla.R                                            |
|    - wpp2024 (pop1dt, popproj1dt, popAge1dt, tfr, e0, ...) |
|    - rnaturalearth(hires) + rmapshaper (sadeleştirme)      |
|    - geodata / terra / exactextractr (ek rasterler)        |
|    ->  data/*.rds  (9 dosya)                               |
+------------------------------------------------------------+
                          |
                          v (readRDS, global scope)
+------------------------------------------------------------+
|                   SHINY UYGULAMASI (app.R)                 |
|                                                            |
|  GLOBAL (bir kez, tüm oturumlar için paylaşılan)           |
|    - readRDS ile 9 tablo ve sf nesnesi                     |
|    - guncel_population.csv (Worldometer) ismeleştirme      |
|    - ulke_tr / kita_tr Türkçe sözlükleri                   |
|    - ulke_etiket_* (büyük/orta/küçük centroidler)          |
|    - ekle_ulke_etiketleri() (label katmanı)                |
|                                                            |
|  UI (page_navbar + bslib)                                  |
|    - Flatly teması + özel CSS (~200 satır <style>)         |
|    - 9 nav_panel (sekme)                                   |
|    - input_dark_mode() açık/koyu geçişi                    |
|                                                            |
|  SERVER (function(input, output, session))                 |
|    - .veri_cache_env: yıl bazlı merge cache                |
|    - ulke_veri_yil(yil): pop/tfr/e0/mig/misc birleştirici  |
|    - piramit_ciz(cc, yil, kompakt)                         |
|    - Her sekme için bir output bloğu                       |
|                                                            |
+------------------------------------------------------------+
                          |
                          v
                  Kullanıcı (tarayıcı)
                   HTML + JS widget'lar
```

### 2.1 Tasarım Prensipleri

- **Tek-dosya monolit** — Kasıtlı. Proje akademik / demo niteliğindedir; modül (`shinyModule`) ayrımı yapılmamıştır.
- **Global scope veri yükleme** — 9 `.rds` dosyası `app.R`'nin üstünde okunur; tüm Shiny oturumları aynı in-memory kopyayı paylaşır. `age_pyramid.rds` (~30 MB) en büyük tablodur.
- **Yıl bazlı cache (`.veri_cache_env`)** — Aynı yıl birden fazla sekme için istenince `merge`'ler tekrarlanmaz. Bir environment içinde string anahtarla (yıl) saklanır.
- **Opsiyonel paket graceful-degrade** — `shinycssloaders`, `waiter`, `leaflet.extras`, `bsicons`, `echarts4r` yüklü değilse uygulama yine ayakta kalır; `.has_*` bayrakları ve sarmalayıcı fonksiyonlar (`withSpin`, `bsi`) bu dallanmayı yönetir.
- **Hata gizleme** — `options(shiny.sanitize.errors = TRUE)` + `options(warn = -1)` ile kullanıcıya ham R hataları sızdırılmaz; bu kasıtlıdır (kullanıcı talebi).
- **Türkçe yerelleştirme** — UTF-8 locale `Turkish_Turkey.65001` → `.UTF-8` fallback zinciri ile Windows ve Linux'ta Türkçe karakter bütünlüğü sağlanır. Tüm etiket/metin Unicode escape (`\u...`) ile yazılmıştır.

---

## 3. Dosya Yapısı

```
coğrafi/
├── app.R                           # Ana Shiny uygulaması (~2300 satır, tek dosya)
├── app_backup.R                    # Önceki sürüm yedeği (~1500 satır) — referans
├── data_hazirla.R                  # RDS üretim scripti (offline çalışır)
├── nufus_yogunlugu_projesi.R       # Eski/analitik R scripti (PNG/PDF çıktıları üretir)
├── diagnose.R / diagnose2.R        # Veri teşhis scriptleri (küçük, araç)
├── test_wpp2024.R / test_wpp2024b.R # wpp2024 paket keşif scriptleri
├── guncel_population.csv           # Worldometer 2025 (medyan yaş, kentleşme, yoğunluk)
├── Wolrd Population Data.csv       # Eski CSV (proje öncesi, kullanılmıyor)
├── data/                           # Ön-hesaplanmış RDS dosyaları (Shiny'nin tek okuma kaynağı)
│   ├── ulke_harita.rds             # 61 KB — sf nesnesi (Natural Earth 50m, basitleştirilmiş)
│   ├── eyalet_harita.rds           # 656 KB — opsiyonel ikinci seviye admin katmanı
│   ├── ulke_ref.rds                # 6 KB — country_code, continent, Alan_km2, iso3
│   ├── pop_yillik.rds              # 270 KB — (country_code, year, pop) 1950–2100
│   ├── age_pyramid.rds             # 32 MB — (cc, year, age, popM, popF, pop) tek yaş
│   ├── tfr_yillik.rds              # 338 KB — toplam doğurganlık hızı
│   ├── e0_yillik.rds               # 395 KB — yaşam beklentisi (B/M/F)
│   ├── mig_yillik.rds              # 126 KB — net göç
│   └── misc_yillik.rds             # 901 KB — growthrate, births, deaths
├── geodata_cache/                  # geodata paketinin indirme cache'i
├── 01..08_*.png / *.html           # Statik analitik çıktılar (nufus_yogunlugu_projesi.R üretir)
└── docs/                           # Proje mimari dokümantasyonu
    ├── architecture.md
    └── tech-stack.md
```

### 3.1 Kritik Dosyaların Rolleri

| Dosya | Rol | Değişim Sıklığı |
|---|---|---|
| `app.R` | Tek Shiny uygulama dosyası — UI + server | Aktif (son değişim: Nisan 2026) |
| `data_hazirla.R` | WPP/NaturalEarth → RDS ETL | Düşük (WPP yeni revizyonda) |
| `data/*.rds` | Uygulamanın gerçek veri kaynağı | `data_hazirla.R` çalışınca |
| `guncel_population.csv` | Worldometer snapshot (medyan yaş, kent %) | Yıllık |
| `app_backup.R` | Güvenli yedek, referans amacıyla tutulur | Dokunulmaz |

---

## 4. Veri Modeli

Tüm tablolar `data.table` formatında, `country_code` (UN M49 üç haneli) birincil birleştirme anahtarıdır.

### 4.1 Uzun Format (yıl × ülke)

| Tablo | Anahtar | Metrik Sütunları | Kapsam |
|---|---|---|---|
| `pop_dt` | (country_code, year) | `pop` (kişi, ×1000 çözülmüş) | 1950–2100, ~237 ülke |
| `tfr_dt` | (country_code, year) | `tfr` | 1950–2100 |
| `e0_dt` | (country_code, year) | `e0B`, `e0M`, `e0F` (yaşam beklentisi B/M/F) | 1950–2100 |
| `mig_dt` | (country_code, year) | `mig` (net göç, kişi) | 1950–2100 |
| `misc_dt` | (country_code, year) | `growthrate`, `births`, `deaths` | 1950–2100 |
| `age_dt` | (country_code, year, age) | `popM`, `popF`, `pop` | 1950–2100, tek yaş (0–100+) |

### 4.2 Referans / Harita

- `ulke_ref` — country_code, name, name_tr, continent, Alan_km2, adm0_a3 (ISO3).
- `ulke_sf` — `sf` objesi, geometri + country_code + name_tr + Alan_km2. NaturalEarth 50m, `rmapshaper::ms_simplify()` ile sadeleştirilmiş.
- `eyalet_sf` — opsiyonel ikinci seviye (eyalet/il) sınırlar.
- `wmeter_dt` — Worldometer 2025 snapshot: wm_median_age, wm_urban_pct, wm_tfr, wm_density.

### 4.3 İsim Eşleme Zorlukları

Natural Earth ↔ WPP2024 ↔ Worldometer arasında ülke adı tutarsızlıkları vardır. `app.R`'de üç eşleme tablosu bulunur:

1. **`.eksik_kodlar`** — Natural Earth'ün `-99` döndürdüğü ISO3 kodları için manuel WPP M49 eşlemesi (NOR, KOS, TWN, FRA, PSE, SDS, SOL, CYN).
2. **`wm_isim_eslestir`** — Worldometer İngilizce isimleri → WPP isimleri (30+ giriş).
3. **`ulke_tr`** — Natural Earth + WPP isimleri → Türkçe (250+ giriş). `to_tr()` fonksiyonu bu sözlükte arama yapar, bulamazsa orijinal ismi döner.

Bu sözlükler kod tabanında satır 80–280 arasında yaşar; yeni ülke veya ada eklemek gerekirse tek değiştirilecek yer burasıdır.

---

## 5. UI Mimarisi

### 5.1 Shell

```r
ui <- page_navbar(
  title = ...,
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#6366f1", ...),
  fillable = TRUE,
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
  header = tags$head(... <style>... 200 satır CSS ...),
  nav_panel("Genel Bakış", ...),    # Sekme 1
  nav_panel("İnteraktif Harita", ...),  # Sekme 2
  ...
  nav_panel("Veri Tablosu", ...)     # Sekme 9
)
```

### 5.2 Tema Paleti (CSS değişkenleri)

- **Ana renk**: `#6366f1` (indigo-500)
- **İkincil**: `#8b5cf6` (violet-500)
- **Başarı/bilgi/uyarı/tehlike**: `#10b981 / #06b6d4 / #f59e0b / #ef4444`
- **Navbar arka planı**: `linear-gradient(135deg, #0f172a 0%, #1e293b 100%)`
- **Gövde arka planı (açık)**: `#f8fafc`
- **Gövde arka planı (koyu)**: `#0b1120`
- **Kart radius**: `14px` / Value box: `16px`
- **Fontlar**: `Plus Jakarta Sans` (gövde) + `Outfit` (başlıklar) + `JetBrains Mono` (kod). Google Fonts üzerinden yüklenir.

### 5.3 Özel CSS Blokları

`app.R` içindeki tek büyük `<style>` bloğu (satır ~441–619) şu gruplara ayrılmıştır:

- Temel tipografi
- Navbar modern görünüm
- Kartlar (hover, shadow, radius)
- Value box modernleştirme + gradient sınıfları (`.vb-grad-1..4`)
- Sidebar gradient
- Form elemanları (+ irs slider)
- Nav pills (tematik haritalar için)
- DT tablo başlıkları
- Leaflet/Plotly konteyner stili
- Scrollbar (webkit)
- CSS-tabanlı yükleme spinner'ı (`.recalculating::after`)
- Koyu mod override'ları (`[data-bs-theme='dark']`)
- Giriş animasyonu (`fadeInUp`)
- Tab intro kutusu (`.tab-intro`)
- Zoom-bağımlı ülke etiketi görünürlüğü (`.zoom-1` → `.zoom-5plus`)

### 5.4 Ortak UI Yardımcıları

| Fonksiyon | Görev |
|---|---|
| `info_baslik(baslik, aciklama)` | Her sekmenin üstünde gradient çerçeveli tanıtım kutusu |
| `withSpin(x)` | shinycssloaders varsa spinner sar, yoksa olduğu gibi döndür |
| `bsi(name)` | bsicons varsa bs_icon, yoksa Font Awesome icon |
| `fmt_sayi(x)` | `format(round(x), big.mark=",")`, NA → "-" |
| `to_tr(x)` | Ülke ismini Türkçeleştir |
| `ekle_ulke_etiketleri(map)` | Leaflet haritasına 3 boyut (büyük/orta/küçük) ülke adı etiketleri + zoom-bağlı JS gözleyici ekler |

---

## 6. Server Mimarisi

### 6.1 Paylaşılan Yardımcılar (server scope)

```r
.veri_cache_env <- new.env(parent = emptyenv())
ulke_veri_yil(yil)   # Her metrik tablosunu yıl filtreleyip country_code üzerinden merge eder
                     # Sonucu cache_env[as.character(yil)] altında saklar
piramit_ciz(cc, yil, kompakt = FALSE)   # Plotly horizontal bar yaş piramidi
                                          # Erkek negatife çevrilir (overlay mode)
bos_grafik(mesaj, ikon)                 # Plotly_empty + ikon + alt metin (veri yok durumu için)
```

### 6.2 Sekme-başına Output Yapısı

Her sekme kendi output bloğunu sahiplenir; aralarında paylaşılan bir reaktif zincir yok (istisna: `harita_yil_r` throttled reactive sadece Sekme 2'de kullanılır). Bu monolitik ama anlaşılır bir yapı sağlar — bir sekmeyi değiştirmek diğerlerini nadiren bozar.

```
Sekme 1 (Genel Bakış)
  Statik hesap: gb_veri <- ulke_veri_yil(2026) + ulke_ref merge
  output$vb_nufus, vb_ulke, vb_yogunluk, vb_tfr          (textOutput)
  output$gb_top15, gb_pasta, gb_kita_yogunluk,
         gb_en_yogun, gb_en_seyrek                        (plotly)
  observeEvent(input$buyut_*) -> ac_modal() (her grafik için büyütme)

Sekme 2 (İnteraktif Harita)
  harita_yil_r <- reactive(input$harita_yil) %>% throttle(300)
  output$ana_harita (leaflet, choropleth, palet metriğe göre)
  observeEvent(input$ana_harita_shape_click) -> seçili_ulke tutulur
  output$harita_detay (htmlOutput — sağ tıklanan ülkenin kart özeti)
  output$harita_piramit (sidebar içinde mini piramit)

Sekme 3 (Nüfus Projeksiyonu)
  output$proj_grafik — input$proj_ulkeler × input$proj_metrik
                        çoklu line chart, 2023 kesik çizgi

Sekme 4 (Yaş Piramidi)
  output$pir_baslik1/2, piramit1/2 — piramit_ciz() iki ülke

Sekme 5 (Tematik Haritalar)
  tema_render(metrik)  — ortak leaflet render fabrikası
  output$tema_goc_harita, tema_tfr_harita, tema_e0_harita,
         tema_buy_harita, tema_yas_harita, tema_kent_harita,
         tema_gecis_harita, tema_bag_harita

Sekme 6 (3D Görünüm)
  if (.has_echarts) output$d3_globe <- renderEcharts4r({
      echarts4r 3D globe + sütunlar (bars3D)
  })

Sekme 7 (Yoğunluk-Büyüme)
  output$scatter_grafik — log x, balon = nüfus, renk = kıta

Sekme 8 (Karşılaştırma)
  kars_ccs <- reactive(input$kars_ulkeler)
  output$kars_ozet_kartlar (uiOutput — dinamik kart grid)
  output$kars_zaman (plotly zaman serisi)
  output$kars_piramitler (uiOutput — n × plotly piramit)

Sekme 9 (Veri Tablosu)
  tablo_veri <- reactive(...)
  output$tablo_baslik, veri_tablosu (DT)
  downloadHandler("tablo_indir_csv")
```

### 6.3 Reaktif Performans Notları

- **Throttle**: `harita_yil_r %>% throttle(300)` — slider oynatıldığında her kare için choropleth yeniden çizilmez.
- **Global hesaplanan `gb_veri`** — Genel Bakış sekmesinde 2026 yılı verisi uygulama başlatılırken bir kez hesaplanır (reactive değil). Bu sayede Sekme 1 anında açılır ama "canlı yıl değişimi" desteklemez (zaten tasarım böyle).
- **`.veri_cache_env`** — İnteraktif Harita + Tematik Haritalar aynı yıl seçildiğinde merge sonuçları paylaşılır.
- **data.table by-reference mutasyonları** — `copy()` yalnızca piramit fonksiyonunda kullanılır, diğer yerlerde dikkatli olunmalı (mevcut kod emniyetli durumda).

---

## 7. Dış Bağımlılıklar ve Yerel Varsayımlar

### 7.1 Çalıştırma Ortamı

- **R 4.5.2** (geliştirme ortamı)
- Windows 10/11 veya Linux — Windows için `Turkish_Turkey.65001` locale denenir, başarısızsa `.UTF-8` fallback.
- `options(encoding = "UTF-8")` global set edilir.

### 7.2 Offline / Online Durumu

- **Runtime**: Tamamen offline çalışır. `readRDS` ve `fread` dışında dış çağrı yok.
- **Fontlar**: Google Fonts'tan `<link>` ile çekilir — internet yoksa sistem fallback fontlarına düşer (CSS `font-family` zincirinde `-apple-system, 'Segoe UI', sans-serif` vardır).
- **Leaflet tile'ları**: OpenStreetMap default tile'ı — internet gerekir.
- **3D Globe (echarts4r)**: ECharts library'nin dünya textur'u internet gerektirir.

### 7.3 Paket Kurulum Notu

`data_hazirla.R` ciddi bağımlılık ağacı ister (`geodata`, `terra`, `exactextractr`, `rnaturalearthhires`); `app.R` ise daha hafiftir. `rnaturalearthhires` CRAN'de değil — GitHub / alternatif repodan kurulur.

---

## 8. Bilinen Teknik Borçlar ve Riskler

| Konu | Açıklama | Risk |
|---|---|---|
| Tek-dosya monolit | `app.R` 2300+ satır, modül ayrımı yok | Orta — değişiklik odağı dar olursa okunur; büyümesi zor |
| Global scope veri | 9 RDS tüm oturumlara paylaşılır; age_dt ~30 MB bellek | Düşük-orta — sunucuda çok kullanıcı varsa belleğe dikkat |
| Global `options(warn = -1)` | Gerçek uyarılar görünmez | Orta — debug sırasında bu satır kapatılmalı |
| `options(shiny.sanitize.errors = TRUE)` | Kullanıcıya hata gizlenir | Düşük — bilinçli tasarım |
| İsim eşleme sözlükleri | ~300 satırlık Türkçe ve Worldometer eşleme; yeni ülkede manuel ek gerekir | Düşük — seyrek değişir |
| `app_backup.R` ve eski CSV'ler | Depoya dahil | Çok düşük — temizlik fırsatı |
| CSS 200 satır inline | UI dosyasında gömülü | Düşük — external `.css` olarak ayrılabilir |
| Sekme server bloklarının yerleşimi | Sekme 5 tema_gecis ve tema_bag en altta (satır 2192/2264) — geri kalan sekmeden sonra | Düşük — ama "tek yerde" olmadığı için arama gerekir |
| İkili sekme numaralandırma | Kodda yorum "SEKME 7" iki kez kullanılır (Karşılaştırma + Veri Tablosu) | Çok düşük — kozmetik |

---

## 9. Genişletme Kılavuzu

### 9.1 Yeni Metrik Ekleme

1. `data_hazirla.R`'de yeni WPP veri setini oku, `(country_code, year, metrik)` uzun formatta kaydet (`saveRDS`).
2. `app.R` global scope'ta `yeni_dt <- .safe_read("data/yeni.rds")` ekle.
3. `ulke_veri_yil(yil)` içinde yeni tabloyu da `merge` et.
4. İlgili sekme(ler)e metriği choice listesinde göster.

### 9.2 Yeni Sekme Ekleme

1. `ui` içinde `nav_panel("Ad", icon = icon(...), info_baslik(...), layout_sidebar(sidebar = sidebar(...), ...))` ekle.
2. `server` bloğunda `output$xxx <- render...({ ... })` yaz.
3. `withSpin()` sarmala (opsiyonel).

### 9.3 Yeni Ülke / Ad Düzeltmesi

- Türkçe ad: `ulke_tr` sözlüğüne ekle.
- Worldometer adı ↔ WPP adı: `wm_isim_eslestir`'e ekle.
- Natural Earth `-99` ISO3 → M49: `.eksik_kodlar`'a ekle.

### 9.4 Veriyi Yeniden Oluşturma

```bash
Rscript data_hazirla.R
```

Bu `data/` klasörünü yeniden doldurur. Tipik süre: birkaç dakika (wpp2024 verisi zaten paket içinde, NaturalEarth sadeleştirme CPU-yoğun).

---

## 10. Referanslar

- **BM WPP 2024**: https://population.un.org/wpp/
- **R wpp2024 paketi**: UNPD resmi dağıtımı
- **Natural Earth**: https://www.naturalearthdata.com/
- **Worldometer**: https://www.worldometers.info/world-population/population-by-country/
- **bslib**: https://rstudio.github.io/bslib/
- **echarts4r**: https://echarts4r.john-coene.com/

---

*Bu doküman Nisan 2026 kod tabanına göre oluşturulmuştur. `app.R`'de yapısal değişiklik yapıldığında (yeni sekme, veri tablosu, büyük refactor) güncellenmelidir.*
