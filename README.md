# Dünya Nüfus Analizi Paneli

İnteraktif coğrafya/demografi dashboard'u — R Shiny ile geliştirildi.

BM DESA'nın **World Population Prospects 2024 (WPP 2024)** veri setini ve Worldometer 2025 göstergelerini kullanarak 1950–2100 arası tüm ülkeler için nüfus, yoğunluk, doğurganlık, yaşam süresi, net göç, büyüme oranı, yaş piramitleri ve demografik geçiş aşamalarını görselleştirir.

---

## İçerdiği Sekmeler

| Sekme | İçerik |
|---|---|
| **Genel Bakış** | Dünya nüfusu özet metrikler, en kalabalık 15 ülke, kıtalara göre dağılım, yoğunluk sıralamaları |
| **İnteraktif Harita** | Tıklanabilir dünya haritası — 6 metrik × 151 yıl, ülke bazlı detay kartı + yaş piramidi |
| **Tematik Haritalar** | Net göç · TFR · Yaşam süresi · Büyüme · Demografik geçiş · Bağımlılık · Medyan yaş · Kentleşme |
| **Projeksiyon** | Çoklu ülke × çoklu metrik zaman serileri (1950–2100) |
| **Yaş Piramidi** | İki ülkenin yaş+cinsiyet piramitlerini yan yana karşılaştır |
| **3D Dünya** | MapLibre tabanlı 3D interaktif harita + rayshader ile sinema kalitesinde render |
| **Yoğunluk-Büyüme** | Log ölçekli saçılım grafiği, balon boyutu = nüfus |
| **Karşılaştırma** | 2–4 ülke: özet kartlar + zaman serileri (4 metrik) + yaş piramitleri |
| **Veri Tablosu** | Aranabilir/sıralanabilir, kıta filtreli, CSV indirilebilir tablo |

---

## Nasıl Çalıştırılır?

### 1. R kurulumu
R **4.3+** önerilir. RStudio veya herhangi bir R ortamı yeterli.

### 2. Bağımlı paketleri kur
```r
install.packages(c(
  "shiny", "bslib", "leaflet", "plotly", "DT",
  "sf", "data.table", "viridisLite", "dplyr", "htmlwidgets",
  "htmltools", "jsonlite", "geojsonsf"
))

# Opsiyonel (uygulama yoklarsa da çalışır ama bazı özellikler sınırlanır):
install.packages(c("mapgl", "shinycssloaders", "leaflet.extras", "bsicons"))
```

### 3. Uygulamayı çalıştır
Proje klasöründe R konsolu açıp:
```r
shiny::runApp(".")
```
Veya terminal:
```bash
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
```

Tarayıcı otomatik açılır ve dashboard yüklenir (ilk açılış veri yüklemesi ~5 saniye sürer).

---

## Proje Yapısı

```
.
├── app.R                 # Ana Shiny uygulaması (~157 KB, tek dosya)
├── data_hazirla.R        # WPP 2024 + Natural Earth verilerini RDS'e dönüştürür
├── render_cinema.R       # 3D cinema render için rayshader betiği (opsiyonel)
├── guncel_population.csv # Worldometer 2025 snapshot
├── data/                 # Önceden hesaplanmış veri setleri (.rds)
│   ├── pop_yillik.rds      # Toplam nüfus (1950-2100)
│   ├── age_pyramid.rds     # Yaş+cinsiyet piramit verisi (~32 MB)
│   ├── tfr_yillik.rds      # Doğurganlık oranı
│   ├── e0_yillik.rds       # Yaşam süresi (erkek/kadın/toplam)
│   ├── mig_yillik.rds      # Net göç
│   ├── misc_yillik.rds     # Büyüme, doğum, ölüm oranları
│   ├── ulke_harita.rds     # Ülke sınırları (Natural Earth 1:10m, sadeleştirilmiş)
│   ├── ulke_ref.rds        # Ülke referans tablosu (kod, isim, kıta, alan)
│   └── eyalet_harita.rds   # ABD eyaletleri, TR illeri (GADM)
├── docs/
│   ├── architecture.md
│   └── tech-stack.md
└── README.md
```

---

## Veri Kaynakları

| Kaynak | İçerik | Güncellik |
|---|---|---|
| [BM WPP 2024](https://population.un.org/wpp/) | 1950–2023 tahmin + 2024–2100 projeksiyon (medium variant) | Nisan 2024 revizyonu (en güncel resmi sürüm) |
| [Worldometer](https://www.worldometers.info/world-population/population-by-country/) | 2025 snapshot — medyan yaş, şehirleşme, göç | 2025 ortası |
| [Natural Earth](https://www.naturalearthdata.com/) | 1:10m ülke sınırları | Sabit referans |

Yeni bir WPP revizyonu veya Worldometer snapshot'ı geldiğinde `data_hazirla.R`'yi tekrar çalıştırmak `data/*.rds` dosyalarını yeniler.

---

## Teknoloji Yığını

- **Frontend:** `bslib` (Bootstrap 5 + flatly tema), özel CSS ile modern kart tasarımı
- **Haritalar:** `leaflet` (2D) + `mapgl` (3D MapLibre) + `rayshader` (sinema)
- **Grafikler:** `plotly` (etkileşimli), `DT` (tablo)
- **Veri:** `data.table` (hızlı okuma/filtreleme), `sf` (coğrafi katmanlar)
- **Performans:** Reactive throttle/debounce, JS-side harita güncelleme (polygon'ları silmeden `setStyle` ile renk değiştirme), `bindCache` ile plot önbelleği

Daha detaylı mimari için: [`docs/architecture.md`](docs/architecture.md) ve [`docs/tech-stack.md`](docs/tech-stack.md).

---

## Lisans ve Atıf

Proje kodu eğitim amaçlıdır. Kullanılan veri setlerinin lisansları ilgili kaynaklara aittir:
- WPP 2024: © UN DESA Population Division
- Worldometer: © Dadax
- Natural Earth: Public Domain
