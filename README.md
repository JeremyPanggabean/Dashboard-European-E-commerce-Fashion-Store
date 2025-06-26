# 🛍️ Dashboard European E-commerce Fashion Store

## 📊 Ringkasan Project
Project ini adalah aplikasi dashboard interaktif berbasis **Shiny** yang dikembangkan menggunakan bahasa pemrograman **R**. Aplikasi ini memvisualisasikan data penjualan dari sebuah toko fashion online di wilayah Eropa.

Dashboard ini bertujuan untuk:
- Menyediakan insight penjualan harian dan total pendapatan
- Menganalisis perilaku pelanggan berdasarkan usia, negara, dan waktu registrasi
- Mengukur efektivitas penggunaan diskon dan promosi
- Menyajikan data mentah untuk keperluan eksplorasi atau analisis lanjutan

---

## 🗂️ Sumber Data

Data yang digunakan adalah hasil penggabungan dari tiga file Excel:
- `dataset_fashion_store_customers.xlsx`
- `dataset_fashion_store_sales.xlsx`
- `dataset_fashion_store_salesitems.xlsx`

Ketiganya digabung menjadi satu tabel PostgreSQL di Railway dengan nama:  
**`gabungan_data_customers_sales_items`**

---

## 🧾 Struktur Tabel (Kolom Utama)

| Nama Kolom            | Deskripsi                                         |
|------------------------|--------------------------------------------------|
| `id_penjualan`         | ID transaksi                                     |
| `tanggal_penjualan`    | Tanggal transaksi                                |
| `id_item`              | ID item terjual                                  |
| `id_produk`            | ID produk                                        |
| `jumlah`               | Jumlah unit dibeli                               |
| `harga_satuan`         | Harga per unit                                   |
| `jumlah_total`         | Total nilai pesanan                              |
| `status_diskon`        | Status diskon (Dengan / Tanpa Diskon)           |
| `saluran`              | Kanal penjualan (App, E-commerce, dll.)         |
| `kategori_penjualan`   | Kategori channel (Online-Middle, dll.)          |
| `id_pelanggan`         | ID pelanggan                                     |
| `profil_pelanggan`     | Negara dan segmentasi usia pelanggan            |
| `tanggal_mendaftar`    | Tanggal registrasi pelanggan                    |
| `kategori_usia`        | Segmentasi usia (Remaja, Dewasa, Lansia)        |
| `negara`               | Negara pelanggan                                 |

---

## 🧠 Fitur Dashboard

### 📌 Tab: Overview
- **Total Penjualan**, **Jumlah Transaksi**, dan **Jumlah Pelanggan**
- Grafik penjualan berdasarkan **Saluran** dan **Negara**

### 📌 Tab: Analisis Penjualan
- Tren penjualan harian
- Penjualan berdasarkan kategori usia
- Analisis penggunaan diskon

### 📌 Tab: Analisis Pelanggan
- Tren registrasi pelanggan bulanan
- Distribusi profil pelanggan dalam bentuk pie chart
- Rata-rata nilai pesanan per kategori usia

### 📌 Tab: Data Mentah
- Tabel interaktif yang menampilkan seluruh data mentah
- Fitur filter dan pencarian otomatis

---

## 🛠️ Teknologi yang Digunakan

- **R & Shiny**: Untuk membangun UI interaktif
- **PostgreSQL**: Sebagai penyimpanan data yang di-query langsung dari Railway
- **Plotly & ggplot2**: Untuk visualisasi data dinamis
- **DT**: Untuk menampilkan data tabel dengan filter dan pencarian
- **dplyr, lubridate**: Untuk manipulasi data dan waktu

---

## 🔗 Koneksi ke Database Railway
Koneksi dilakukan langsung menggunakan package `RPostgreSQL` dengan kredensial yang telah disesuaikan (lihat fungsi `get_db_connection()` di dalam file `app.R`).
