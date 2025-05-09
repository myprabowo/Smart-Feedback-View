library(shiny)
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(tidyr)
library(httr)
library(jsonlite)
library(commonmark)
library(shinydashboard)

# =========================================================================
# Penting: Ganti dengan kunci API Gemini Anda yang sebenarnya!
GEMINI_API_KEY <- "Insert your key here"
# =========================================================================

# Kategori dan Warna (Definisikan sebagai Variabel Global)
kategori <- c(
  "PeningkatanKeterampilan" = "Capaian Belajar",
  "PeningkatanPengetahuan" = "Capaian Belajar",
  "BahanAjar" = "Relevansi dan Kualitas Kurikulum",
  "Media" = "Relevansi dan Kualitas Kurikulum",
  "KesesuaianTujuan" = "Relevansi dan Kualitas Kurikulum",
  "KesesuaianWaktu" = "Relevansi dan Kualitas Kurikulum",
  "Konsumsi" = "Sarana dan Prasarana",
  "ATK" = "Sarana dan Prasarana",
  "Kenyamanan" = "Sarana dan Prasarana",
  "Fasilitas" = "Sarana dan Prasarana",
  "Kesopanan" = "Pelayanan Panitia",
  "Keadilan" = "Pelayanan Panitia",
  "Kesigapan" = "Pelayanan Panitia",
  "Kejelasan" = "Pelayanan Panitia",
  "Kemampuan" = "Pelayanan Panitia",
  "TanggungJawab" = "Pelayanan Panitia",
  "Kedisiplinan" = "Pelayanan Panitia"
)

warna_kategori <- c(
  "Capaian Belajar" = "indianred",
  "Relevansi dan Kualitas Kurikulum" = "cyan4",
  "Sarana dan Prasarana" = "goldenrod1",
  "Pelayanan Panitia" = "cornflowerblue"
)

# Daftar Nama Bulan
nama_bulan <- c("Semua", "Januari", "Februari", "Maret", "April", "Mei", "Juni",
                "Juli", "Agustus", "September", "Oktober", "November", "Desember")

# Fungsi Gemini
gemini <- function(prompt,
                   temperature = 1,
                   max_output_tokens = 200,
                   api_key = GEMINI_API_KEY,  # Gunakan variabel global
                   model = "gemini-2.0-flash") {
  
  if(api_key == "YOUR_ACTUAL_API_KEY"){
    stop("Anda belum mengganti 'YOUR_ACTUAL_API_KEY' dengan kunci API Gemini Anda!")
  }
  
  model_query <- paste0(model, ":generateContent")
  
  response <- tryCatch({
    POST(
      url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
      query = list(key = api_key),
      content_type_json(),
      encode = "json",
      body = list(
        contents = list(
          parts = list(
            list(text = prompt)
          )),
        generationConfig = list(
          temperature = temperature,
          maxOutputTokens = max_output_tokens
        )
      )
    )
  }, error = function(e) {
    stop(paste("Gagal membuat permintaan ke Gemini API:", e$message))
  })
  
  
  if (response$status_code > 200) {
    stop(paste("Error - ", content(response)$error$message))
  }
  
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
  
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Survei Pelatihan"),
  dashboardSidebar(
    selectInput("tahun", "Tahun:",
                choices = c("2024", "2025"),
                selected = "2025"),
    selectInput("pelatihan", "Pelatihan yang Diikuti:",
                choices = c("Semua", ""), # Choices akan diupdate di server
                selected = "Semua"),
    selectInput("periode", "Periode Pelatihan (Bulan):",
                choices = nama_bulan,
                selected = "Semua"),
    actionButton("analyzeButton", "Analisis Kualitatif") # Tombol Analisis
  ),
  dashboardBody(
    fluidRow(
      infoBoxOutput("totalResponden"),
      infoBoxOutput("rataRataAspek"),
      infoBoxOutput("rangePenilaian") #Tambahkan infoBoxOutput baru
    ),
    fluidRow(
      box(plotOutput("barChart"), width = 12)
    ),
    fluidRow(
      box(htmlOutput("geminiAnalysis"), width = 12)
    )
  )
)

# Server
server <- function(input, output, session) { # Tidak ada argumen tambahan
  
  # Path relatif ke file service account (pastikan sudah dipindahkan dari WWW!)
  service_account_path <- "secrets/gentle-edition-458710-g7-ecafe7761a39.json"
  
  # Pastikan file ada
  if (!file.exists(service_account_path)) {
    stop(paste("File service account tidak ditemukan di:", service_account_path))
  }
  
  # Definisikan drive_scopes
  drive_scopes <- c(
    "https://www.googleapis.com/auth/drive",
    "https://www.googleapis.com/auth/spreadsheets.readonly" #atau "https://www.googleapis.com/auth/spreadsheets" jika perlu menulis
  )
  
  # Otorisasi Google Sheets dengan Service Account
  gs4_auth(
    path = service_account_path,
    scopes = drive_scopes
  )
  
  
  # 1. Baca Data dari Google Sheet (dengan error handling)
  data <- reactive({
    tryCatch({
      sheet_url <- ifelse(input$tahun == "2024",
                          "https://docs.google.com/spreadsheets/d/11EvoDiXft7_Bfnu6vxA9eVYQYdd8KJNBx-GmIJEHwdk/edit?usp=sharing",
                          "https://docs.google.com/spreadsheets/d/1hSwQldPgxc7wIcjF_f89uGGENnNuoveeiYgqEbt251M/edit?usp=sharing")
      read_sheet(sheet_url)
    }, error = function(e) {
      showNotification(paste("Gagal membaca data dari Google Sheet:", e$message), type = "error", duration = NULL)
      NULL # Kembalikan NULL jika terjadi kesalahan
    })
  })
  
  # 2. Ubah Nama Kolom dan Tipe Data (Reaktif terhadap perubahan data)
  formatted_data <- reactive({
    req(data())  # Memastikan data sudah berhasil dibaca
    
    local_data <- data()  # Buat salinan lokal dari data
    
    names(local_data) <- c("Timestamp", "Pelatihan", "Periode", "Kedisiplinan", "TanggungJawab",
                           "Kemampuan", "Kejelasan", "Kesigapan", "Keadilan", "Kesopanan",
                           "Fasilitas", "Kenyamanan", "ATK", "Konsumsi", "KesesuaianWaktu",
                           "KesesuaianTujuan", "Media", "BahanAjar", "PeningkatanPengetahuan",
                           "PeningkatanKeterampilan", "KurangiWaktu", "TambahWaktu", "PerbaikiSilabus",
                           "HapusMateri", "TambahMateri", "KritikSaran")
    
    # Ubah Periode menjadi Faktor (setelah di-format)
    local_data$Periode <- factor(local_data$Periode, levels = nama_bulan)
    
    local_data <- local_data %>%
      mutate(across(Kedisiplinan:PeningkatanKeterampilan, as.numeric)) # Konversi kolom Likert
    
    return(local_data)
  })
  
  # Update Pilihan selectInput (setelah data dimuat)
  observe({
    req(formatted_data()) # Pastikan data sudah diformat
    updateSelectInput(session, "pelatihan", choices = c("Semua", unique(formatted_data()$Pelatihan)), selected = "Semua")
    #updateSelectInput(session, "periode", choices = nama_bulan, selected = "Semua") #Tidak perlu diupdate karena sudah statis
  })
  
  # 3. Filter Data (Reaktif terhadap pilihan user dan data)
  filtered_data <- reactive({
    req(formatted_data())
    
    local_data <- formatted_data()
    
    if (input$pelatihan != "Semua") {
      local_data <- local_data %>% filter(Pelatihan == input$pelatihan)
    }
    
    # Filter Periode (gunakan faktor level)
    if (input$periode != "Semua") {
      local_data <- local_data %>% filter(Periode == input$periode)
    }
    
    return(local_data)
  })
  
  # 4. Buat Plot (Reaktif terhadap filter dan data)
  output$barChart <- renderPlot({
    req(filtered_data())
    
    summary_data <- filtered_data() %>%
      summarise(across(Kedisiplinan:PeningkatanKeterampilan, ~mean(., na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "Aspek", values_to = "RataRata") %>%
      mutate(
        Kategori = recode(Aspek, !!!kategori), # Gunakan kategori global
        Aspek = factor(Aspek, levels = names(kategori)),
        Aspek_Label = paste0(Aspek, " (", round(RataRata, 2), ")") # Buat label baru
      )
    
    urutan_kategori <- c("Pelayanan Panitia",  "Sarana dan Prasarana", "Relevansi dan Kualitas Kurikulum", "Capaian Belajar")
    
    title <- paste0("Penilaian ",
                    ifelse(input$pelatihan == "Semua", "Semua Pelatihan", input$pelatihan),
                    "\nPeriode ",
                    ifelse(input$periode == "Semua", "Semua Bulan", input$periode),
                    " Tahun ", input$tahun)
    
    ggplot(summary_data, aes(x = Aspek, y = RataRata, fill = Kategori)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = warna_kategori[urutan_kategori],
                        breaks = urutan_kategori) +
      scale_x_discrete(labels = setNames(summary_data$Aspek_Label, summary_data$Aspek)) + # Gunakan label baru
      labs(title = title,
           x = "Aspek Pelayanan",
           y = "Rata-rata",
           fill = "Kategori") +
      theme_minimal() +
      theme(text = element_text(size = 14),
            plot.title = element_text(hjust = 0.5)) + # Rata tengah
      theme(legend.position = "bottom")
  })
  
  # 5. Info Box: Total Responden
  output$totalResponden <- renderInfoBox({
    req(filtered_data())
    
    total_responden <- nrow(filtered_data())
    
    infoBox(
      "Total Responden",
      total_responden,
      icon = icon("users"),
      color = "purple"
    )
  })
  
  # 6. Info Box: Rata-rata Keseluruhan Aspek
  output$rataRataAspek <- renderInfoBox({
    req(filtered_data())
    
    # Hitung rata-rata semua aspek
    rata_rata_keseluruhan <- filtered_data() %>%
      summarise(across(Kedisiplinan:PeningkatanKeterampilan, ~mean(., na.rm = TRUE))) %>%
      summarise(RataRata = mean(c_across(everything()))) %>%
      pull(RataRata)
    
    infoBox(
      "Rata-rata Keseluruhan Aspek",
      round(rata_rata_keseluruhan, 2),
      icon = icon("star"),
      color = "green"
    )
  })
  
  # 7. Info Box: Range Penilaian (Nilai Rata-rata Aspek)
  output$rangePenilaian <- renderInfoBox({
    req(filtered_data())
    
    # Hitung rata-rata setiap aspek
    summary_data <- filtered_data() %>%
      summarise(across(Kedisiplinan:PeningkatanKeterampilan, ~mean(., na.rm = TRUE)))
    
    # Ambil nilai rata-rata sebagai vektor
    rata_rata_aspek <- unlist(summary_data)
    
    # Hitung nilai terendah dan tertinggi dari RATA-RATA aspek
    nilai_terendah <- min(rata_rata_aspek)
    nilai_tertinggi <- max(rata_rata_aspek)
    
    # Format range sebagai string
    range_string <- paste(round(nilai_terendah, 2), "-", round(nilai_tertinggi, 2))
    
    infoBox(
      "Range Penilaian (Rata-rata Aspek)",
      range_string,
      icon = icon("signal"),
      color = "yellow"
    )
  })
  
  
  # 8. Analisis Gemini (Reaktif terhadap tombol "Analisis Kualitatif")
  gemini_analysis_results <- eventReactive(input$analyzeButton, { # Hanya dipicu saat tombol ditekan
    req(filtered_data())
    
    local_data <- filtered_data()
    text_data <- local_data %>%
      select(KurangiWaktu, TambahWaktu, PerbaikiSilabus, HapusMateri, TambahMateri, KritikSaran)
    
    instructions <- list(
      KurangiWaktu = "Ringkas topik-topik yang peserta ingin dikurangi waktunya dalam bentuk paragraf tanpa bullet dengan maksimal 150 kata:",
      TambahWaktu = "Ringkas topik-topik yang peserta ingin ditambahkan waktunya dalam bentuk paragraf tanpa bullet dengan maksimal 150 kata:",
      PerbaikiSilabus = "Ringkas perbaikan silabus yang disarankan oleh peserta dalam bentuk paragraf tanpa bullet dengan maksimal 150 kata:",
      HapusMateri = "Ringkas materi-materi yang peserta sarankan untuk dihapus dalam bentuk paragraf tanpa bullet dengan maksimal 150 kata:",
      TambahMateri = "Ringkas materi baru yang peserta sarankan untuk ditambahkan dalam bentuk paragraf tanpa bullet dengan maksimal 150 kata:",
      KritikSaran = "Ringkas umpan balik dan saran umum dari peserta dalam bentuk paragraf tanpa bullet dengan maksimal 150 kata:"
    )
    
    tryCatch({
      list(
        KurangiWaktu = commonmark::markdown_html(gemini(prompt = paste(instructions$KurangiWaktu, paste(text_data$KurangiWaktu, collapse = " ")))),
        TambahWaktu = commonmark::markdown_html(gemini(prompt = paste(instructions$TambahWaktu, paste(text_data$TambahWaktu, collapse = " ")))),
        PerbaikiSilabus = commonmark::markdown_html(gemini(prompt = paste(instructions$PerbaikiSilabus, paste(text_data$PerbaikiSilabus, collapse = " ")))),
        HapusMateri = commonmark::markdown_html(gemini(prompt = paste(instructions$HapusMateri, paste(text_data$HapusMateri, collapse = " ")))),
        TambahMateri = commonmark::markdown_html(gemini(prompt = paste(instructions$TambahMateri, paste(text_data$TambahMateri, collapse = " ")))),
        KritikSaran = commonmark::markdown_html(gemini(prompt = paste(instructions$KritikSaran, paste(text_data$KritikSaran, collapse = " "))))
      )
    }, error = function(e) {
      list(error = paste("Gagal melakukan analisis Gemini API:", e$message))
    })
  })
  
  output$geminiAnalysis <- renderUI({
    analysis_results <- gemini_analysis_results()
    
    if (is.null(analysis_results)) {
      return(HTML("<p>Tekan tombol 'Analisis Kualitatif' untuk memulai analisis.</p>"))
    }
    
    if (!is.null(analysis_results$error)) {
      return(HTML(paste("<p style='color:red;'>", analysis_results$error, "</p>")))
    }
    
    html_content <- paste(
      "<div>",
      "<h3>Analisis Masukan Peserta Pelatihan:</h3>",
      "<div><strong>A. Materi yang Perlu Dikurangi Waktunya:</strong>", analysis_results$KurangiWaktu, "</div>",
      "<div><strong>B. Materi yang Perlu Ditambah Waktunya:</strong>", analysis_results$TambahWaktu, "</div>",
      "<div><strong>C. Materi yang Materi/Silabusnya Perlu Diperbaiki:</strong>", analysis_results$PerbaikiSilabus, "</div>",
      "<div><strong>D. Materi yang Sebaiknya Dihapus:</strong>", analysis_results$HapusMateri, "</div>",
      "<div><strong>E. Materi yang Perlu Ditambahkan Selain Mata Pelajaran yang Sudah Ada:</strong>", analysis_results$TambahMateri, "</div>",
      "<div><strong>F. Kritik dan Saran:</strong>", analysis_results$KritikSaran, "</div>",
      "</div>",
      sep = ""
    )
    
    HTML(html_content)
  })
}

# Run the application
shinyApp(ui = ui, server = server)