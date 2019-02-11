function(input, output, session) {

  # Pop up menu proyek prioritas 1
  lapply(
    1:length(prioritas.1$no.proyek), 
    function(i) {  
      observeEvent(input[[paste0("klikA", i)]], {
        
        # Informasi proyek prioritas 1
        output[[paste0("tabel.proyek.a", i)]] <- 
          renderTable({
            matrix(
              data = c("Nama Proyek", "Mitra", "Area", "Total Kota", 
                       "Nilai Proyek / Belum BAST-1", "Mulai --- Akhir",
                       "Total Lokasi / On Going / Lag", 
                       "Pre / Del / Ins / TC / UT / ST",
                       ": ", ": ", ": ", ": ", ": ", ": ", ": ", ": ",
                       prioritas.1$NM_PROYEK[i], prioritas.1$NM_MITRA[i], prioritas.1$area[i],
                       prioritas.1$kota[i],
                       paste0(
                         formatC(
                           prioritas.1$nilai[i], big.mark = ".", decimal.mark = ",", 
                           format = "f", drop0trailing = TRUE
                         ), " / ",
                         formatC(
                           prioritas.1$blm.bast[i], big.mark = ".", decimal.mark = ",", 
                           format = "f", drop0trailing = TRUE
                         )
                       ),
                       paste0(prioritas.1$TG_MULAI[i], " --- ", prioritas.1$TG_AKHIR[i]), 
                       paste0(
                         prioritas.1$lokasi[i], " / ",
                         prioritas.1$lokasi.og[i], " / ", prioritas.1$lokasi.lag[i]
                       ),
                       paste(prioritas.1[i, 18:23], collapse = " / ")
              ),
              nrow = 8,
              ncol = 3
            )
          }, colnames = FALSE, spacing = "xs"
          )
        
        # Membuat grafik progres proyek prioritas 1
        output[[paste0("grafik.progres.a", i)]] <- 
          renderPlot({
            prioritas.1 %>% 
              filter(NO_PROYEK == prioritas.1$NO_PROYEK[i]) %>%
              ggplot(aes(x = NO_PROYEK, y = progres)) + 
              scale_y_continuous(breaks = round(prioritas.1$progres[i]), 
                                 sec.axis = dup_axis(breaks = round(prioritas.1$rencana[i]))) +
              scale_x_discrete(labels = "PROGRES") +
              geom_bar(stat = "identity") + expand_limits(y = c(0, 100))  +
              geom_hline(yintercept= prioritas.1$rencana[i], size = 2) + 
              geom_hline(yintercept= 0, size = 0.5) + 
              geom_hline(yintercept= 100, size = 0.5) +
              coord_flip() + theme_bw() +
              theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    panel.border = element_blank(),
                    axis.title = element_blank(), axis.text.y = element_text(size = 10),
                    axis.ticks = element_blank(), axis.text.x = element_text(size = 11)
              )
          })          
        
        # Membuat grafik status proyek prioritas 1
        output[[paste0("grafik.status.a", i)]] <- 
          renderPlot({
            db.proyek %>% 
              filter(NO_PROYEK == prioritas.1$NO_PROYEK[i]) %>% group_by(STATUS) %>% summarise(nilai = sum(NILAI)) %>%
              ggplot(aes(x = STATUS, y = nilai)) + 
              geom_bar(stat = "identity") + 
              geom_text(aes(label = round(nilai/1000000, 0)), hjust = 1.1, vjust = 0.4, color = "#FFFFFF") +
              scale_x_discrete(limits = rev(c("PREPARING", 
                                              "DELIVERY", "INSTALLASI", "TEST COMM", "UJI TERIMA", 
                                              "BAST-1"))) +
              labs(y = "NILAI (juta)") + coord_flip() + theme_bw() +
              theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    panel.border = element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.x = element_blank(), axis.text.y = element_text(size = 10),
                    axis.ticks = element_blank()
              )
          })
      })  
    })    

  # Pop up menu proyek prioritas 2
  lapply(
    1:length(prioritas.2$no.proyek), 
    function(i) {  
      observeEvent(input[[paste0("klikB", i)]], {
        
        # Informasi proyek prioritas 2
        output[[paste0("tabel.proyek", i)]] <- 
          renderTable({
            matrix(
              data = c("Nama Proyek", "Mitra", "Area", "Total Kota", 
                       "Nilai Proyek / Belum BAST-1", "Mulai --- Akhir",
                       "Total Lokasi / On Going / Lag", 
                       "Pre / Del / Ins / TC / UT / ST",
                       ": ", ": ", ": ", ": ", ": ", ": ", ": ", ": ",
                       prioritas.2$NM_PROYEK[i], prioritas.2$NM_MITRA[i], prioritas.2$area[i],
                       prioritas.2$kota[i],
                       paste0(
                         formatC(
                           prioritas.2$nilai[i], big.mark = ".", decimal.mark = ",", 
                           format = "f", drop0trailing = TRUE
                         ), " / ",
                         formatC(
                           prioritas.2$blm.bast[i], big.mark = ".", decimal.mark = ",", 
                           format = "f", drop0trailing = TRUE
                         )
                       ),
                       paste0(prioritas.2$TG_MULAI[i], " --- ", prioritas.2$TG_AKHIR[i]), 
                       paste0(
                         prioritas.2$lokasi[i], " / ",
                         prioritas.2$lokasi.og[i], " / ", prioritas.2$lokasi.lag[i]
                       ),
                       paste(prioritas.2[i, 18:23], collapse = " / ")
              ),
              nrow = 8,
              ncol = 3
            )
          }, colnames = FALSE, spacing = "xs"
          )
        
        # Membuat grafik progres proyek prioritas 2
        output[[paste0("grafik.progres", i)]] <- 
          renderPlot({
            prioritas.2 %>% 
              filter(NO_PROYEK == prioritas.2$NO_PROYEK[i]) %>%
              ggplot(aes(x = NO_PROYEK, y = progres)) + 
              scale_y_continuous(breaks = round(prioritas.2$progres[i]), 
                                 sec.axis = dup_axis(breaks = round(prioritas.2$rencana[i]))) +
              scale_x_discrete(labels = "PROGRES") +
              geom_bar(stat = "identity") + expand_limits(y = c(0, 100))  +
              geom_hline(yintercept= prioritas.2$rencana[i], size = 2) + 
              geom_hline(yintercept= 0, size = 0.5) + 
              geom_hline(yintercept= 100, size = 0.5) +
              coord_flip() + theme_bw() +
              theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    panel.border = element_blank(),
                    axis.title = element_blank(), axis.text.y = element_text(size = 10),
                    axis.ticks = element_blank(), axis.text.x = element_text(size = 11)
              )
          })          
        
        # Membuat grafik status proyek prioritas 2
        output[[paste0("grafik.status", i)]] <- 
          renderPlot({
            db.proyek %>% 
              filter(NO_PROYEK == prioritas.2$NO_PROYEK[i]) %>% group_by(STATUS) %>% summarise(nilai = sum(NILAI)) %>%
              ggplot(aes(x = STATUS, y = nilai)) + 
              geom_bar(stat = "identity") + 
              geom_text(aes(label = round(nilai/1000000, 0)), hjust = 1.1, vjust = 0.4, color = "#FFFFFF") +
              scale_x_discrete(limits = rev(c("PREPARING", 
                                              "DELIVERY", "INSTALLASI", "TEST COMM", "UJI TERIMA", 
                                              "BAST-1"))) +
              labs(y = "NILAI (juta)") + coord_flip() + theme_bw() +
              theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    panel.border = element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.x = element_blank(), axis.text.y = element_text(size = 10),
                    axis.ticks = element_blank()
              )
          })
      })  
    })    
  

  # Lokasi rinci untuk proyek prioritas 1
    lapply(
    1:length(prioritas.1$NO_PROYEK),
    function(i) {
      observeEvent(input[[paste0("lokasi.a", i)]], {
       toggleModal(session, modalId = paste0("klikmodalA", i), toggle = "close")
        # Saring lokasi
        lokasi.rinci <- lokasi.rinci %>% 
          filter(NO_PROYEK == prioritas.1$NO_PROYEK[i])
        output$tabel.rinci <-
          renderDT({
            datatable(lokasi.rinci[-9]) %>% 
              formatStyle("Jadwal", target = "row", 
                          backgroundColor = styleEqual("Lag", "khaki")
              )
          })
        output$lokasi <-
          renderUI({
            fluidRow(
              box(
                title = paste0(prioritas.1$NO_PROYEK[i], " -- ",
                               prioritas.1$NM_JENIS[i], " -- ",
                               prioritas.1$NM_MITRA[i]),
                solidHeader = TRUE,
                width = 12,
                status = "primary",
                DTOutput("tabel.rinci")
              )
            )  
          })
      })
    })
   
    # Lokasi rinci untuk proyek prioritas 2
    lapply(
      1:length(prioritas.2$NO_PROYEK),
      function(i) {
        observeEvent(input[[paste0("lokasi.b", i)]], {
          toggleModal(session, modalId = paste0("klikmodalB", i), toggle = "close")
          # Saring lokasi
          lokasi.rinci <- lokasi.rinci %>% 
            filter(NO_PROYEK == prioritas.2$NO_PROYEK[i])
          output$tabel.rinci <-
            renderDT({
              datatable(lokasi.rinci[-9]) %>% 
                formatStyle("Jadwal", target = "row", 
                            backgroundColor = styleEqual("Lag", "khaki")
                )
            })
          output$lokasi <-
            renderUI({
              fluidRow(
                box(
                  title = paste0(prioritas.2$NO_PROYEK[i], " -- ",
                                 prioritas.2$NM_JENIS[i], " -- ",
                                 prioritas.2$NM_MITRA[i]),
                  solidHeader = TRUE,
                  width = 12,
                  status = "primary",
                  DTOutput("tabel.rinci")
                )
              )  
            })
        })
      })
   
    # Pilihan input set data excel
    xls <- 
      reactive({
              switch(input$input.xls,
                     "Sumber set data proyek" = db.proyek,
                     "Daftar proyek" = proyek[c(1:7, 9:11)]
                    )
      })
    
    # Output tabel pilihan set data
    output$tabel.xls <-
     renderDT(datatable(xls()))
    
    # Unduh set data excel
    output$unduh.xls <- downloadHandler(
      filename = function() {
        paste(input$input.xls, ".xlsx", sep = "")
      },
      content = function(file) {
        write.xlsx(xls(), file)
      }
    )
    
    # Pilihan input format laporan
    format.laporan <- 
      reactive({
        switch(input$input.laporan,
               "power point" = "pptx",
               "pdf" = "pdf"
        )
      })
    
    # Unduh laporan proyek
    output$laporan <- downloadHandler(
      filename = function() {
        paste("laporan_proyek.", format.laporan(), sep = "")
        },
      content = function(file) {
        tempReport <- file.path(tempdir(), paste0(format.laporan(), ".Rmd"))
        file.copy(paste0(format.laporan(), ".Rmd"), tempReport, overwrite = TRUE)
        
        tempData <- file.path(tempdir(), "proyek.rds")
        file.copy("proyek.rds", tempData, overwrite = TRUE)
      
        rmarkdown::render(tempReport, output_file = file,
                          envir = new.env(parent = globalenv()))
        }
    )
    
}