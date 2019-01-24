library(shinydashboard)
library(shinyBS)
library(DT)

dashboardPage( 
  
  # Judul dashboard
  dashboardHeader(title = HTML(paste(icon('cubes'),'proyekABADI'))),
  
  # Menu dashboard
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Ringkasan Proyek", tabName = "ringkasan", icon = icon("dashboard")),
      menuItem("Laporan", icon = icon("list"),
        menuSubItem("xls", tabName = "excel"),
        menuSubItem("ppt - pdf", tabName = "powerpoint")
      )
    )
  ),
  
  # Isi dashboard
  dashboardBody(
    # Kumpulan menu
    tabItems(
      # Mulai menu ringkasan proyek
      tabItem(
        tabName = "ringkasan",
        fluidRow(
          valueBox(
            value = length(ongoing$NO_PROYEK),
            subtitle = "proyek on going", 
            icon = icon("tasks"), width = 3
          ),
          valueBox(
            value = formatC(length(lokasi.blmbast$NM_LOKASI), big.mark = ".", 
                    decimal.mark = ",", format = "f", drop0trailing = TRUE),
            subtitle = "lokasi on going", 
            icon = icon("map-marker"), width = 3
          ),
          valueBox(
            value = paste0(formatC(mean(ongoing$progres), big.mark = ".", 
                            decimal.mark = ",", format = "f", digits = 2), "%"),
            subtitle = " progres on going", 
            icon = icon("line-chart"), width = 3
          ),
          valueBox(
            value = paste0(formatC(blmbast/1000000000, big.mark = ".", 
                            decimal.mark = ",", format = "f", digits = 2), "M"), 
            subtitle = "nilai on going", 
            icon = icon("money"), width = 3
          ),
          box(HTML(paste0(icon('level-down'), " ", proyek.lag, " proyek lag")), 
          style = "font-size : 12pt; text-align : center",
              background = "yellow", width = 3
          ),
          box(HTML(paste0(icon('toggle-down'), " ", lokasi.lag, " lokasi lag")), 
              style = "font-size : 12pt; text-align : center",
              background = "yellow", width = 3
          ),
          box(HTML(paste0(icon(ikon), " ", deviasi, " deviasi progres")), 
              style = "font-size : 12pt; text-align : center",
              background = "yellow", width = 3
          ),
          box(HTML(paste0(icon('exclamation-triangle'), " ", proyek.telat, " proyek masa laku habis")), 
              style = "font-size : 12pt; text-align : center",
              background = "yellow", width = 3
          ),
          
          # Kotak prioritas 1
          box(
            title = paste0("PRIORITAS 1 -- ", length(prioritas.1$no.proyek), " on going / ",
                           length(prioritas.1$NO_PROYEK[prioritas.1$progres < prioritas.1$rencana]),
                           " lag / ", length(prioritas.1$NO_PROYEK[prioritas.1$telat == "*"]), " masa laku habis"
                           ),
            solidHeader = TRUE, 
            lapply(
              1:length(prioritas.1$no.proyek), 
              function(i) {
                actionButton(
                  inputId = paste0("klikA", i), 
                  label = paste0(prioritas.1$no.proyek[i], prioritas.1$telat[i]), 
                  width = "60px", 
                  style = paste0("color : white; font-weight: bold; background-color : ", 
                          prioritas.1$jadwal[i]
                          )
                )
              }
            ),
            width = 6, status = "primary"
          ), 
          
          # Kotak prioritas 2
          box(
            title = paste0("PRIORITAS 2 -- ", length(prioritas.2$no.proyek), " on going / ",
                           length(prioritas.2$NO_PROYEK[prioritas.2$progres < prioritas.2$rencana]),
                           " lag / ", length(prioritas.2$NO_PROYEK[prioritas.2$telat == "*"]), " masa laku habis"
            ),
            solidHeader = TRUE, 
            lapply(
              1:length(prioritas.2$no.proyek), 
              function(i) {
                actionButton(
                  inputId = paste0("klikB", i), 
                  label = paste0(prioritas.2$no.proyek[i], prioritas.2$telat[i]), 
                  width = "60px", 
                  style = paste0("color : white; font-weight: bold; background-color : ", 
                                 prioritas.2$jadwal[i]
                  )
                )
              }
            ),
            width = 6, status = "primary"
          ), 
          
          # Kotak informasi proyek prioritas 1
          lapply(
            1:length(prioritas.1$no.proyek),
            function(i) {
              bsModal(
                id = paste0("klikmodalA", i),
                title = prioritas.1$NO_PROYEK[i],
                strong(prioritas.1$NM_JENIS[i]),
                trigger = paste0("klikA", i),
                tableOutput(paste0("tabel.proyek.a", i)),
                plotOutput(paste0("grafik.progres.a", i), height = "60px"),
                plotOutput(paste0("grafik.status.a", i), height = "130px"),
                actionButton(
                  inputId = paste0("lokasi.a", i), 
                  label = "Rincian Lokasi"
                )
              )
              
            }
          ),
          
          # Kotak informasi proyek prioritas 2
          lapply(
            1:length(prioritas.2$no.proyek),
            function(i) {
              bsModal(
                id = paste0("klikmodalB", i),
                title = prioritas.2$NO_PROYEK[i],
                strong(prioritas.2$NM_JENIS[i]),
                trigger = paste0("klikB", i),
                tableOutput(paste0("tabel.proyek", i)),
                plotOutput(paste0("grafik.progres", i), height = "60px"),
                plotOutput(paste0("grafik.status", i), height = "130px"),
                actionButton(
                  inputId = paste0("lokasi.b", i), 
                  label = "Rincian Lokasi"
                )
              )
              
            }
          ),
          
          uiOutput("lokasi")
        )
      ),
      # Akhir dari menu ringkasan proyek
    
      # Menu excel   
    tabItem(tabName = "excel", 
            fluidRow(
              column(3, selectInput('input.xls', label = NULL,
                          choices = c("Sumber set data proyek", "Daftar proyek"))
              ),
              column(3, downloadButton("unduh.xls", "Unduh"))
              ),
              DTOutput("tabel.xls")
            ),
      # Akhir menu excel
    
    # Menu powerpoint   
    tabItem(tabName = "powerpoint", 
            sidebarPanel(
              radioButtons("input.laporan", "Pilih format laporan:",
                           choices = c("power point", "pdf")
              ),
              downloadButton("laporan", "Unduh Laporan")
            )
    )
    # Akhir menu powerpoint
    )
  )
)