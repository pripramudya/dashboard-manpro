library(shinydashboard)
library(openxlsx)
library(tidyverse)

# Membaca file daftar_proyek.xlsx
db.proyek <- read.xlsx("data/daftar_proyek.xlsx")

# Membuat kolom prioritas proyek
db.proyek <- 
  db.proyek %>%
  mutate(GRUP = ifelse(NO_PROYEK %in% c("KT-6632/2019",
                                        "KT-3259/2019", 
                                        "KT-3703/3017", 
                                        "KT-2155/3518"), 
                       "PRIORITAS 1", "PRIORITAS 2")
         )

# Membuat kolom Lag dan Lead
db.proyek <- db.proyek %>%
  mutate(JADWAL = if_else(PLAN > REAL, "Lag", "Lead"))

# Mengubah format tanggal mulai dan akhir proyek
db.proyek$TG_MULAI <- substr(db.proyek$TG_MULAI, 1, 10)
db.proyek$TG_AKHIR <- substr(db.proyek$TG_AKHIR, 1, 10)
db.proyek$TG_MULAI <- format(as.Date(db.proyek$TG_MULAI), "%d-%b-%y")

# Membuat ringkasan status proyek
status.proyek <- db.proyek %>% 
  group_by(NO_PROYEK) %>% 
  summarise(progres = mean(REAL), rencana = mean(PLAN), 
            nilai = sum(NILAI), blm.bast = sum(NILAI[STATUS != "BAST-1"]),
            lokasi = n(), lokasi.og = length(NO_PROYEK[STATUS != "BAST-1"]),
            lokasi.lag = length(NO_PROYEK[REAL < PLAN]),
            kota = n_distinct(NM_KOTA),
            area = paste(sort(unique(AREA)), collapse = ", "), 
            loc.pre = length(NO_PROYEK[STATUS == "PREPARING"]), 
            loc.del = length(NO_PROYEK[STATUS == "DELIVERY"]), 
            loc.ins = length(NO_PROYEK[STATUS == "INSTALLASI"]), 
            loc.tc  = length(NO_PROYEK[STATUS == "TEST COMM"]), 
            loc.ut  = length(NO_PROYEK[STATUS == "UJI TERIMA"]), 
            loc.bas = length(NO_PROYEK[STATUS == "BAST-1"])
            ) %>% 
  arrange(progres)

proyek <- db.proyek %>% 
  distinct(NO_PROYEK, .keep_all = TRUE) %>% 
  select(NO_PROYEK, NM_JENIS, NM_PROYEK, NM_MITRA, GRUP, TG_MULAI, TG_AKHIR, REAL)

# Menambahkan kolom jadwal berisi 3 warna status proyek
status.proyek <- 
  status.proyek %>% 
  mutate(no.proyek = substr(NO_PROYEK, 4, regexpr("/", NO_PROYEK)-1), 
         jadwal = if_else(progres - rencana >= 0, "MediumSeaGreen",
                  if_else(progres - rencana >= -10, "goldenrod", "crimson"
                  ))
  )

# Membuat daftar proyek
proyek <- left_join(proyek, status.proyek, by = "NO_PROYEK")

# Menandai proyek telat
proyek <- proyek %>% mutate(telat = if_else(as.Date(TG_AKHIR) < Sys.Date(), "*", ""))

# Mengubah format tanggal akhir
proyek$TG_AKHIR <- format(as.Date(proyek$TG_AKHIR), "%d-%b-%y")

# Mengubah tipe variabel nama mitra
proyek$NM_MITRA <- as.character(proyek$NM_MITRA)

# Membuat daftar proyek ongoing
ongoing <- proyek %>%
           filter(progres < 100)

# Total nilai belum BAST
lokasi.blmbast <- db.proyek %>% filter(STATUS  != "BAST-1")
blmbast <- sum(lokasi.blmbast$NILAI)

# Jumlah proyek lag
proyek.lag <- length(ongoing$NO_PROYEK[ongoing$progres < ongoing$rencana])
proyek.telat <- length(ongoing$NO_PROYEK[ongoing$telat == "*"])
lokasi.lag <- length(lokasi.blmbast$NM_LOKASI[lokasi.blmbast$REAL < lokasi.blmbast$PLAN])
lokasi.lag <- formatC(lokasi.lag, big.mark = ".", 
                      decimal.mark = ",", format = "f", drop0trailing = TRUE) 

# Membagi proyek berdasarkan grup prioritas
prioritas.1 <- ongoing %>% filter(GRUP == "PRIORITAS 1") %>% arrange(progres)
prioritas.2 <- ongoing %>% filter(GRUP == "PRIORITAS 2") %>% arrange(progres)

# Membuat variabel lokasi rinci
lokasi.rinci <- db.proyek %>% 
  select(AREA, NM_KOTA, NM_LOKASI, NILAI, PLAN, REAL, JADWAL, STATUS, NO_PROYEK)

# Mengubah format nilai pada set data lokasi rinci
lokasi.rinci$NILAI <- formatC(
  lokasi.rinci$NILAI, big.mark = ".", decimal.mark = ",", 
  format = "f", drop0trailing = TRUE)

# Mengubah nama kolom pada set data lokasi rinci
colnames(lokasi.rinci)[2] <- "Kota"
colnames(lokasi.rinci)[3] <- "Lokasi"
colnames(lokasi.rinci)[1] <- "Area"
colnames(lokasi.rinci)[4] <- "Nilai"
colnames(lokasi.rinci)[5] <- "Plan (%)"
colnames(lokasi.rinci)[6] <- "Real (%)"
colnames(lokasi.rinci)[7] <- "Jadwal"
colnames(lokasi.rinci)[8] <- "Status"

# Membuat ikon deviasi
ikon <- if_else(mean(ongoing$progres) >= mean(ongoing$rencana), "plus", "minus")

# Mengubah format deviasi proyek
deviasi <- abs(mean(ongoing$progres) - mean(ongoing$rencana))
deviasi <- paste0(formatC(deviasi, big.mark = ".", 
                                     decimal.mark = ",", format = "f", digits = 2), "%")

# Simpan data proyek
saveRDS(proyek, "proyek.rds")
