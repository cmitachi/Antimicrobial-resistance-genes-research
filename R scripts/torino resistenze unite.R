#cambia working directory
setwd("C:/Users/cosmin/Desktop/tesi/torino")

# Carica il pacchetto
library(Biostrings)

# Lista dei file FSA da unire
file_list_to <- c("T2 res.fsa", "T2 r2 res.fsa", "T6 res.fsa", "T7 res.fsa", "T9 res.fsa")  # Sostituisci con i tuoi file

# Lista delle date corrispondenti ai file
dates_to <- c("febbraio 2024", "giugno 2024", "luglio 2024", "settembre 2024")  # Inserisci le date corrispondenti

# Inizializza una lista per raccogliere i dati
all_sequences_to <- list()

# Ciclo per leggere e raccogliere le sequenze con le date
for (i in seq_along(file_list_to)) {
  # Leggi il file FSA
  sequences_to <- readDNAStringSet(file_list_to[i])
  
  # Rimuovi il suffisso "_codice" dai nomi dei geni (Header)
  cleaned_headers_to <- gsub("_.*$", "", names(sequences_to))  # Elimina tutto dopo il "_"
  
  # Estrai le intestazioni, le sequenze e aggiungi la data
  seq_data_to <- data.frame(
    Header = cleaned_headers_to, #intestazioni senza suffisso
    Sequence = as.character(sequences_to),
    Date = dates_to[i],  # Aggiungi la data corrispondente
    stringsAsFactors = FALSE
  )
  
  # Aggiungi i dati alla lista
  all_sequences_to[[file_list_to[i]]] <- seq_data_to
}

# Combina tutti i data frame in uno solo
torino_resistenze <- do.call(rbind, all_sequences_to)

# Visualizza la tabella combinata
print(torino_resistenze)

torino_resistenze$anno<-"2024"

