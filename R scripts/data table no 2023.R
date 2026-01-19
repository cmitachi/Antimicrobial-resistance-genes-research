#cambia working directory
setwd("C:/Users/cosmin/Desktop/tesi/2023/Novara")

# Carica il pacchetto
library(Biostrings)

# Lista dei file FSA da unire
file_list <- c("N1710 res.fsa", "N1311 res.fsa", "N1812 res.fsa") 

# Lista delle date corrispondenti ai file
dates <- c("ottobre 2023", "novembre 2023", "dicembre 2023")

# Inizializza una lista per raccogliere i dati
all_sequences <- list()

# Ciclo per leggere e raccogliere le sequenze con le date
for (i in seq_along(file_list)) {
  # Leggi il file FSA
  sequences <- readDNAStringSet(file_list[i])
  
  # Rimuovi il suffisso "_codice" dai nomi dei geni (Header)
  cleaned_headers <- gsub("_.*$", "", names(sequences))  # Elimina tutto dopo il "_"
  
  # Estrai le intestazioni, le sequenze e aggiungi la data
  seq_data <- data.frame(
    Header = cleaned_headers, #intestazioni senza suffisso
    Sequence = as.character(sequences),
    Date = dates[i],  # Aggiungi la data corrispondente
    stringsAsFactors = FALSE
  )
  
  # Aggiungi i dati alla lista
  all_sequences[[file_list[i]]] <- seq_data
}

# Combina tutti i data frame in uno solo
novara_2023 <- do.call(rbind, all_sequences)

# Visualizza la tabella combinata
print(novara_2023)

novara_2023$anno<-"2023"
