# Aggiungi la colonna "Città" per ciascun dataset
novara_2023$Città <- "Novara"
torino_2023$Città <- "Torino"

# Unisci i due dataset
combined_2023 <- rbind(novara_2023, torino_2023)

# Visualizza i primi dati del dataset combinato per controllare
head(combined_2023)

# ---- Definire le famiglie di antibiotici in base ai prefissi ----

# Creare un vettore di associazione tra prefissi e famiglie
antibiotic_families <- list(
  "beta-lattamici" = c("blaOXA-347", "blaOXA-101", "blaOXA-119", "blaOXA-211", "blaOXA-281", "blaOXA-20", "blaOXA-392","blaOXA-1", "blaOXA-10", "blaVEB-1", "blaVEB-3","cfxA", "cfxA3", "cfxA6", "cfxA5","blaOXA-464", "blaOXA-2", "blaOXA-17", "blaOXA-4", "cfxA4"),
  "aminoglicosidi" = c("ant(3'')-Ia", "aac(6')-31", "aph(6)-Id", "aph(3'')-Ib", "aadA1", "aadA2", "aadA5"),
  "macrolidi" = c("erm(B)", "mph(A)", "erm(F)", "msr(E)", "mph(E)", "mph(N)", "mph(G)", "mef(B)", "lnu(C)", "mef(C)", "ere(D)"),
  "sulfonamidi" = c("sul1", "sul2"),
  "tetracicline" = c("tet(X)", "tet(C)", "tet(Q)", "tet(A)", "tet(39)"),
  "fluorochinoloni" = c("qnrS2", "qnrD1", "qnrVC4", "qnrB19"),
  "rifampicine" = c("ARR-8", "ARR-3"),
  "fenicoli" = c("cat"),
  "trimetoprime" = c("dfrB1")
)

# Creare una funzione per assegnare la famiglia di antibiotici
assign_family <- function(gene) {
  for (family in names(antibiotic_families)) {
    if (gene %in% antibiotic_families[[family]]) {
      return(family)
    }
  }
  return(NA)  # Restituisce NA se non trova una corrispondenza
}

# Applicare la funzione alla colonna Header del dataset combined_resistenze
combined_2023$Antibiotic_Family <- sapply(combined_2023$Header, assign_family)

# Visualizzare il dataset aggiornato
head(combined_2023)

combined_2023$anno<-"2023"

#unione entrambe le combinate
combinazione <- rbind(combined_2023, combined_resistenze)

# ---- Contare il numero totale di geni per ciascuna città----

# Numero totale di geni trovati a Torino
num_2023_torino <- nrow(subset(combined_2023, Città == "Torino"))
cat("Numero totale di geni trovati a Torino:", num_2023_torino, "\n")

# Numero totale di geni trovati a Novara
num_2023_novara <- nrow(subset(combined_2023, Città == "Novara"))
cat("Numero totale di geni trovati a Novara:", num_2023_novara, "\n")

# ---- Creare i barplot per Torino e Novara in base ai mesi ----

par(mar = c(7, 6, 4, 2))

torino_counts_2023 <- table(subset(combined_2023, Città == "Torino")$Date)

# Specifica l'ordine corretto dei mesi
ordine_mesi <- c("ottobre 2023", "novembre 2023", "dicembre 2023")

# Riordina i dati in base a questo ordine
torino_counts_2023 <- torino_counts_2023[ordine_mesi]

# Barplot per Torino (conteggio per mese)

barplot(torino_counts_2023, main="Geni di Resistenza a Torino per Mese", 
        xlab="Mese", ylab="Numero di Geni", col="lightblue", las=2, ylim=c(0,25))


# Barplot per Novara (conteggio per mese)
novara_counts_2023 <- table(subset(combined_2023, Città == "Novara")$Date)
novara_counts_2023 <- novara_counts_2023[ordine_mesi]
barplot(novara_counts_2023, main="Geni di Resistenza a Novara per Mese", 
        xlab="Mese", ylab="Numero di Geni", col="lightgreen", las=2, ylim=c(0,25))

# ---- Creare i barplot per Torino e Novara in base ai mesi ASSIEME----

torino_counts_2324 <- table(subset(combinazione, Città == "Torino")$Date)

# Specifica l'ordine corretto dei mesi
ordine_mesi <- c("ottobre 2023", "novembre 2023", "dicembre 2023", "febbraio", "giugno", "luglio", "settembre")

# Riordina i dati in base a questo ordine
torino_counts_2324 <- torino_counts_2324[ordine_mesi]

# Barplot per Torino (conteggio per mese)

barplot(torino_counts_2324, main="Geni di Resistenza a Torino per Mese", 
        xlab="Mese", ylab="Numero di Geni", col="lightblue", las=2, ylim=c(0,25))

#per novara
novara_counts_2324 <- table(subset(combinazione, Città == "Novara")$Date)

# Specifica l'ordine corretto dei mesi
ordine_mesi <- c("ottobre 2023", "novembre 2023", "dicembre 2023", "febbraio", "giugno", "luglio", "settembre")

# Riordina i dati in base a questo ordine
novara_counts_2324 <- novara_counts_2324[ordine_mesi]

# Barplot per Torino (conteggio per mese)

barplot(novara_counts_2324, main="Geni di Resistenza a Novara per Mese", 
        xlab="Mese", ylab="Numero di Geni", col="lightgreen", las=2, ylim=c(0,25))

# ---- Creare il barplot impilato per confrontare torino nei due anni----

comb_to<-rbind(torino_2023, torino_resistenze)

# Contare i geni per ciascuna città
torino_geni_2023 <- table(subset(comb_to, anno == "2023")$Header)
torino_geni_2024 <- table(subset(comb_to, anno == "2024")$Header)

# Creare un dataframe con i geni e i conteggi per novara e torino
gene_counts_to <- merge(as.data.frame(torino_geni_2023), as.data.frame(torino_geni_2024), 
                        by = "Var1", all = TRUE)

# Rinominare le colonne per chiarezza
colnames(gene_counts_to) <- c("Gene", "torino2023", "torino2024")

# Sostituire i valori NA con 0
gene_counts_to[is.na(gene_counts_to)] <- 0

# Definire il massimo numero di geni per personalizzare ylim
max_geni_to <- max(rowSums(gene_counts_to[, 2:3]))

# Creare il barplot impilato con i nomi dei geni sull'asse X
bp <- barplot(t(as.matrix(gene_counts_to[, 2:3])), beside = FALSE, 
              col = c("blue", "lightblue"), legend = c("2023", "2024"),
              main = "Distribuzione dei Geni di Resistenza a Torino 10/2023 - 09/2024",
              xlab = "Numero di geni", ylab = "Geni", las = 2, cex.names = 0.7, 
              names.arg = gene_counts_to$Gene, horiz=TRUE)

# Creare i tick dell'asse y solo per i valori dispari o scelti
y_ticks <- seq(0, max_geni_to, by = 1)  # Tick di 1 in 1

# Aggiungere l'asse y con i valori selezionati
axis(2, at = y_ticks, labels = y_labels)

# ---- Creare il barplot impilato per confrontare Novara nei due anni----

comb_no<-rbind(novara_2023, novara_resistenze)

# Contare i geni per ciascuna città
novara_geni_2023 <- table(subset(comb_no, anno == "2023")$Header)
novara_geni_2024 <- table(subset(comb_no, anno == "2024")$Header)

# Creare un dataframe con i geni e i conteggi per novara e Novara
gene_counts_no <- merge(as.data.frame(novara_geni_2023), as.data.frame(novara_geni_2024), 
                        by = "Var1", all = TRUE)

# Rinominare le colonne per chiarezza
colnames(gene_counts_no) <- c("Gene", "novara2023", "novara2024")

# Sostituire i valori NA con 0
gene_counts_no[is.na(gene_counts_no)] <- 0

# Definire il massimo numero di geni per personalizzare ylim
max_geni_no <- max(rowSums(gene_counts_no[, 2:3]))

# Creare il barplot impilato con i nomi dei geni sull'asse X
bp <- barplot(t(as.matrix(gene_counts_no[, 2:3])), beside = FALSE, 
              col = c("darkgreen", "lightgreen"), legend = c("2023", "2024"),
              main = "Distribuzione dei Geni di Resistenza a Novara 10/2023 - 09/2024",
              xlab = "Numero di geni", ylab = "Geni", las = 2, cex.names = 0.7, 
              names.arg = gene_counts_no$Gene, horiz=TRUE)

# Creare i tick dell'asse y solo per i valori dispari o scelti
y_ticks <- seq(0, max_geni_no, by = 1)  # Tick di 1 in 1

# Aggiungere l'asse y con i valori selezionati
axis(2, at = y_ticks, labels = y_labels)

# ---- Contare la frequenza delle famiglie di antibiotici per Torino e Novara 2023 ----

# Frequenza delle famiglie per Torino
torino_famiglie_freq <- table(subset(combined_2023, Città == "Torino")$Antibiotic_Family)

# Frequenza delle famiglie per Novara
novara_famiglie_freq <- table(subset(combined_2023, Città == "Novara")$Antibiotic_Family)

# Frequenza delle famiglie complessive
tutte_famiglie_freq <- table(combined_2023$Antibiotic_Family)

# ---- Creare un barplot impilato per Torino e Novara nel 2023----

par(mar = c(9, 6, 4, 2))

# Creare un dataset per il barplot
barplot_data <- as.data.frame.matrix(table(combined_2023$Città, combined_2023$Antibiotic_Family))

# Creare il barplot impilato
barplot(as.matrix(barplot_data), beside = FALSE, col = rainbow(ncol(barplot_data)),
        main = "Distribuzione delle Famiglie di Antibiotici a Torino e Novara nel 2023",
        xlab = "Famiglie di Antibiotici", ylab = "Frequenza",
        legend = rownames(barplot_data), las = 2)

# ---- Creare un barplot impilato per Torino e Novara COMPLESSIVAMENTE----

par(mar = c(7, 6, 4, 2))

# Creare un dataset per il barplot
barplot_data <- as.data.frame.matrix(table(combinazione$Città, combinazione$Antibiotic_Family))

# Creare il barplot impilato
barplot(as.matrix(barplot_data), beside = FALSE, col = rainbow(ncol(barplot_data)),
        main = "Distribuzione delle Famiglie di Antibiotici a Torino e Novara 10/2023 - 09/2024",
        xlab = "Famiglie di Antibiotici", ylab = "Frequenza",
        legend = rownames(barplot_data), las = 2)

#---------------- chi quadro ---------------------

cgeni_no_2024<-c(0,0,1,0,2,0,3,2,3,0,0,1,2,0,4,0,0,4,1,4,0,1,1,0,4,4,1,1,1,1,1,1,3,1,1,1,1,2,3)
cgeni_no_2023<-c(1,1,2,1,2,1,3,1,1,1,1,1,1,1,3,1,1,3,1,3,1,2,1,2,1,2,3,0,0,0,0,0,0,0,0,0,0,0,0)

tabella_contingenza <- rbind(cgeni_no_2023, cgeni_no_2024)
chi_quadro_risultato <- chisq.test(tabella_contingenza)

# Visualizzare il risultato
print(chi_quadro_risultato)

#non ci sono differenze nella distribuzione

library(vegan)

# Calcolare l'indice di Shannon per ciascun anno
shannon_2023 <- diversity(cgeni_no_2023, index = "shannon")
shannon_2024 <- diversity(cgeni_no_2024, index = "shannon")

# Visualizzare i risultati
cat("Indice di Shannon 2023:", shannon_2023, "\n")
cat("Indice di Shannon 2024:", shannon_2024, "\n")

#gli indici di 3 e 3,1 sono abbastanza alti per il numero di geni considerati
#la differenza è molto piccola ma denota un leggero aumento nel 2024 a novara

#---------------- chi quadro ---------------------

cgeni_to_2023<-c(1,1,1,1,1,1,3,2,3,2,3,3,3,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
cgeni_to_2024<-c(1,4,0,1,0,2,4,4,4,4,4,4,3,3,1,1,2,4,2,2,1,1,1,1,2,2,1,1,2,1,2,1,1)

tabella_contingenza <- rbind(cgeni_to_2023, cgeni_to_2024)
chi_quadro_risultato <- chisq.test(tabella_contingenza)

# Visualizzare il risultato
print(chi_quadro_risultato)

#non ci sono differenze nella distribuzione

# Calcolare l'indice di Shannon per ciascun anno
shannon_2023 <- diversity(cgeni_to_2023, index = "shannon")
shannon_2024 <- diversity(cgeni_to_2024, index = "shannon")

# Visualizzare i risultati
cat("Indice di Shannon 2023:", shannon_2023, "\n")
cat("Indice di Shannon 2024:", shannon_2024, "\n")

#gli indici sono maggiori rispetto a novara quindi c'è più diversità
#3,21 e 3,23 sono ancora più simili con una differenza insignificativa

# ----------- Geni come blocchi impilati -----------

# Generare una lista di colori casuale escludendo i grigi
set.seed(123)  # Per rendere i colori ripetibili

# Lista di tutti i colori disponibili in R
all_colors <- colors()

# Escludere i colori che contengono "grey" o "gray"
filtered_colors <- all_colors[!grepl("grey|gray", all_colors)]

# Prendere n_genes colori casuali dalla lista filtrata
n_genes <- length(unique(combinazione$Header))  # Numero di geni unici
random_colors <- sample(filtered_colors, n_genes)

# Ristrutturare i dati per ottenere le presenze dei geni per ogni mese e città
library(ggplot2)
library(dplyr)

# Creare una colonna combinata "Città-Mese" per avere Torino e Novara separati
blocchi_resistenze <- combinazione %>%
  mutate(Città_Date = paste(Città, Date, sep = "-"))

ordinone<-c("Novara-ottobre 2023","Novara-novembre 2023","Novara-dicembre 2023","Novara-febbraio","Novara-giugno","Novara-luglio","Novara-settembre","Torino-ottobre 2023","Torino-novembre 2023","Torino-dicembre 2023","Torino-febbraio","Torino-giugno","Torino-luglio","Torino-settembre")
blocchi_resistenze$Date <- factor(blocchi_resistenze$Date, levels = ordinone)


par(mar = c(9, 6, 8, 2))

# Creare un grafico a barre impilate
ggplot(blocchi_resistenze, aes(x = Città_Date, fill = Header)) +
  geom_bar(stat = "identity", aes(y = Header), position = "stack") +
  
  
  # Rimuovere l'asse Y
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  
  # Impostare i colori per ciascun gene (palette univoca per i geni)
  scale_fill_manual(values = setNames(random_colors, unique(blocchi_resistenze$Header))) +
  
  # Aggiungere la legenda sotto il grafico
  theme(legend.position = "bottom", legend.title = element_blank()) +
  
  # Titolo del grafico
  labs(title = "Distribuzione Geni AMR 10/2023 - 09/2024", x = "Mesi (Torino e Novara)") +
  
  # Etichette delle X in obliquo se necessario
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(aspect.ratio = 0.7)


# ---- Creare i bar plot per la distribuzione totale dei geni ----

par(mar = c(5, 6, 4, 2))

# Barplot per Torino (distribuzione dei geni)
torino_geni <- table(subset(combinazione, Città == "Torino")$Header)
barplot(torino_geni, main="Distribuzione dei Geni di Resistenza a Torino 10/2023 - 09/2024", 
        xlab="Numero di Geni", ylab="Geni", col="lightblue", las=2, horiz=TRUE)

# Barplot per Novara (distribuzione dei geni)
novara_geni <- table(subset(combinazione, Città == "Novara")$Header)
barplot(novara_geni, main="Distribuzione dei Geni di Resistenza a Novara 10/2023 - 09/2024", 
        xlab="Numero di Geni", ylab="Geni", col="lightgreen", las=2, horiz=TRUE)

# ---- Creare il barplot impilato per confrontare Torino e Novara ----

# Creare un dataframe con i geni e i conteggi per Torino e Novara
gene_counts <- merge(as.data.frame(torino_geni), as.data.frame(novara_geni), 
                     by = "Var1", all = TRUE)

# Rinominare le colonne per chiarezza
colnames(gene_counts) <- c("Gene", "Torino", "Novara")

# Sostituire i valori NA con 0
gene_counts[is.na(gene_counts)] <- 0

# Definire il massimo numero di geni per personalizzare ylim
max_geni <- max(rowSums(gene_counts[, 2:3]))

# Creare il barplot impilato con i nomi dei geni sull'asse X
bp <- barplot(t(as.matrix(gene_counts[, 2:3])), beside = FALSE, 
              col = c("lightblue", "lightgreen"), legend = c("Torino", "Novara"),
              main = "Distribuzione dei Geni di Resistenza tra Torino e Novara 10/2023 - 09/2024",
              xlab = "Geni", ylab = "Numero di Geni", las = 2, cex.names = 0.7, 
              names.arg = gene_counts$Gene, ylim = c(0, max_geni))

# Creare i tick dell'asse y solo per i valori dispari o scelti
y_ticks <- seq(0, max_geni, by = 1)  # Tick di 1 in 1

# Aggiungere l'asse y con i valori selezionati
axis(2, at = y_ticks, labels = y_labels)

# ------------------------- famiglie di antibiotici nei mesi per città ------------------
#metto le famiglie che interessano a me
famiglie_interessanti <- c("aminoglicosidi", "beta-lattamici", "macrolidi", "tetracicline")
combinazione_filtrata <- combinazione %>%
  filter(Antibiotic_Family %in% famiglie_interessanti)

combinazione_filtrata$Date <- factor(combinazione_filtrata$Date, levels = ordine_mesi)

#creazione grafico
ggplot(combinazione_filtrata, aes(x = Date, fill = Antibiotic_Family)) +
  geom_bar(position = "dodge") +  # Bar chart con le famiglie affiancate
  facet_wrap(~ Città) +           # Dividere il grafico per città
  labs(title = "Frequenza delle principali famiglie di antibiotici per città", 
       x = "Mesi", 
       y = "Frequenza",
       fill = "Famiglia di Antibiotici") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ruotare le etichette delle date
