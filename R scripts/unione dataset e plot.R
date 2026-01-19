# Aggiungi la colonna "Città" per ciascun dataset
novara_resistenze$Città <- "Novara"
torino_resistenze$Città <- "Torino"

# Unisci i due dataset
combined_resistenze <- rbind(novara_resistenze, torino_resistenze)

# Visualizza i primi dati del dataset combinato per controllare
head(combined_resistenze)

# ---- Definire le famiglie di antibiotici in base ai prefissi ----

# Creare un vettore di associazione tra prefissi e famiglie
antibiotic_families <- list(
  "beta-lattamici" = c("blaOXA-347", "blaOXA-392","blaOXA-1", "blaOXA-10", "blaVEB-1", "blaVEB-3","cfxA", "cfxA3", "cfxA6", "cfxA5","blaOXA-464", "blaOXA-2", "blaOXA-17", "blaOXA-4", "cfxA4"),
  "aminoglicosidi" = c("ant(3'')-Ia", "aph(6)-Id", "aph(3'')-Ib", "aadA1", "aadA2", "aadA5"),
  "macrolidi" = c("erm(B)", "erm(F)", "msr(E)", "mph(E)", "mph(N)", "mph(G)", "mef(B)", "lnu(C)", "mef(C)", "ere(D)"),
  "sulfonamidi" = c("sul1"),
  "tetracicline" = c("tet(X)", "tet(C)", "tet(Q)", "tet(A)", "tet(39)"),
  "fluorochinoloni" = c("qnrS2", "qnrD1"),
  "rifampicine" = c("ARR-8"),
  "fenicoli" = c("cat")
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
combined_resistenze$Antibiotic_Family <- sapply(combined_resistenze$Header, assign_family)

# Visualizzare il dataset aggiornato
head(combined_resistenze)

combined_resistenze$anno<-"2024"


# ---- Contare il numero totale di geni per ciascuna città ----

# Numero totale di geni trovati a Torino
num_geni_torino <- nrow(subset(combined_resistenze, Città == "Torino"))
cat("Numero totale di geni trovati a Torino:", num_geni_torino, "\n")

# Numero totale di geni trovati a Novara
num_geni_novara <- nrow(subset(combined_resistenze, Città == "Novara"))
cat("Numero totale di geni trovati a Novara:", num_geni_novara, "\n")

# ---- Creare i barplot per Torino e Novara in base ai mesi ----

# Barplot per Torino (conteggio per mese)
torino_counts <- table(subset(combined_resistenze, Città == "Torino")$Date)
barplot(torino_counts, main="Geni di Resistenza a Torino per Mese", 
        xlab="Mese", ylab="Numero di Geni", col="lightblue", las=2)

july_data <- nrow(subset(torino_resistenze, Date == "luglio"))
other_months_data <- nrow(subset(torino_resistenze, Date != "luglio"))

# Test di Wilcoxon (non parametrico) per confrontare Luglio con gli altri mesi
wilcox.test(july_data, other_months_data) #non è significativo o ci sono troppi pochi dati

# Barplot per Novara (conteggio per mese)
novara_counts <- table(subset(combined_resistenze, Città == "Novara")$Date)
barplot(novara_counts, main="Geni di Resistenza a Novara per Mese", 
        xlab="Mese", ylab="Numero di Geni", col="lightgreen", las=2, ylim=c(0,20))

# ---- Creare le pie chart per la distribuzione totale dei geni ----

# Pie chart per Torino (distribuzione dei geni)
torino_geni <- table(subset(combined_resistenze, Città == "Torino")$Header)
pie(torino_geni, main="Distribuzione dei Geni di Resistenza a Torino", 
    col=rainbow(length(torino_geni)))

# Pie chart per Novara (distribuzione dei geni)
novara_geni <- table(subset(combined_resistenze, Città == "Novara")$Header)
pie(novara_geni, main="Distribuzione dei Geni di Resistenza a Novara", 
    col=rainbow(length(novara_geni)))

# ---- Creare i bar plot per la distribuzione totale dei geni ----

par(mar = c(5, 6, 4, 2))

# Barplot per Torino (distribuzione dei geni)
torino_geni <- table(subset(combined_resistenze, Città == "Torino")$Header)
barplot(torino_geni, main="Distribuzione dei Geni di Resistenza a Torino", 
        xlab="Numero di Geni", ylab="Geni", col="lightblue", las=2, horiz=TRUE)

# Barplot per Novara (distribuzione dei geni)
novara_geni <- table(subset(combined_resistenze, Città == "Novara")$Header)
barplot(novara_geni, main="Distribuzione dei Geni di Resistenza a Novara", 
        xlab="Numero di Geni", ylab="Geni", col="lightgreen", las=2, horiz=TRUE)


# ---- Creare il barplot impilato per confrontare Torino e Novara ----

# Contare i geni per ciascuna città
torino_geni <- table(subset(combined_resistenze, Città == "Torino")$Header)
novara_geni <- table(subset(combined_resistenze, Città == "Novara")$Header)

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
              main = "Distribuzione dei Geni di Resistenza tra Torino e Novara",
              xlab = "Geni", ylab = "Numero di Geni", las = 2, cex.names = 0.7, 
              names.arg = gene_counts$Gene, ylim = c(0, max_geni))

# Creare i tick dell'asse y solo per i valori dispari o scelti
y_ticks <- seq(0, max_geni, by = 1)  # Tick di 1 in 1

# Aggiungere l'asse y con i valori selezionati
axis(2, at = y_ticks, labels = y_labels)

# ---- Grafico a torta per Torino ----

# Contare i geni per Torino
torino_frequencies <- table(subset(combined_resistenze, Città == "Torino")$Header)

# Definire una soglia di frequenza 
soglia <- 2

# Filtrare i geni che appaiono almeno 'soglia' volte
torino_comuni <- torino_frequencies[torino_frequencies > soglia]

# Sommare le frequenze dei geni meno comuni (meno di 'soglia')
torino_meno_comuni <- torino_frequencies[torino_frequencies < soglia]
torino_frequenza_altro <- sum(torino_meno_comuni)

# Creare un nuovo vettore combinando i geni più comuni e la categoria "Altro"
torino_pie_data <- c(torino_comuni, Altro = torino_frequenza_altro)

# Creare il grafico a torta per Torino
pie(torino_pie_data, main = "Distribuzione dei Geni di Resistenza a Torino", 
    col = rainbow(length(torino_pie_data)), labels = names(torino_pie_data))


# ---- Grafico a torta per Novara ----

# Contare i geni per Novara
novara_frequencies <- table(subset(combined_resistenze, Città == "Novara")$Header)

# Filtrare i geni che appaiono almeno 'soglia' volte
novara_comuni <- novara_frequencies[novara_frequencies > soglia]

# Sommare le frequenze dei geni meno comuni (meno di 'soglia')
novara_meno_comuni <- novara_frequencies[novara_frequencies < soglia]
novara_frequenza_altro <- sum(novara_meno_comuni)

# Creare un nuovo vettore combinando i geni più comuni e la categoria "Altro"
novara_pie_data <- c(novara_comuni, Altro = novara_frequenza_altro)

# Creare il grafico a torta per Novara
pie(novara_pie_data, main = "Distribuzione dei Geni di Resistenza a Novara", 
    col = rainbow(length(novara_pie_data)), labels = names(novara_pie_data))

# ---- Contare la frequenza delle famiglie di antibiotici per Torino e Novara ----

# Frequenza delle famiglie per Torino
torino_famiglie_freq <- table(subset(combined_resistenze, Città == "Torino")$Antibiotic_Family)

# Frequenza delle famiglie per Novara
novara_famiglie_freq <- table(subset(combined_resistenze, Città == "Novara")$Antibiotic_Family)

# Frequenza delle famiglie complessive
tutte_famiglie_freq <- table(combined_resistenze$Antibiotic_Family)

# ---- Creare i pie chart ----

# Pie chart per Torino
pie(torino_famiglie_freq, main = "Frequenza delle Famiglie di Antibiotici a Torino",
    col = rainbow(length(torino_famiglie_freq)), labels = names(torino_famiglie_freq))

# Pie chart per Novara
pie(novara_famiglie_freq, main = "Frequenza delle Famiglie di Antibiotici a Novara",
    col = rainbow(length(novara_famiglie_freq)), labels = names(novara_famiglie_freq))

# Pie chart complessivo
pie(tutte_famiglie_freq, main = "Frequenza Totale delle Famiglie di Antibiotici",
    col = rainbow(length(tutte_famiglie_freq)), labels = names(tutte_famiglie_freq))


# ---- Creare un barplot impilato per Torino e Novara ----

par(mar = c(9, 6, 4, 2))

# Creare un dataset per il barplot
barplot_data <- as.data.frame.matrix(table(combined_resistenze$Città, combined_resistenze$Antibiotic_Family))

# Creare il barplot impilato
barplot(as.matrix(barplot_data), beside = FALSE, col = rainbow(ncol(barplot_data)),
        main = "Distribuzione delle Famiglie di Antibiotici a Torino e Novara",
        xlab = "Famiglie di Antibiotici", ylab = "Frequenza",
        legend = rownames(barplot_data), las = 2)

# ----------- Geni come blocchi impilati -----------

# Generare una lista di colori casuale escludendo i grigi
set.seed(123)  # Per rendere i colori ripetibili

# Lista di tutti i colori disponibili in R
all_colors <- colors()

# Escludere i colori che contengono "grey" o "gray"
filtered_colors <- all_colors[!grepl("grey|gray", all_colors)]

# Prendere n_genes colori casuali dalla lista filtrata
n_genes <- length(unique(combined_resistenze$Header))  # Numero di geni unici
random_colors <- sample(filtered_colors, n_genes)

# Ristrutturare i dati per ottenere le presenze dei geni per ogni mese e città
library(ggplot2)
library(dplyr)

# Creare una colonna combinata "Città-Mese" per avere Torino e Novara separati
blocchi_resistenze <- combined_resistenze %>%
  mutate(Città_Date = paste(Città, Date, sep = "-"))

par(mar = c(9, 6, 8, 2))

# Creare un grafico a barre impilate
ggplot(blocchi_resistenze, aes(x = Città_Date, fill = Header)) +
  geom_bar(stat = "identity", aes(y = Header), position = "stack") +
  

  # Rimuovere l'asse Y
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  
  # Impostare i colori per ciascun gene in palette univoca
  scale_fill_manual(values = setNames(random_colors, unique(blocchi_resistenze$Header))) +
  
  # Aggiungere la legenda sotto il grafico
  theme(legend.position = "bottom", legend.title = element_blank()) +
  
  # Titolo del grafico
  labs(title = "Distribuzione Geni AMR", x = "Mesi (Torino e Novara)") +
  
  # Etichette delle X in obliquo
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(aspect.ratio = 0.7)

