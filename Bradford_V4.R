# BRADFORD ASSAY

# 0. Voorbereidend werk ----
# 1. Download het bestand van Canvas
# 2. Sla een kopie van het bestand op, als -> CSV (Comma delimited) ( .csv) 
# 3. Zet het in je werk map. bv: Bradford.csv

# VERVANG:
# Bij setwd() -> C:/Users/... -> door je eigen workingdirectory
# Bij read.csv() -> Bradford.csv -> door je eigen bestandsnaam inclusief .csv

# CONTRLEER:
# Pipeteerschema. Deze code houdt aan dat in kolom 1 t/m 8 de BSA-reeks zit van klein naar groot
# en in kolom 9 t/m 13 de volgorde exact: FL2.5, FL10, dTPR2.5, dTPR10, Blanco is\
# en elke rij een duo/student
# Bij 4. of je een outlier moet verwijderen
# Bij bsa_long %>% filter(Student -> 6 studenten? voeg , "F" toe.



# 1. Werkdirectory instellen-----
# In je verkenner, kan je naast de zoekbalk op een andere balk klikken en de work directory kopieren.
# Verander de Backslash in een forwardslash
setwd("C:/Users") 
getwd()


# 1.1 load packages
library(ggplot2)
library(tidyr)
library(dplyr)


# 2. Data inlezen en schoonmaken ----
# tussen de " " zet je de naam van het bestand, let op hoofdletters en spaties etc.
data <- read.csv("Bradford.csv", sep = ";", header = TRUE, skip = 10) # slaat de metadata over.
str(data) # om te controleren hoe R de data ziet, characters of numeriek


# Alle kolommen, behalve de eerste, omzetten naar numeriek
data[,-1] <- lapply(data[,-1], function(x) as.numeric(gsub(",", ".", trimws(x))))

# Kolomnamen aanpassen <- controleer, in het pipeteerschema, of dit ook voor jouw groep geldt!
#
colnames(data)[1:8]  <- c("Student", "0.0", "0.1", "0.2", "0.4", "0.8", "1.5", "2.0") # de BSA-reeks (worden gezien als characters)
colnames(data)[9:13] <- c("FL2.5", "FL10", "dTPR2.5", "dTPR10", "Blanco") # de kolom namen van de andere sampels

# controleer of het werkt
data

# Vector voor de [BSA], gebruik je later
BSA_conc <- c(0.0, 0.1, 0.2, 0.4, 0.8, 1.5, 2.0)


# 3. BSA grafiek met x-as van 0.0ug – 2.0ug -----

# Alleen de BSA-standaarden selecteren
bsa_data <- data[, c(1:8)]  # kolommen X t/m 2.0, als je de BSA verdunningen in welletje 1t/m7 hebt gepipeteerd
bsa_data
# Omzetten naar long format
bsa_long <- pivot_longer(bsa_data,
                         cols = "0.0":"2.0",
                         names_to = "Hoeveelheid",
                         values_to = "Absorbantie")
names(bsa_long)

# X-waarden numeriek maken
bsa_long$Hoeveelheid <- as.numeric(bsa_long$Hoeveelheid)
head(bsa_long)

# Filteren op studenten A t/m E, tot en met welke rij ze in de welletje zitten
# 6 studenten? = , "F" toevoegen
bsa_long <- bsa_long %>% filter(Student %in% c("A", "B", "C", "D", "E", "F"))
tail(bsa_long)

# Plot alleen de punten
ggplot(bsa_long, aes(x = Hoeveelheid, y = Absorbantie, colour = Student)) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, 2, by = 0.5),
                     limits = c(0, 2)) +
  labs(title = "BSA standaardpunten per student (A t/m F)",
       x = "BSA concentratie (µg/µl)",
       y = "Gemeten absorbantie (595 nm)") +
  theme_minimal()


# 4. OPTIONAL: Outlier verwijderen voorbeeld (D2) ----
# Alleen rijen A t/m E (6 studenten? = 5 veranderen naar 6) en kolommen X1 t/m X7 (BSA)
#bsa_data <- data[1:5, 1:7]
#rownames(bsa_data) <- data$X[1:5]
#bsa_data

# Zet de 0.1 waarde van student D (rij 4) op NA
#bsa_data["4", "0.1"] <- NA
#bsa_data


# 5. Grafiek maken per student met regressielijn en R2 waarde er in geprint -----
bsa_data

# Layout voor 5 (2 bij 3) grafieken
par(mfrow=c(2,3), mar=c(4,4,2,1)) # 2 rijen en 3 kolommen met marges c(bottom, left, top, right)

# hou alleen de echte studenten
bsa_data <- bsa_data %>% filter(Student %in% c("A", "B", "C", "D", "E", "F"))
rownames(bsa_data) <- bsa_data$Student

# Vector om R² op te slaan
r2_values <- numeric(nrow(bsa_data))
names(r2_values) <- rownames(bsa_data)


for (i in 1:nrow(bsa_data)) {
  y <- as.numeric(bsa_data[i, 2:8])  # kolommen 0.0 t/m 2.0
  x <- BSA_conc                     # echte BSA concentraties
  
  # Lineaire regressie op echte x-waarden
  fit <- lm(y ~ x)
  
  # R² opslaan
  r2_values[i] <- summary(fit)$r.squared
  
  # Plot
  plot(x, y, type="p", pch=16, col="blue",
       xlab="BSA hoeveelheid (µg)", 
       ylab="Gemeten absorbantie (595 nm)",
       main=paste("Student", rownames(bsa_data)[i]),
       ylim=c(min(bsa_data[,2:8], na.rm=TRUE), max(bsa_data[,2:8], na.rm=TRUE)),
       xaxt="n")
  axis(1, at=seq(0, 2, by=0.5))
  
  # Regressielijn toevoegen
  abline(fit, col="red", lwd=2)
  
  # R² waardes toevoegen 
  x_pos <- max(x)*0.5 # op 50% van de x-as naar rechts
  y_pos <- max(y, na.rm=TRUE)*0.95 # 5% onder de hoogste meetwaarde van die student
  text(x = x_pos, y = y_pos, labels = paste0("R² = ", round(r2_values[i], 3)))
}

# Terug naar normale layout
par(mfrow=c(1,1))

# Bekijk alle R²-waarden
r2_values


# 7. Maak per student een regressiemodel ----

# Maak een lege matrix om de resultaten in te zetten
df <- data.frame(data)
result <- data.frame(Student = df$Student,
                     FL_2.5 = NA,
                     FL_10 = NA,
                     dTPR_2.5 = NA,
                     dTPR_10 = NA,
                     FL_corr = "none",
                     dTPR_corr = "none"
                     )

# Loop over elke student A t/m E = 1:5, 6 studenten = 1:6
for (i in 1:6) {
  
  # BSA waarden van student i
  OD_BSA <- as.numeric(df[i, 2:8])
  model <- lm(OD_BSA ~ BSA_conc)
  
  # onbekende samples
  unknown_OD <- as.numeric(df[i, 9:12])  # FL2.5, FL10, dTPR2.5, dTPR10
  
  OD_min <- min(OD_BSA, na.rm = TRUE)
  OD_max <- max(OD_BSA, na.rm = TRUE)
  
  # standaardberekening: binnen bereik → concentratie, buiten → NA
  predicted <- sapply(unknown_OD, function(od) {
    if (od >= OD_min & od <= OD_max) {
      (od - coef(model)[1]) / coef(model)[2]
    } else {
      NA
    }
  })
  
  # ---- EXTRA REGELS: alleen corrigeren als beide NA zijn ----
  
  ## 1. FL correctie (pos 1 en 2 van predicted)
  FL_vals <- unknown_OD[1:2]        # ruwe OD’s
  FL_pred <- predicted[1:2]         # berekende waarden
  
  if (all(is.na(FL_pred))) {
    
    # alleen corrigeren als beide boven OD_max liggen
    if (all(FL_vals > OD_max, na.rm = TRUE)) {
      
      afstand <- abs(FL_vals - OD_max)
      index_min <- which.min(afstand)  # 1 = FL2.5, 2 = FL10
      
      # corrigeer alleen deze ene
      predicted[index_min] <- (OD_max - coef(model)[1]) / coef(model)[2]
      
      # noteer welke is gecorrigeerd
      result$FL_corr[i] <- ifelse(index_min == 1, "2.5", "10")
    }
  }
  
  ## 2. dTPR correctie (pos 3 en 4 van predicted)
  dTPR_vals <- unknown_OD[3:4]
  dTPR_pred <- predicted[3:4]
  
  if (all(is.na(dTPR_pred))) {
    
    if (all(dTPR_vals > OD_max, na.rm = TRUE)) {
      
      afstand <- abs(dTPR_vals - OD_max)
      index_min <- which.min(afstand) # 1 = dTPR2.5, 2 = dTPR10
      
      # posities 3 en 4 in predicted
      predicted[2 + index_min] <- (OD_max - coef(model)[1]) / coef(model)[2]
      
      # noteer correctie
      result$dTPR_corr[i] <- ifelse(index_min == 1, "2.5", "10")
    }
  }
  
  # opslaan
  result[i, 2:5] <- predicted
}

# Bekijk de resultaten
result #“Er zat ongeveer X µg eiwit in het sample dat in de well is gepipetteerd.”

# 8. Samenvatting van correcties SAMENVATTING VAN CORRECTIES

summary_text <- c()

for (i in 1:6) {
  student <- result$Student[i]
  fl_corr <- result$FL_corr[i]
  dtpr_corr <- result$dTPR_corr[i]
  
  # Start zin
  line <- paste0("Student ", student, ": ")
  parts <- c()
  
  # FL correctie
  if (fl_corr == "none") {
    parts <- c(parts, "FL niet gecorrigeerd")
  } else {
    parts <- c(parts,
               paste0("FL", fl_corr, " gecorrigeerd (lag het dichtst bij de bovengrens van de ijklijn)")
    )
  }
  
  # dTPR correctie
  if (dtpr_corr == "none") {
    parts <- c(parts, "dTPR niet gecorrigeerd")
  } else {
    parts <- c(parts,
               paste0("dTPR", dtpr_corr, " gecorrigeerd (lag het dichtst bij de bovengrens van de ijklijn)")
    )
  }
  
  # Combineer
  line <- paste(line, paste(parts, collapse = "; "), ".")
  summary_text <- c(summary_text, line)
}

# Print samenvatting
cat(summary_text, sep = "\n")


# 9. Concentratie berekenen (ug/ul) ----

# Corrigeren voor hoeveel volume je van dat monster hebt gepipetteerd in de well.
# Bereken concentraties (µg/µl) i.p.v. totale hoeveelheid (µg)
# Valt de OD waarde van de onbekende samples buiten de ijklijn, dan vult die NA in.
result$FL_2.5   <- result$FL_2.5   / 2.5
result$FL_10    <- result$FL_10    / 10
result$dTPR_2.5 <- result$dTPR_2.5 / 2.5
result$dTPR_10  <- result$dTPR_10  / 10

# Verwijder de correctie-informatie uit het zicht
result <- result[, !(names(result) %in% c("FL_corr", "dTPR_corr"))]

result


