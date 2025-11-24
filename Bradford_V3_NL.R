# 0. Voorbereidend werk ----
# 1. Download het bestand van Canvas
# 2. Zet het in je betreffende map
# 3. Open het bestand in excel
# 4. Verwijder rij 1 t/m 9
# 5. Sla een kopie van het bestand op, als -> CSV (Comma delimited) ( .csv) 
#    in je werk map. bv: Groep4_Bradford.csv


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
data <- read.csv("Groep4_bradford.csv", sep = ";", header = TRUE) 
str(data) # om te controleren hoe R de data ziet, characters of numeriek


# Alle kolommen behalve de eerste omzetten naar numeriek
data[,-1] <- lapply(data[,-1], function(x) as.numeric(gsub(",", ".", trimws(x))))

# Kolomnamen aanpassen <- controleer, in het pipeteerschema, of dit ook voor jouw groep geldt!
colnames(data)[1:8]  <- c("Student", "0.0", "0.1", "0.2", "0.4", "0.8", "1.5", "2.0") # de BSA verdunningen (worden gezien als characters)
colnames(data)[9:13] <- c("FL2.5", "FL10", "dTPR2.5", "dTPR10", "Blanco") # de kolom namen van de andere sampels

# controleer of het werkt
data

# Vector voor de [BSA], gebruik je later
BSA_conc <- c(0.0, 0.1, 0.2, 0.4, 0.8, 1.5, 2.0)


# 3. BSA grafiek met x-as van 0.0ug – 2.0ug -----

# Alleen de BSA-standaarden selecteren
bsa_data <- data[, c(1:8)]  # kolommen X t/m 2.0, als je de BSA verdunningen in welletje 1t/m7 hebt gepipeteerd

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
bsa_long <- bsa_long %>% filter(Student %in% c("A", "B", "C", "D", "E"))
#deze gaat nog niet helemaal lekker

# Plot alleen de punten
ggplot(bsa_long, aes(x = Hoeveelheid, y = Absorbantie, colour = Student)) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, 2, by = 0.5),
                     limits = c(0, 2)) +
  labs(title = "BSA standaardpunten per student (A t/m E)",
       x = "BSA concentratie (µg/µl)",
       y = "Gemeten absorbantie (595 nm)") +
  theme_minimal()


# 4. Outlier verwijderen voorbeeld (D2) ----

# Alleen rijen A t/m E en kolommen X1 t/m X7
bsa_data <- data[1:5, 2:8]
rownames(bsa_data) <- data$X[1:5]
bsa_data

# Zet de 0.1 waarde van student D (rij 4) op NA
bsa_data["4", "0.1"] <- NA
bsa_data


# 5. Grafiek maken per student met regressielijn en R2 waarde er in geprint -----

# Layout voor 5 grafieken
par(mfrow=c(2,3), mar=c(4,4,2,1))

# Vector om R² op te slaan
r2_values <- numeric(nrow(bsa_data))
names(r2_values) <- rownames(bsa_data)

for (i in 1:nrow(bsa_data)) {
  y <- as.numeric(bsa_data[i, 1:7])  # kolommen 0.0 t/m 2.0
  x <- BSA_conc                        # echte BSA concentraties
  
  # Lineaire regressie op echte x-waarden
  fit <- lm(y ~ x)
  
  # R² opslaan
  r2_values[i] <- summary(fit)$r.squared
  
  # Plot
  plot(x, y, type="p", pch=16, col="blue",
       xlab="BSA hoeveelheid (µg)", 
       ylab="Gemeten absorbantie (595 nm)",
       main=paste("Student", rownames(bsa_data)[i]),
       ylim=c(min(bsa_data[,1:7], na.rm=TRUE), max(bsa_data[,1:7], na.rm=TRUE)),
       xaxt="n")
  axis(1, at=seq(0, 2, by=0.5))
  
  # Regressielijn toevoegen
  abline(fit, col="red", lwd=2)
  
  # R² toevoegen 
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
df <- data.frame(data) # volgens mij is deze stap onnodig, want "data" is al een data frame
result <- data.frame(Student = df$Student,
                     FL_2.5 = NA,
                     FL_10 = NA,
                     dTPR_2.5 = NA,
                     dTPR_10 = NA)

# Loop over elke student A t/m E = 1:5
for (i in 1:5) {
  
  # OD waarden van BSA-standaarden
  OD_BSA <- as.numeric(df[i, 2:8])   # kolommen 0.0:0.2, zo heeft elke student zijn eigen ijklijn
  
  # Maak lineaire regressiemodel: OD = a + b * concentratie
  model <- lm(OD_BSA ~ BSA_conc) #(OD ~ Concen) "Hoe hangen de OD-waarde samen met de bekende BSA-concentraties"
      # De relatie OD = a + b × concentratie berekend
      # Dit model berekent dus de rechte lijn (intercept + helling).
  
  # Onbekende sampels de OD waarden
  unknown_Sample_OD <- as.numeric(df[i, 9:12])  # kolommen FL_2.5:dTPR_10
  
  # Bepaal het bereik van de BSA-OD's
  OD_min <- min(OD_BSA, na.rm = TRUE)
  OD_max <- max(OD_BSA, na.rm = TRUE)
  
  # Controleer of elke OD binnen de ijklijn valt; anders NA
  predicted_conc <- sapply(unknown_Sample_OD, function(od) {
    if (od >= OD_min & od <= OD_max) {
      (od - coef(model)[1]) / coef(model)[2]  # bereken concentratie, trekt de intercept (coef(model)[1]) van de gemeten OD af = achtergrond correctie.
    } else {
      NA  # buiten bereik, niet gebruiken
    }
  })
  # De lijn is: OD = intercept + slope × concentratie
  # concentratie = (OD – intercept) / slope
  #coef(model)[1] = intercept (a) -> a = coef(model)[1] → dit is de y-intercept, oftewel de OD wanneer de BSA-concentratie nul is.
  #coef(model)[2] =  helling/slope (b)
  # dit herschrikt de functie y=ax+b, zodat je x kan berekenen, de eiwithoeveelheid
  
  
  # Sla op
  result[i, 2:5] <- predicted_conc
}

# Bekijk de resultaten
result #“Er zat ongeveer X µg eiwit in het sample dat in de well is gepipetteerd.”

# 8. concentratie berekenen (ug/ul) ----

# Corrigeren voor hoeveel volume je van dat monster hebt gepipetteerd in de well.
# Bereken concentraties (µg/µl) i.p.v. totale hoeveelheid (µg)
# Valt de OD waarde van de onbekende samples buiten de ijklijn, dan vult die NA in.
result$FL_2.5   <- result$FL_2.5   / 2.5
result$FL_10    <- result$FL_10    / 10
result$dTPR_2.5 <- result$dTPR_2.5 / 2.5
result$dTPR_10  <- result$dTPR_10  / 10

result

# Stel de OD-waarde 2.5 en 10 van FL/dTPR zijn allebei NA
