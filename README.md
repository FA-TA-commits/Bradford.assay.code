# README – Bradford Assay Analyse

 Dit script analyseert een Bradford assay door de BSA standaardreeks
 in te lezen, outliers optioneel te verwijderen, een lineair regressiemodel per student te berekenen, 
 extrapolatie correct toe te passen
 en uiteindelijk de eiwitconcentraties van FL en dTPR samples te bepalen.

## Voorbereiding:
 1. Download het plate reader bestand van Canvas.
 2. Open het in Excel en sla het op als CSV (Comma delimited).
 3. Plaats het CSV bestand in je eigen werkmap.

## Aan te passen in het script:
 - Bij setwd(), regel 25: vul je eigen working directory in.
 - Bij read.csv(), regel 37: vul de exacte bestandsnaam inclusief .csv in.
 - Controleer je pipeteerschema of: kolom 1 t/m 8 de BSA reeks zijn
   van 0.0 tot 2.0 en kolom 9 t/m 13 moeten exact deze volgorde hebben: FL2.5, FL10, dTPR2.5, dTPR10, Blanco.
 - Controleer of het aantal studenten klopt. In dit script worden
   standaard de studenten A t/m F gebruikt. Voeg letters toe of verwijder
   er als jouw groep anders is.
 - Outlier verwijderen: in sectie 4 staat een voorbeeld, dat kun je
   handmatig activeren wanneer nodig.

## Wat het script automatisch doet:
 - Zet comma decimalen om naar punten en converteert alles naar numeric.
 - Maakt een BSA grafiek per student in long format.
 - Maakt regressielijnen met R2 waarden voor elke student.
 - Controleert of OD waarden, van de onbekende samples, binnen de ijklijn vallen.
 - Berekent de concentraties van de onbekende FL en dTPR samples.
 

## Extrapolatieregels:
 - Waarden buiten en onder de ijklijn worden nooit gecorrigeerd en blijven NA.
 - Waarden buiten, maar boven de ijklijn worden alleen gecorrigeerd wanneer beide
   waarden van hetzelfde type (FL2.5 en FL10 of dTPR2.5 en dTPR10)
   buiten de ijklijn vallen.
 - In dat geval wordt alleen de waarde gecorrigeerd die het dichtst bij
   de bovengrens van de ijklijn ligt. De andere blijft NA.
 - Correcties worden tijdelijk opgeslagen in de kolommen FL_corr en
    dTPR_corr.
 - Vervolgens genereert het een automatische tekstsamenvatting voor elke student waarin staat of er een correctie is toegepast en waarom. 

## Output:
De uiteindelijke tabel bevat per student de berekende concentraties van
FL2.5, FL10, dTPR2.5 en dTPR10 in microgram per microliter. 

Dit script is geschikt voor gebruik in onderwijs en onderzoek en helpt
om de analyse van Bradford assays reproduceerbaar en controleerbaar
uit te voeren.

