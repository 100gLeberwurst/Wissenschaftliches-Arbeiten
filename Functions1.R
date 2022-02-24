#Hier die Funktionen zur grafischen Auswertung

# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# für metrische Variablen berechnet und ausgibt

metric <- function(var){
  print(summary(var))
}
#Test
test1 <- rnorm(200, 23,2)
metric(test1)



# b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# für kategoriale Variablen berechnet und ausgibt

kateg <- function(var){
  cat("Haeufigkeiten\n")
  print(table(var))
  cat("\nModus: ", names(which(table(test)==max(table(test)))), "\n")
}
#Test
test2 <- sample(c("A", "B", "C", "D"), 200, rep=T)
kateg(test)



# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken für
# den Zusammenhang zwischen zwei kategorialen Variablen
# berechnet ausgibt

bivKateg <- function(){
  
}



# (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken für
# den Zusammengang zwischen einer metrischen und einer
# dichotomen Variablen berechnet und ausgibt

bivMetDicho <- function(){
  
}



# (e) Eine Funktion, die eine mindestens ordinal skalierte Variable
# quantilbasiert kategorisiert (z.B. in „niedrig“, „mittel“, „hoch“)

quantKateg <- function(){
  
}



# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier
# kategorialen Variablen erstellt

visualize <- function(){
  
}



# Freiwillig: weitere zur Deskription und Visualisierung geeignete
# Funktionen