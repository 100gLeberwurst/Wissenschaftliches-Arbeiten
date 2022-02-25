#Hier die Funktionen zur Auswertung
test <- data.frame("alter"=round(rnorm(100, mean = 25,sd = 2),1),
                   "fach"=factor(sample(c("A", "B", "C"),100, rep=T)),
                   "IntMath" = factor(sample(1:7, 100, rep=T)),
                   "IntPro" = factor(sample(1:7, 100, rep=T)),
                   "MLK" = factor(sample(0:1, 100, rep=T))                   )

# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer metrische Variablen berechnet und ausgibt

metric <- function(v){
  print(summary(v))
  cat("Var:", var(v))
  boxplot(v)
}
#Test
metric(test$alter)



# b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer kategoriale Variablen berechnet und ausgibt

kateg <- function(v){
  #browser()
  #Haeufigkeiten
  cat("Haeufigkeiten","\n")
  print(table(v))
  
  #Modus
  cat("\nModus: ", names(which(table(v)==max(table(v)))), "\n")
  cat("--------------------------", "\n")
  
  #Grafik
  n <- length(levels(v))
  maH <- max(table(v))
  plot.new()
  plot.window(xlim=c(0,n+1), ylim=c(0,maH))
  axis(1, at=0:(n+1), labels = c("",levels(v),""))
  axis(2)
  rect(xleft=(1:n)-0.5, ybottom=0, xright=(1:n)+0.5, ytop=table(v))
  
}
#Test
kateg(test$IntMath)

# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
# den Zusammenhang zwischen zwei kategorialen Variablen
# berechnet ausgibt

bivKateg <- function(v1, v2){
  par(mfrow=c(1,2))
  
  #einzelne Auswertungen
  kateg(v1)
  kateg(v2)
  
  #gemeinsame Haeufigkeiten
  cat("Gemeinsame Haeufigkeiten:", "\n")
  table(test$IntMath, test$IntPro)
  
  #evtl noch Plot der gemeinsamen Haeufigkeiten
}

#Test
bivKateg(test$IntMath, test$MLK)


# (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
# den Zusammengang zwischen einer metrischen und einer
# dichotomen Variablen berechnet und ausgibt

bivMetDicho <- function(met, dic){
  print(boxplot(met ~ dic, plot=FALSE))
  boxplot(met ~ dic)
}
bivMetDicho(met = test$alter, dic= test$MLK)


# (e) Eine Funktion, die eine mindestens ordinal skalierte Variable
# quantilbasiert kategorisiert (z.B. in niedrig, mittel, hoch)

quantKateg <- function(){
  
}



# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier
# kategorialen Variablen erstellt

visualize <- function(data1,data2,data3,data4){
  
  par(mfrow=c(2,2),mar=c(3,6,2,1))
  
  barplot(table(data1)/100,horiz=TRUE,las=1,xlim=c(0,1),col="lightblue",
           xlab="Prozent",main="Studienfach")
  box()
  
  barplot(table(data2)/100,horiz=TRUE,las=1,xlim=c(0,1),col="red4",
          xlab="Prozent",main="MatheLK")
  box()
  
  barplot(table(data3)/100,horiz=TRUE,las=1,xlim=c(0,1),col="green4",
          xlab="Prozent",main="Interesse in Programmieren")
  box()
  
  barplot(table(data4)/100,horiz=TRUE,las=1,xlim=c(0,1),col="darkviolet",
          xlab="Prozent",main="Intersse in Mathe ")
  box()
  
  
}

visualize(daten$studienfach,daten$matheLK,daten$intProg,daten$intMathe)



# Freiwillig: weitere zur Deskription und Visualisierung geeignete
# Funktionen
