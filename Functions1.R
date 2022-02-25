#Hier die Funktionen zur Auswertung
test <- data.frame("alter"=round(rnorm(100, mean = 25,sd = 2),1),
                   "fach"=factor(sample(c("A", "B", "C"),100, rep=T)),
                   "IntMath" = factor(sample(1:7, 100, rep=T)),
                   "IntPro" = factor(sample(1:7, 100, rep=T)),
                   "MLK" = factor(sample(0:1, 100, rep=T))                   )

# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer metrische Variablen berechnet und ausgibt

metric <- function(v, boxpl = FALSE){
  if(boxpl){boxplot(v)}
  return(c(summary(v), "Var" = var(v)))
}
#Test
metric(test$alter)

# b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer kategoriale Variablen berechnet und ausgibt

kateg <- function(v, graf = FALSE){
  
  #Grafik
  if(graf){
    n <- length(levels(v))
    maH <- max(table(v))
    plot.new()
    plot.window(xlim=c(0,n+1), ylim=c(0,maH))
    axis(1, at=0:(n+1), labels = c("",levels(v),""))
    axis(2)
    rect(xleft=(1:n)-0.5, ybottom=0, xright=(1:n)+0.5, ytop=table(v))
  }
  return(list("Modus" = names(which(table(v)==max(table(v)))), "Hauefigkeiten" = table(v),
              "relative Hauefigkeiten" = table(v)*(1/length(v))))

}
#Test
kateg(test$IntMath, TRUE)

# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
# den Zusammenhang zwischen zwei kategorialen Variablen
# berechnet ausgibt

bivKateg <- function(v1, v2){
  par(mfrow=c(1,2))
  
  #einzelne Auswertungen
  cat("Auswewrtung der Variable v1", "\n")
  print(kateg(v1))
  cat("--------------------------", "\n")
  cat("Auswertung der Variable v2", "\n")
  print(kateg(v2))
  cat("--------------------------", "\n")
  
  #gemeinsame Haeufigkeiten
  haeufigkeitstabelle = table(v1, v2)
  rel_haeufigkeitstabelle = (1/min(c(length(v1), length(v2))))*haeufigkeitstabelle
  return(list("Kombinationshauefigkeiten" = haeufigkeitstabelle, 
              "relative Kombinationshaeufigkeiten" = rel_haeufigkeitstabelle))
  
  #evtl noch Plot der gemeinsamen Haeufigkeiten
}

#Test
bivKateg(test$IntMath, test$MLK)


# (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
# den Zusammengang zwischen einer metrischen und einer
# dichotomen Variablen berechnet und ausgibt

bivMetDicho <- function(met, dic){
  boxplot(met ~ dic)
  realisationen_von_dic = dic[which(duplicated(janein) == FALSE)]
  met1 = met[which(dic == realisationen_von_dic[1])]
  met2 = met[which(dic == realisationen_von_dic[2])]
  
  return(list("Realisationen_von_dic" = table(dic), "Erste_Realisation_von_dic" = metric(met1), 
              "Zweite_Realisation_von_dic" = metric(met2)))
}

#Test
bivMetDicho(met = test$alter, dic=test$MLK)


# (e) Eine Funktion, die eine mindestens ordinal skalierte Variable
# quantilbasiert kategorisiert (z.B. in niedrig, mittel, hoch)

#unterteilt in hoch, mittel oder niedrig
#Hoch heißt beispielswese, dass 66% der Realisierungen kleiner sind, als die untere 
#Grenze des Intervals für hoch
quantKateg <- function(v){
  v = as.numeric(v)
  quants = quantile(v, c(0.33,0.66))
  categ = 0
  for(i in 1:length(v)){
    if(v[i] <= quants[1]){categ[i] = "niedirg"}
    if(v[i] <= quants[2] && v[i] > quants[1]){categ[i] = "mittel"} 
    if(v[i] > quants[2]){categ[i] = "hoch"}
    }
  
  return(list("Qunatile von v" = quants, "Kategorisierter_Vektor" = categ))

}
#Test
quantKateg(test$IntMath)



# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier
# kategorialen Variablen erstellt

visualize <- function(){
  
}



# Freiwillig: weitere zur Deskription und Visualisierung geeignete
# Funktionen