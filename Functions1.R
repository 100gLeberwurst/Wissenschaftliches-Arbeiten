#Hier die Funktionen zur Auswertung
test <- data.frame("alter"=round(rnorm(100, mean = 25,sd = 2),1),
                   "fach"=factor(sample(c("A", "B", "C"),100, rep=T)),
                   "IntMath" = factor(sample(1:7, 100, rep=T)),
                   "IntPro" = factor(sample(1:7, 100, rep=T)),
                   "MLK" = factor(sample(0:1, 100, rep=T))                   )

# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer metrische Variablen berechnet und ausgibt

metric <- function(v, name, boxpl=FALSE){
  if(boxpl){boxplot(v, xlab=name, main=paste("Auswertung", name))}
  return(c(summary(v), "Varianz"=var(v)))
}
#Test
metric(test$alter, "Alter")



# b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer kategoriale Variablen berechnet und ausgibt.
# Erstellt den Modus, die absolute Haeufigkeit und die relative Haeufigkeit.

kateg <- function(v, name, plot=TRUE){
  load("Functions1.R")
  tab <- table(v)
  mod <- mod(v)
  
  #Grafik
  if(plot){
    n <- length(levels(as.factor(v)))
    maH <- max(table(v))
    plot.new()
    plot.window(xlim=c(0,n+1), ylim=c(0,maH))
    par(oma=c(3,3,3,3))
    axis(1, at=0:(n+1), labels = c("",1:n,""))
    axis(2)
    rect(xleft=(1:n)-0.5, ybottom=0, xright=(1:n)+0.5, ytop=table(v))
    mtext(text=paste("Haeufigkeiten", name),side = 3, outer=TRUE)
    mtext("Auspraegung", 1, outer = TRUE)
    mtext("Haeufigkeit", 2, outer = TRUE)
  }
  return(list("Modus"=mod, "abs. Haeufigkeiten"=tab, "rel. Haeufigkeiten"=tab*(1/length(v))))
}
#Test
kateg(test$IntMath, "Interesse Mathe")

# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
# den Zusammenhang zwischen zwei kategorialen Variablen
# berechnet ausgibt.
# Erstellt die gemeinsame Haeufigkeit und die dazugehoerige relative 
# Haeufigkeit.

bivKateg <- function(v1, v2, name1, name2, plot){
  #einzelne Auswertungen
  cat("Auswewrtung der Variable v1", "\n")
  print(kateg(v1,name1))
  cat("--------------------------", "\n")
  cat("Auswertung der Variable v2", "\n")
  print(kateg(v2,name2))
  cat("--------------------------", "\n")
  
  #gemeinsame Haeufigkeiten
  tab <- table(v1, v2)
  rel_tab <- (1/min(c(length(v1), length(v2))))*tab
  
  #evtl noch Plot der gemeinsamen Haeufigkeiten
  if(plot){
    dev.off()
    plot(table(v1,v2), xlab=name1, ylab=name2, 
         main=paste("Zusammenhang", name1 ,"und", name2))
  }
  
  return(list("gemeinsame Haeufigkeiten"= tab, "rel. Haeufigkeiten"=rel_tab))
  
}

#Test
bivKateg(test$IntMath, test$MLK, "Interesse Mathe", "Mathe LK", TRUE)


# (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
# den Zusammengang zwischen einer metrischen und einer
# dichotomen Variablen berechnet und ausgibt

bivMetDicho <- function(met, dic, nameMet, nameDic){
  boxplot(met ~ dic, xlab=nameDic, ylab=nameMet, 
          main=paste("Zusammenhang", nameMet, "und", nameDic))
  realisationen_von_dic = dic[which(duplicated(dic) == FALSE)]
  met1 = met[which(dic == realisationen_von_dic[1])]
  met2 = met[which(dic == realisationen_von_dic[2])]
  
  return(list("Realisationen_von_dic" = table(dic), "Erste_Realisation_von_dic" = metric(met1), 
              "Zweite_Realisation_von_dic" = metric(met2)))
}

#Test
bivMetDicho(met = test$alter, dic= test$MLK, "Alter", "MatheLK")


# (e) Eine Funktion, die eine mindestens ordinal skalierte Variable
# quantilbasiert kategorisiert (z.B. in niedrig, mittel, hoch)

quantKateg <- function(v){
  quantiles <- quantile(v, c(0.33,0.66))
  kat <- 0
  for(i in 1:length(v)){
    if(v[i]<=quantiles[1]){kat[i] <- "niedrig"}
    if(v[i]<=quantiles[2] && v[i]>quantiles[1]){kat[i] <- "mittel"}
    if(v[i]>quantiles[2]){kat[i] <- "hoch"}
  }
  if(plot){
    barplot(table(kat), xlab = "Anzahl", ylab = "Quantilseinteilung", horiz = TRUE, xlim = c(0, (max(table(kat)))+5))
    title(main = "Anzahl je 33%-Quantil")
  }
  return(kat)
}

#Test
quantKateg(test$alter)



# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier
# kategorialen Variablen erstellt

visualize <- function(v1,v2,v3,v4="", name1, name2, name3, name4=""){
  par(mfrow=c(2,2))
  plot(table(v1), xlab=name1, ylab="Haeufigkeit")
  box()
  plot(table(v2), xlab=name2, ylab="Haeufigkeit")
  box()
  plot(table(v3), xlab=name3, ylab="Haeufigkeit")
  box()
  if(v4!=""){
    plot(table(v4), xlab=name4, ylab="Haeufigkeit")
    box()
    }
}

visualize(test$fach, test$IntMath, test$IntPro, test$MLK, 
          "Fach", "InteresseMathe", "Interesse Programmieren", "MatheLK")

# Freiwillig: weitere zur Deskription und Visualisierung geeignete
# Funktionen
