#Hier die Funktionen zur Auswertung
test <- data.frame("alter"=round(rnorm(100, mean = 25,sd = 2),1),
                   "fach"=factor(sample(c("A", "B", "C"),100, rep=T)),
                   "IntMath" = factor(sample(1:7, 100, rep=T)),
                   "IntPro" = factor(sample(1:7, 100, rep=T)),
                   "MLK" = factor(sample(0:1, 100, rep=T)))

# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer metrische Variablen berechnet und ausgibt


#Die Frunktion metric gibt als Rueckgabe eine Liste mit: Minimum, Unteres Quartil,
#Median, Arith. Mittel, oberes Quartil, Maximum und Varianz der Daten. 
#Wenn der Eingabeparameter "boxpl" auf TRUE setzt, dann wird zudem noch ein Boxplot der
#Daten gezeichnet und es muss eine ueberschrift eingefuegt werden, ebenfalls als Eingabeparameter.
#dev.off() wird hier im Falle eines plots und auch bei allen anderen Funktionen in den nur ein plot
#erstellt wird ausgefuehrt um Voreinstellungen der R-Session des Bentzers zurueckzusetzen.
metric <- function(v, name = "" , boxpl=FALSE){
  if(boxpl){
    par(mfrow = c(1,1))
    boxplot(v, xlab=name, main=paste("Auswertung", name))
    }
  return(c(summary(v), "Varianz"=var(v)))
}
#Test
metric(test$alter, "Alter", TRUE)



# b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# fuer kategoriale Variablen berechnet und ausgibt.
# Erstellt den Modus, die absolute Haeufigkeit und die relative Haeufigkeit.


#Die Funktion kateg gibt eine Haeufigkeitstabelle zurueck und mit der Hilfsfunktion
#aus Functions2 auch deren Modalwert. Generiert wird die Tabelle ueber den table Befehl 
#aus R. Zudem wird eine relative Hauefigkeitstabelle zurueckgegeben, indem durch die Anzahl
#der Datenpunkte dividiert wird. 
#Wenn plot TRUE ist, dann wird zudem ein Histogramm der Daten geplottet.
kateg <- function(v, name = "", plot=FALSE){
  source("Functions2.R")
  tab <- table(v)
  mod <- mod(v)
  
  #Grafik
  if(plot){
    par(mfrow = c(1,1))
    n <- length(levels(as.factor(v)))
    maH <- max(table(v))
    plot.new()
    plot.window(xlim=c(0,n+1), ylim=c(0,maH*1.3))
    par(oma=c(3,3,3,3))
    axis(1, at=0:(n+1), labels = c("",levels(as.factor(v)),""))
    axis(2)
    rect(xleft=(1:n)-0.5, ybottom=0, xright=(1:n)+0.5, ytop=table(v))
    mtext(text=paste("Haeufigkeiten", name),side = 3, outer=TRUE)
    mtext("Auspraegung", 1, outer = TRUE)
    mtext("Haeufigkeit", 2, outer = TRUE)
  }
  return(list("Modus"=mod, "abs. Haeufigkeiten"=tab, "rel. Haeufigkeiten"=tab*(1/length(v))))
}
#Test
kateg(test$IntMath, "Interesse Mathe", TRUE)

# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
# den Zusammenhang zwischen zwei kategorialen Variablen
# berechnet ausgibt.
# Erstellt die gemeinsame Haeufigkeit und die dazugehoerige relative 
# Haeufigkeit.


#Die Funktion printet zunaechst Haeufigkeitstabellen der einzelnen Variabeln und dann
#gibt es nach analogen Vorgehen aus b eine absolute und eine realtive Haeufigkeitstabelle 
#zurueck. Des Weiteren kann auch hier mit plot = TRUE und dementsprechenden Eingaben der 
#Variablennamen eine GRafik geplotted werden.
bivKateg <- function(v1, v2, name1 = "", name2 = "", plot = FALSE){
  #einzelne Auswertungen
  cat("Auswertung der Variable v1", "\n")
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
    par(mfrow = c(1,1))
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

#Die Funktion plottet nebeneinander 2 Boxplots, indem nach der dichotomen Variable aufgeteilt wird.
#Sie gibt ausserdem die in a erstellten Informationen, ebenfalls aufgeteilt aus, und wie oft
#die jeweiligne Realisationen der dcihotomen Variable vorkommt.
bivMetDicho <- function(met, dic, nameMet, nameDic){
  par(mfrow = c(1,1))
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

#Kategorisiert mit Hilfe der Quantil-Funktion eine Eingabevariable in 3 verschiedene
#Klassen und gibt diese als Vektor zurueck, das heisst jede Realisation wird hier einmal kategorisiert.
quantKateg <- function(v){
  quantiles <- quantile(v, c(0.33,0.66))
  kat <- 0
  for(i in 1:length(v)){
    if(v[i]<=quantiles[1]){kat[i] <- "niedrig"}
    if(v[i]<=quantiles[2] && v[i]>quantiles[1]){kat[i] <- "mittel"}
    if(v[i]>quantiles[2]){kat[i] <- "hoch"}
  }
  return(kat)
}

#Test
quantKateg(test$alter)



# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier
# kategorialen Variablen erstellt


#Gibt fuer 3 oder wenn fuer den Namen von Variable 4 auch eine Eingabe getaetigt wird 
#entsprechende Barplots aus. Das Vorgehen ist hierbei fuer alle Variablen gleich,
#und die Plots werden mit Hilfe von par(mfrow = c(2,2)) in ein Grafikfenster geplottet.
visualize <- function(v1, name1, v2, name2, v3, name3, v4 = "", name4 = "") {
  par(mfrow = c(2, 2), mar = c(4, 6, 2, 1))
  barplot(table(v1), horiz = TRUE, las = 1, col = "lightblue", xlim = c(0, max(table(v1))*1.3),
          main=name1)
  box()
  mtext(text = "Haeufigkeit", side = 1, line = 2.2)
  barplot(table(v2), horiz = TRUE, las = 1, col = "red4", xlim = c(0, max(table(v2))*1.3),
          main=name2)
  box()
  mtext(text = "Haeufigkeit", side = 1, line = 2.2)
  barplot(table(v3), horiz = TRUE, las = 1, col = "green4", xlim = c(0, max(table(v3))*1.3),
          main=name3)
  box()
  mtext(text = "Haeufigkeit", side = 1, line = 2.2)
  if (v4 != "") {
    barplot(table(v4), horiz = TRUE, las = 1, col = "darkviolet", xlim = c(0, max(table(v4))*1.3),
            main=name4)
    box()
    mtext(text = "Haeufigkeit", side = 1, line = 2.2)
  }
}

visualize(v1=test$fach, name1="Fach", v2=test$IntMath, name2="InteresseMathe", v3=test$IntPro,
          name3="Interesse Programmieren", v4=test$MLK, name4="MatheLK")

# Freiwillig: weitere zur Deskription und Visualisierung geeignete
# Funktionen
