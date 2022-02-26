#helfer-Funktion
mod <- function(daten)
{
  return(names(which(table(daten)==max(table(daten)))))
}
