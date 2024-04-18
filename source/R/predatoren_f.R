roofvogels_f <- function() {
  roofvogels <- c(
    "Blauwe Kiekendief",
    "Boomvalk",
    "Bosuil",
    "Bruine Kiekendief",
    "Buizerd",
    "Grauwe Kiekendief",
    "Havik",
    "Ransuil",
    "Rode Wouw",
    "Slechtvalk",
    "Smelleken",
    "Sperwer",
    "Steenuil",
    "Steppekiekendief",
    "Torenvalk",
    "Vale Gier",
    "Velduil",
    "Wespendief",
    "Zwarte Wouw")

  return(roofvogels)
}

kraaiachtigen_f <- function() {
  kraaiachtigen <- c(
    "Ekster",
    "Gaai",
    "Kauw",
    "Raaf",
    "Roek",
    "Zwarte Kraai")

  return(kraaiachtigen)
}

predatoren_f <- function() {
  return(c(roofvogels, kraaiachtigen))
}
