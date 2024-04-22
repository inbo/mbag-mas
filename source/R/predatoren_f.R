roofvogels_f <- function() {
  roofvogels <- c(
    "Blauwe Kiekendief",
    "Boomvalk",
    "Bosuil",
    "Bruine Kiekendief",
    "Buizerd",
    "Grauwe Kiekendief",
    "Havik",
    "Kerkuil",
    "Ransuil",
    "Rode Wouw",
    "Slangenarend",
    "Slechtvalk",
    "Smelleken",
    "Sperwer",
    "Steenuil",
    "Steppekiekendief",
    "Torenvalk",
    "Velduil",
    "Visarend",
    "Wespendief",
    "Zeearend",
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
  return(c(roofvogels_f(), kraaiachtigen_f()))
}
