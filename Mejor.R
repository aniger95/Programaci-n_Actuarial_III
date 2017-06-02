mejor <- function(estado,resultado){
  setwd("C:/Users/aniger95/Calidad de Hospitales")
  lectura <- read.csv("outcome-of-care-measures.csv")
  estados <- as.character(lectura$State)
  
  
  if (!(resultado %in%  c("ataque","falla","neumonia"))) {
    stop("Dato invalido") 
  }
  
  
  if (!(estado %in% estados)){
    stop("estado inválido")
  }
  
  s <- subset(lectura,lectura$State == estado)
  
  if (resultado == "ataque") { 
    
    dat <- s[,11]
    }else  if (resultado == "falla") { 
    dat <-  s[,17]
    }else  if (resultado == "neumonia") {
    dat <- s[,23]
    }
  
  dat <- suppressWarnings(as.numeric(dat))
  mort <- s[which.min(dat),] 
  
  hospital <- mort[order(mort[,2] , na.last = NA), ]
  hospital1 <- hospital[1,2]
  hospital1
}
mejor("MD","ataque")
