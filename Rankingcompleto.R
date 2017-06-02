getwd()

rankingcompleto <- function(resultado , num = "mejor"){
  setwd("C:/Users/aniger95/Calidad de Hospitales")
  lectura <- read.csv("outcome-of-care-measures.csv")
  estados <- as.character(lectura$State)
  
  x <- c()
  
  if (!(resultado %in%  c("ataque","falla","neumonia"))) {
    stop("Dato invalido") 
    }
  
  if (resultado == "ataque") {
    a <- 11
    lectura <- lectura[grep("[[:digit:]]", lectura[,a]), ]
    lec2 <-  data.frame(lectura[,2],lectura[,7], lectura[, a])
    }
  if (resultado == "falla") { 
    a <-  17
    lectura <- lectura[grep("[[:digit:]]", lectura[,a]), ]
    lec2 <-  data.frame(lectura[,2],lectura[,7], lectura[, a])
    }
  
  if (resultado == "neumonia") {
    a <- 23
    lectura <- lectura[grep("[[:digit:]]", lectura[,a]), ]
    lec2 <-  data.frame(lectura[,2],lectura[,7], lectura[, a])
    }
    
  
  colnames(lec2) = c("hospital", "estado", "resultado")
  lec2 <- lec2[order(lectura[,7], lectura[,2], na.last = NA), ]
  
  
  sp <- split(lec2, lec2[,2])
  
    for (i in 1:54) {
      s <- num 
      s1 <- length(i == as.numeric(lec2[,2]))
      w <- as.data.frame(sp[i])
      sp <- w[order(as.numeric(as.vector(w[,3])), w[,1]), ]
      if(num=="mejor") {
        s <- 1
      }
      if(num=="peor"){
        s1 <-1
      }        
      sp <- data.frame(sp[s,1], sp[1,2])
      x <- rbind(x, sp)
      sp <- split(lec2, lec2[,2])
      num <- num
      
      }
colnames(x) <- c("Hospital", "Estado")
x
  
}
head(rankingcompleto("ataque", 20),10)
