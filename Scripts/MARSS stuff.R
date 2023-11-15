library(MARSS)

#run source on chla.R code
#use chla dataset
#convert to long form
empty.chla <- rep(NA, 24)
chla.vec <- c(empty.chla, as.numeric(as.vector(chla[1,2:13])), as.numeric(as.vector(chla[2,2:13])), as.numeric(as.vector(chla[3,2:13])), as.numeric(as.vector(chla[4,2:13])), as.numeric(as.vector(chla[5,2:13])), as.numeric(as.vector(chla[6,2:13])), as.numeric(as.vector(chla[7,2:13])), as.numeric(as.vector(chla[8,2:13])), as.numeric(as.vector(chla[9,2:13])), as.numeric(as.vector(chla[10,2:13])), as.numeric(as.vector(chla[11,2:13])), as.numeric(as.vector(chla[12,2:13])), as.numeric(as.vector(chla[13,2:13])), as.numeric(as.vector(chla[14,2:13])), as.numeric(as.vector(chla[15,2:13])), as.numeric(as.vector(chla[16,2:13])), as.numeric(as.vector(chla[17,2:13])), as.numeric(as.vector(chla[18,2:13])), as.numeric(as.vector(chla[19,2:13])), as.numeric(as.vector(chla[20,2:13])), as.numeric(as.vector(chla[21,2:13])), as.numeric(as.vector(chla[22,2:13])), as.numeric(as.vector(chla[23,2:13])), as.numeric(as.vector(chla[24,2:13])), as.numeric(as.vector(chla[25,2:13])), as.numeric(as.vector(chla[26,2:13])))

diag.matrix <- diag(x = 1, nrow = 12, ncol = 12, names = FALSE)
bound.matrix <- c(diag.matrix, diag.matrix) #trying this and also cbind. outputs look... not what we want. thoughts?



#here goes nothing
mod.list <- list(Z = matrix(1),
                 a = matrix(0), #error message stating this "not allowed in model list for this form"
                 D = matrix(0),
                 d = matrix(0),
                 R = matrix("R"),
                 B = matrix(1),
                 u = matrix(0), #error message stating this "not allowed in model list for this form"
                 Q = matrix("Q"),
                 C = "diagonal and unequal",
                 c = diag.matrix)

obj <- MARSS(y = chla.vec, model = mod.list)

chl <- obj$states
