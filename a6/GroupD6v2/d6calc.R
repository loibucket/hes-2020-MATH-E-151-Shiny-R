#d6calc.R

makeD6data <- function(neutral) {
  N <- 12
  DF <- data.frame(button=character(N),
                   perm =character(N),color=character(N),stringsAsFactors= FALSE)
  DF[1,] <- c("btnI","I",neutral) #i
  DF[2,] <- c("btn123456","(123456)",neutral) #r 6
  DF[3,] <- c("btn135.246","(135)(246)",neutral) # 3
  DF[4,] <- c("btn14.25.36","(14)(25)(36)",neutral) # 2
  DF[5,] <- c("btn153.264","(153)(264)",neutral) # 3
  DF[6,] <- c("btn165432","(165432)",neutral) # 6
  DF[7,] <- c("btn13.46","(13)(46)",neutral) # 2
  DF[8,] <- c("btn26.35","(26)(35)",neutral) # 2
  DF[9,] <- c("btn15.24","(15)(24)",neutral) # 2
  DF[10,] <- c("btn14.23.56","(14)(23)(56)",neutral) # 2
  DF[11,] <- c("btn12.36.45","(12)(36)(45)",neutral) # 2
  DF[12,] <- c("btn16.25.34","(16)(25)(34)",neutral) # 2
  return(DF)
}

# C6
#"(123456)"     "(135)(246)"   "(14)(25)(36)" "(153)(264)"   "(165432)"     "I" 
#    2              3                4             5              6          1

# C3 
# "(135)(246)" "(153)(264)" "I" 
#      3             5       1

# S3
# "(12)(36)(45)"  "(14)(23)(56)"  "(16)(25)(34)"  "(135)(246)"  "(153)(264)"  "I"
#     11               10              12             3              5           1

# V4
# "(14)(25)(36)"  "(16)(25)(34)"  "(13)(46)"  "I"
#      4               12            7         1
