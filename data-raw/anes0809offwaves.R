## code to prepare `anes0809offwaves` dataset goes here
## Load file "ANES 2008-2009.Rmd"
x <- anes08_09offwaves
z <- cbind(x[, 2647:2648], x[,5654:5673])
z <- replace(z, z == -7, NA)
z <- replace(z, z == -5, NA)
z <- na.omit(z)
col_order <- c("W3Xage", "W3XGENDER",
               "W7Q1", "W7Q2", "W7Q3", "W7Q4", "W7Q5",
               "W7Q6", "W7Q7", "W7Q8", "W7Q9", "W7Q10",
               "W7Q11", "W7Q12", "W7Q13", "W7Q14", "W7Q15",
               "W7Q16", "W7Q17", "W7Q18", "W7Q19", "W7Q20")
z <- z[, col_order]
anes0809offwaves <- z
usethis::use_data(anes0809offwaves, overwrite = TRUE)
