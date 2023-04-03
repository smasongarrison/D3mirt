## code to prepare `anes08_09offwaves` dataset goes here
x <- anes08_09offwaves
z <- cbind(x[, 2647:2648], x[,5654:5673])
z <- replace(z, z == -7, NA)
z <- replace(z, z == -5, NA)
z <- na.omit(z)
anes08_09offwaves <- z
usethis::use_data(anes08_09offwaves, overwrite = TRUE)
