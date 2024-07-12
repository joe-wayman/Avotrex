##Code to load in and format AvotrexPhylo data
#object. You need to round the group and time columns
#as floating point weirdness between excel and R means
#some have many decimal places
av <- readxl::read_excel(file.choose())
av$group <- round(as.numeric(av$group), 1)
av$time_fixed <- round(as.numeric(av$time_fixed), 3)
colnames(av)
av <- av[,1:19]
AvotrexPhylo <- av
usethis::use_data(AvotrexPhylo, overwrite = TRUE)
