# Post a list of protein GI numbers to the Entrez history server

id_list <- c(194680922,50978626,28558982,9507199,6678417)
p <- epost(id=id_list, db="protein")
p
