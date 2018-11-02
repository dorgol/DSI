# 0. Setup ###

library(stringr)
library(statnet)
library(scholar)

#from Pamela
williams2 = readRDS("~/Documents/DSI/DSI_repo/williams/williams_clean_endnote_data.Rds")
sort(unique(unlist(williams2$contributers)))

#from google scholar
williams = get_publications("h7Na1OoAAAAJ&hl", pagesize= 1000)
#removed this paper, seemed to be written by SW cowper ?
williams = williams[-35,] 
williams = williams[-109,] #another error with OS platt

# 1. Co-authors

#limitation in that only 6 or 7 authors show up?
williams.coauthors = sapply(as.character(williams$author), strsplit, ", ")
williams.coauthors = lapply(williams.coauthors, function(x) {
  str_replace(x, pattern = "S.*[w|W]illiams", replacement = "SL Williams")
})
#^ dealt with multiple williams'
#williams.coauthors.unique[str_detect(williams.coauthors.unique, "[w|W]illiams")]
williams.coauthors.unique = unique(unlist(williams.coauthors))

#which.max(lapply(lapply(williams.coauthors, str_detect, "S Williams Cowper"), sum))
williams.repeats = lapply(williams.coauthors, function(x) {williams.coauthors.unique %in% x})

# number of single
williams.repeats.count = sapply(williams.repeats, sum); table(williams.repeats.count)

N = length(williams.coauthors.unique)
P = length(williams.repeats.count)
tmp = matrix(do.call(cbind, williams.repeats), nrow = N, ncol = P)
rownames(tmp) = williams.coauthors.unique
coauthor.mat = tmp %*% t(tmp)
coauthor.net = as.network(coauthor.mat, directed = F)
coauthor.net%v%"author" = williams.coauthors.unique
plot.network(coauthor.net, edge.col = "gray", 
             edge.lwd = .3, label = "vertex.names", label.cex = .5,
             label.pad = 0, label.pos = 1)

#os platt?
#sapply(williams.coauthors, str_detect, "Platt")
#which.max(sapply(.Last.value, sum))
#williams[109,]

get_complete_authors("10701884175234812264", "0EnyYjriUFMC")
