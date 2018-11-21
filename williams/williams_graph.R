# 0. Setup ####

library(stringr)
library(statnet)
library(scholar)
library(networkDynamic)
library(ndtv)
library(ergm)
library(igraph)

#from google scholar ####
williams = get_publications("h7Na1OoAAAAJ&hl", cstart = 0, pagesize = 100, flush = T)
dim(williams)

#fixes
williams$year[nrow(williams)]=1985 #looked up in pamela's spreadsheet
#removed this paper, seemed to be written by SW cowper ?
williams = williams[-35,] 
williams = williams[-109,] #another error with OS platt
#limitation in that only 6 or 7 authors show up?
williams.coauthors = sapply(as.character(williams$author), strsplit, ", ")
williams.coauthors = lapply(williams.coauthors, function(x) {
  str_replace(x, pattern = "S.*[w|W]illiams", replacement = "SL Williams")
})
#make sure williams in all
williams.coauthors = sapply(williams.coauthors, function(x) {
  if (! ("SL Williams" %in% x)) {
    x = c(x,"SL Williams") 
  } else x = x})

# dealt with multiple williams'
#williams.coauthors.unique[str_detect(williams.coauthors.unique, "[w|W]illiams")]
williams.coauthors.unique = unique(unlist(williams.coauthors))

#from Pamela ####
williams = readRDS("~/Documents/DSI/williams/williams_clean_endnote_data.Rds")
names(williams)[2] =  c("author")
#fixes 
williams$author[49][[1]] = c("Wright, J. T.", "Williams, S. L.", "Dethier, Megan")
williams$author[34][[1]][1] = "Reusch, T. B. H"
williams$author[103][[1]][8] = "Miller, A."
williams$author[54][[1]][2] = "Hughes, A."

williams.coauthors = williams$author
williams.coauthors = lapply(williams.coauthors, function(x) {
  str_replace(x, pattern = ".*[w|W]illiams.*", replacement = "Williams, S. L.")
})
williams.coauthors = sapply(williams.coauthors, function(x) {
  if (! ("Williams, S. L." %in% x)) {
    x = c(x,"Williams, S. L.") 
  } else x = x})
williams.coauthors =lapply(williams.coauthors, function(x) {
  str_replace_all(string = x , pattern = "(?<=,\ [A-Z])[a-z]+", ". ")})

williams.coauthors =lapply(williams.coauthors, function(x) {
  str_replace(string = x, pattern = "(?<=\\.).*$", "")})
                            
williams.coauthors.unique = unique(unlist(williams.coauthors))
sort(williams.coauthors.unique)

williams = data.frame(title = williams$title, year = williams$date, journal = williams$periodicals)
williams$year = as.numeric(as.character(williams$year))

# 1. Co-authors ####

#which.max(lapply(lapply(williams.coauthors, str_detect, "S Williams Cowper"), sum))
williams.edges = lapply(williams.coauthors, function(x) {williams.coauthors.unique %in% x})

# number of single
williams.degree = sapply(williams.edges, sum); table(williams.degree)

N = length(williams.coauthors.unique)
P = length(williams.edges)
tmp = matrix(do.call(cbind, williams.edges), nrow = N, ncol = P)
rownames(tmp) = williams.coauthors.unique
williams.mat = tmp %*% t(tmp)

author.codegree = williams.mat["Williams, S.",]
author.codegree2 = log(author.codegree) + .5

# students
williams.ms.students = c("Yarish, S.", "Lu, T.", "Di Fiori, R.", "Davis, C.", "Ewanchuk, P.", 
                         "Cheroske, A.", "Lieberman, C.")
williams.phd.students = c("Ruckelshaus, M.")
williams.davisphd.students = c("Rodriguez, L.", "Sorte, C.", "Newsom, A.",  
                          "Szoboszlai, A.", "Ha, G.", "DuBois, K.")

student.color = rep("white", length(williams.coauthors.unique)) #colleague
student.color[williams.coauthors.unique %in% williams.phd.students] = "red" #Phd
student.color[williams.coauthors.unique %in% williams.davisphd.students] = "blue" #Phd
student.color[williams.coauthors.unique %in% williams.ms.students] = "yellow" #MS

student = rep("", length(williams.coauthors.unique)) #colleague
student[williams.coauthors.unique %in% williams.phd.students] = "Phd Student" #Phd
student[williams.coauthors.unique %in% williams.davisphd.students] = "Davis PhD Student" #Phd
student[williams.coauthors.unique %in% williams.ms.students] = "MS Student" #MS

# 2. static net ####

#set.seed(3) there is randomness in the layouts
williams.net = as.network(williams.mat, directed = F, names.eval = "edge.lwd", ignore.eval = F)
williams.net%v%"author" = williams.coauthors.unique
williams.net%v%"vertex.cex" = author.codegree2 #how many papers with williams?
williams.net%v%"vertex.pid" = 1:length(williams.coauthors.unique) #how many papers with williams?
williams.net%v%"student" = student #how many papers with williams?
williams.net%v%"student.color" = student.color #how many papers with williams?

plot.network(williams.net, edge.col = "white", 
             label = "vertex.names", label.cex = .5,
             vertex.col = williams.net%v%"student.color",
             label.pad = 0, label.pos = 1,
             edge.lwd = williams.net%e%"edge.lwd")

williams.igraph = intergraph::asIgraph(williams.net)
williams.layout = data.frame(layout.fruchterman.reingold(williams.igraph))
colnames(williams.layout) = c("x","y")
#os platt?
#sapply(williams.coauthors, str_detect, "Platt")
#which.max(sapply(.Last.value, sum))
#williams[109,]


# 3. dynamic net ####

table(williams$year)
slices = seq(1983,2018, by = 1)
start = min(slices); end = max(slices)
author.first = sapply(williams.coauthors.unique, #flexible depending on source google scholar or pamela
  function(x) {
   min(williams[which(sapply(williams.coauthors, function(y) {x %in% y})),"year"], na.rm = T)
  })

williams.network.list = lapply(slices, function(i) {
  authors.sub = author.first <= i
  williams.sort = subset(williams, williams$year <= i)
  N.sub = sum(authors.sub)
  P.sub = nrow(williams.sort)
  williams.edges.sub = lapply(williams.edges[williams$year <= i], function(x) x[authors.sub])
  tmp = matrix(do.call(cbind, williams.edges.sub), nrow = N.sub, ncol = P.sub)
  
  rownames(tmp) = williams.coauthors.unique[authors.sub]
  coauthor.mat = tmp %*% t(tmp)
  coauthor.net = as.network(coauthor.mat, directed = F, names.eval = "edge.lwd", ignore.eval = F)
  coauthor.net%v%"author" = williams.coauthors.unique[authors.sub]
  coauthor.net%v%"student.color" = student.color[authors.sub]
  coauthor.net%v%"student" = student[authors.sub]
  coauthor.net%v%"x" = williams.layout$x[authors.sub]
  coauthor.net%v%"y" = williams.layout$y[authors.sub]
  coauthor.net%v%"vertex.pid" = which(authors.sub)
  coauthor.net%v%"vertex.cex" = author.codegree2[authors.sub] #how many papers with williams?
  return(coauthor.net)
})

tmp = vector(mode = "list", length=min(slices)-1)
tmp = lapply(tmp, function(x) williams.network.list[[1]])
williams.network.list = c(tmp, williams.network.list)

williams.dynamic = networkDynamic(base.net=williams.net, network.list = williams.network.list,
                                  vertex.pid = "vertex.pid", create.TEAs = T)
williams.dynamic 

compute.animation(williams.dynamic, animation.mode = "useAttribute",
                  slice.par=list(start=start, end=end, interval=2, aggregate.dur = 2, rule='latest'),
                  weight.attr = c("edge.lwd"))

render.d3movie(williams.dynamic, usearrows = F, displaylabels = T,
               label= "author",
               vertex.cex = "vertex.cex",
               vertex.col = "student.color",
               vertex.tooltip=paste(williams.net%v%'author', williams.net%v%'student', sep = "<br>"),
               label.col = "black",
               label.cex = .7,
               edge.col = "#cfddc5",
               edge.lwd = "edge.lwd",
               main = "Susan Williams Co-Author Network over Time: 1983-2018",
               xlab = "test",
               bg="#cfddc5",
               vertex.border="#333333",
               render.par = list(show.time = TRUE, show.stats = "~edges"),
               launchBrowser=F, filename="~/Documents/DSI/williams/williamsNet.html", 
               d3.options = list(slider = TRUE))

