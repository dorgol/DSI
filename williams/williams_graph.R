# 0. Setup ####

library(stringr)
library(statnet)
library(scholar)
library(networkDynamic)
library(ndtv)

#from Pamela
williams.xml = readRDS("~/Documents/DSI/williams/williams_clean_endnote_data.Rds")
sort(unique(unlist(williams.xml$contributers)))

#from google scholar
williams = get_publications("h7Na1OoAAAAJ&hl", cstart = 0, pagesize = 100, flush = T)
dim(williams)

#fixes
williams$year[nrow(williams)]=1985 #looked up in pamela's spreadsheet
#removed this paper, seemed to be written by SW cowper ?
williams = williams[-35,] 
williams = williams[-109,] #another error with OS platt

# 1. Co-authors ####

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

#which.max(lapply(lapply(williams.coauthors, str_detect, "S Williams Cowper"), sum))
williams.edges = lapply(williams.coauthors, function(x) {williams.coauthors.unique %in% x})

# number of single
williams.degree = sapply(williams.edges, sum); table(williams.degree)

N = length(williams.coauthors.unique)
P = length(williams.edges.count)
tmp = matrix(do.call(cbind, williams.edges), nrow = N, ncol = P)
rownames(tmp) = williams.coauthors.unique
williams.mat = tmp %*% t(tmp)

author.codegree = williams.mat["SL Williams",]
# 2. static net ####

williams.net = as.network(williams.mat, directed = F, names.eval = "edgeweight", ignore.eval = F)
williams.net%v%"author" = williams.coauthors.unique
plot.network(williams.net, edge.col = "gray", 
             label = "vertex.names", label.cex = .5,
             label.pad = 0, label.pos = 1,
             edge.lwd = williams.net%e%"edgeweight")

williams.igraph = intergraph::asIgraph(williams.net)
williams.layout = data.frame(layout.fruchterman.reingold(williams.igraph))
colnames(williams.layout) = c("x","y")
#os platt?
#sapply(williams.coauthors, str_detect, "Platt")
#which.max(sapply(.Last.value, sum))
#williams[109,]


# 3. dynamic net ####

table(williams$year)
slices = seq(1980,2020, length.out = 9)

author.first = sapply(williams.coauthors.unique,
  function(x) {min(williams[which(sapply(williams.coauthors, function(y) {x %in% y})),"year"], na.rm = T)}
  )

williams.network.list = lapply(slices, function(i) {
  authors.sub = author.first < i
  williams.sort = subset(williams, williams$year <= i)
  N.sub = sum(authors.sub)
  P.sub = nrow(williams.sort)
  williams.edges.sub = lapply(williams.edges[williams$year <= i], function(x) x[authors.sub])
  tmp = matrix(do.call(cbind, williams.edges.sub), nrow = N.sub, ncol = P.sub)
  
  rownames(tmp) = williams.coauthors.unique[authors.sub]
  coauthor.mat = tmp %*% t(tmp)
  coauthor.net = as.network(coauthor.mat, directed = F, names.eval = "edgeweight", ignore.eval = F)
  coauthor.net%v%"author" = williams.coauthors.unique[authors.sub]
  coauthor.net%v%"x" = williams.layout$x[authors.sub]
  coauthor.net%v%"y" = williams.layout$y[authors.sub]
  coauthor.net%v%"vertex.pid" = which(authors.sub)
  return(coauthor.net)
})

names(williams.network.list) = as.character(slices)

williams.dynamic = networkDynamic(base.net=williams.net, network.list = williams.network.list,
                                  vertex.pid = "author", create.TEAs = T, edge.TEA.names="edgeweight")
williams.dynamic 

compute.animation(williams.dynamic, 
                  animation.mode = "useAttribute",
                  slice.par=list(start=0, end=length(slices), interval=1, aggregate.dur=1, rule='any'),
                  weight.attr = c("edgeweight"))


render.d3movie(williams.dynamic, usearrows = F, displaylabels = T,
               vertex.col = "blue",
               vertex.tooltip = paste("<b>Name:</b>", (williams.dynamic %v% "author") ),
               label= "author",
               label.col = "red",
               bg="#ffffff", vertex.border="#333333",
               render.par = list(show.time = TRUE, show.stats = "~edges")
               )
               
               
               vertex.cex = degree(net3)/2,  
               vertex.col = net3.dyn %v% "col",
            
              
               launchBrowser=T, filename="Williams-Network-Dynamic.html",
               plot.par=list(mar=c(0,0,0,0)), output.mode='inline' )
