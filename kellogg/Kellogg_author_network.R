# Code to create author networks for Louise Kellogg
# Jane Carlen, May 2019
#
# Notes: 
#       1) By default, papers with many authors with truncate the list with "...", byt
#          We fix this using the get_complete_authors function
#          make sure to use the short ID, e.g. "NP_WECMAAAAJ", instead of the full one, "NP_WECMAAAAJ&hl=en"
#       2) Filtered out papers with no year or clearly wrong year for now but someone could go through and try to find right year 
#       3) Sections #1-2 in the code below are now superfluous and don't need to be re-run. 
#         They were used to create an author list csv (KelloggAuthorList_needs_author_cleaning.csv), 
#         that LJH cleaned (KelloggAuthorList_needs_author_cleaning.csv), and we now load in section #3 
#         Our plotting process now starts by loading that LJH csv 
#          
#---------------------------------------------------------------------------------------------------------------------
# 0. Setup ----
library(stringr)
library(statnet)
library(scholar)
library(networkDynamic)
library(ndtv)
library(ergm)
library(igraph)
library(dplyr)
library(intergraph)
library(visNetwork)

setwd("~/Documents/DSI/kellogg")

#helper function to make last names normal format (first letter upper case, rest lower)
lowerName <- function(x) {gsub(x, pattern = "\ ([A-Z]){1}([A-Z]+)", replacement = '\ \\1\\L\\2', perl = TRUE)}
initialName <- function(x) {gsub(x, pattern = "([A-Za-z]){1}([a-z]+)", replacement = '\\1', perl = TRUE)}

#-------------------CAN SKIP SECTIONS 1-2 NOW, THEY JUST CREAT THE DATA WE NOW LOAD IN #3 -----------------------------
# 1. Get author network  ----

#get from saved since code to get can only be run once/day

# kellogg = get_publications("NP_WECMAAAAJ", cstart = 0, pagesize = 100, flush = T) #id from url in google scholar page
kellogg = readRDS("kellogg_scholar_pull.RDS")

# 2. Data cleaning (more needed) -> KelloggAuthorList.csv ----

  # deal with truncated author names. (...) ----
# Note!! google scholar has strict limits on requests so this will only work once/day, and might not get the whole list

ellipsis_indices = grep(kellogg$author, pattern =  "\\.\\.\\.")
#author_complete = sapply(kellogg$pubid[ellipsis_indices], get_complete_authors, id = "NP_WECMAAAAJ")
#saveRDS(object = author_complete, file = "author_complete.RDS") 
author_complete = readRDS("kellogg_author_complete.RDS") 

# Edit author names to agree with format in the original "author" column
# Error in gsub(x, pattern = " ([A-Z]){1}([A-Z]+)", replacement = " \\1\\L\\2",  : input string 1 is invalid UTF-8
# caused by invalid UTF-8. Not sure what to do about those, but errors are bypassed for now
author_complete_reformat = lapply(author_complete, function(elem) {
  split_elem = strsplit(elem, ", ")[[1]]
  split_elem = sapply(split_elem, gsub, pattern = "(\\x.{2})+", replacement ="")
  # Put author names into INITIALS, Lastname format of kellogg$author 
  rename_elem = sapply(split_elem, function(name) {
    
    name2 = iconv(name, "latin1", "ASCII", sub = "") #in case of name like "H J\xfc\xbe\x8d\x86\x94\xbcrvinen"
    name2 = lowerName(name2)
    name2 = strsplit(name2, " ")[[1]]
    
    lastname = last(name2)
    
    if (length(name2) > 1) {
      name2 =  sapply(1:(length(name2)-1), function(i) {initialName(lowerName(name2[i]))})
      name2 = paste(paste(name2, collapse = ""), lastname, sep = " ")
    } else {name2 = lastname}
    return(name2)
    
  })
  return(rename_elem)
})
author_complete_reformat = lapply(author_complete_reformat, paste, collapse = ", ")

# Save original author column as "author_orig" and update the "author column"
kellogg$author_orig = kellogg$author
kellogg$author[ellipsis_indices] = author_complete_reformat

  # fix authors names ----

kellogg.coauthors = sapply(as.character(kellogg$author), strsplit, ", ")

# Specific coauthors names need fixing
kellogg.coauthors = lapply(kellogg.coauthors, lowerName) # reduces 16 redundant capital names
kellogg.coauthors = lapply(kellogg.coauthors, str_replace, pattern = "L Kellogg", replacement = "LH Kellogg")
kellogg.coauthors = lapply(kellogg.coauthors, str_replace, pattern = "L. H. Kellogg", replacement = "LH Kellogg")
kellogg.coauthors = lapply(kellogg.coauthors, str_replace, pattern = "van Aalsburg", replacement = "Van Aalsburg")
kellogg.coauthors = lapply(kellogg.coauthors, str_replace, pattern = "van Aalsburg", replacement = "Van Aalsburg")
kellogg.coauthors = lapply(kellogg.coauthors, str_replace, pattern = "M. B. Yikilmaz", replacement = "MB Yikilmaz")
kellogg.coauthors = lapply(kellogg.coauthors, str_replace, pattern = "MB Yıkılmaz", replacement = "MB Yikilmaz")
kellogg.coauthors = lapply(kellogg.coauthors, str_replace, pattern = "MB YıKıLMAZ", replacement = "MB Yikilmaz")
kellogg.coauthors = lapply(kellogg.coauthors, str_replace, pattern = "T Tullis", replacement = "TE Tullis")
kellogg.coauthors = lapply(kellogg.coauthors, str_replace, pattern = "AE Fish", replacement = "A Fish") 
kellogg.coauthors = lapply(kellogg.coauthors, str_replace, pattern = "LouiseH", replacement = "LH")
kellogg.coauthors = lapply(kellogg.coauthors, str_replace, pattern = "L Hwang", replacement = "LJ Hwang") #right to change all these?

# make sure kellogg in all papers -- most commonly left out due to "..."
kellogg.coauthors = sapply(kellogg.coauthors, function(x) {
  if (! ("LH Kellogg" %in% x)) {
    x = c(x,"LH Kellogg") 
  } else x = x})

kellogg$author = sapply(kellogg.coauthors, paste, collapse = ", ")

  # fix papers with bad years (add missing year or filter out) ----

#Pull in chandi's cleaned years (had to manually fix dumb excell issues with pubid starting with a minus -)
chandni_version = read.csv("KelloggAuthorList_needs_cleaning_cn.csv", stringsAsFactors = FALSE)
kellogg = left_join(kellogg, chandni_version[,c("year", "pubid")], by = "pubid", suffix = c("_pre_clean", ""))
write.csv(kellogg, "KelloggAuthorList_before_year_filter.csv", row.names = FALSE)

#NA years don't have titles
#years marked "remove" are papers from another louise kellogg or otherwise invalidated by chandni's cleaning
kellogg = filter(kellogg, !is.na(year), year !="remove")

  # Rest of cleaning is author names ----

#Use this file to clean
write.csv(kellogg, "KelloggAuthorList_needs_author_cleaning.csv", row.names = FALSE)

# Code to find duplicated last names -- this could help in cleaning
sort(table(sapply(  sapply(unique(unlist(kellogg.coauthors)), str_split, " "), last )))
  # For example, see last names containing Kellogg -- should they be the same person?
  unique(unlist(kellogg.coauthors)[grepl(unlist(kellogg.coauthors), pattern = "Kellogg")])

#-------------------------------------------------------------------------------------------------------------------
# 3. Read in Lorraine's author-cleaned version (based off my needs_cleaning .csv created above) ----  
kellogg = read.csv("KelloggAuthorList_needs_author_cleaning_LJH.csv", stringsAsFactors = F)  

# 4. Co-authors ####

#filtered versions (after filtering outpapers with bad years)
kellogg.coauthors.filtered = sapply(as.character(kellogg$author), strsplit, ", ")
kellogg.coauthors.filtered = lapply(kellogg.coauthors.filtered, trimws)
kellogg.coauthors.unique.filtered = 
  unique(unlist(kellogg.coauthors.filtered))[order(unique(unlist(kellogg.coauthors.filtered)))] #alphabetize

kellogg.edges = lapply(kellogg.coauthors.filtered, function(x) {kellogg.coauthors.unique.filtered %in% x})

# number of single
kellogg.degree = sapply(kellogg.edges, sum); table(kellogg.degree)

N = length(kellogg.coauthors.unique.filtered)
P = length(kellogg.edges)
tmp = matrix(do.call(cbind, kellogg.edges), nrow = N, ncol = P)
rownames(tmp) = kellogg.coauthors.unique.filtered
kellogg.mat = tmp %*% t(tmp)
kellogg.mat = kellogg.mat[order(rownames(kellogg.mat)), order(rownames(kellogg.mat))]

author.codegree = kellogg.mat["LH Kellogg",]
author.codegree2 = log(author.codegree) + .5

  # a. static net ####

#set.seed(3) there is randomness in the layouts
kellogg.net = as.network(kellogg.mat, directed = F, names.eval = "edge.lwd", ignore.eval = F)
kellogg.net%v%"author" = rownames(kellogg.mat)
kellogg.net%v%"vertex.cex" = author.codegree2 #how many papers with kellogg?
kellogg.net%v%"vertex.pid" = 1:length(kellogg.coauthors.unique.filtered)
# for visNetwork
kellogg.net%v%"id" = 1:length(kellogg.coauthors.unique.filtered)
kellogg.net%v%"label" = kellogg.coauthors.unique.filtered
kellogg.net%v%"title" = kellogg.coauthors.unique.filtered
kellogg.net%v%"size" = 10*log(author.codegree + .5) #how many papers with kellogg?

plot.network(kellogg.net, edge.col = "gray", 
             label = "vertex.names", label.cex = .5,
             label.pad = 0, label.pos = 1,
             edge.lwd = kellogg.net%e%"edge.lwd")

kellogg.igraph = intergraph::asIgraph(kellogg.net)
kellogg.layout = data.frame(layout.fruchterman.reingold(kellogg.igraph))
colnames(kellogg.layout) = c("x","y")


  # b. static interactive net ####

nodes <- data.frame(id = kellogg.net%v%"id",
                    label = kellogg.net%v%"label",
                    title = kellogg.net%v%"label",
                    value = kellogg.net%v%"size")

edges <- data.frame(from=data.frame(as.edgelist(kellogg.net))$X1, 
                     to=data.frame(as.edgelist(kellogg.net))$X2)
                     #value = as.edgelist(kellogg.net, as.sna.edgelist = T, attrname = "edge.lwd")[,3]

kellogg_interactive = visNetwork(nodes, edges, submain = "Louise Kellogg's Co-Author Network", width = 800, height = 800) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = as.matrix(kellogg.layout), type = "full") %>%
  visNodes(color = list(highlight = "red", hover = list(border = "red"))) %>%
  visEdges(selectionWidth = 5, scaling = list(enabled = TRUE, min = 0, max = 20)) %>%
  visOptions(nodesIdSelection = list(enabled  = TRUE, useLabels = TRUE, main = "Select by Author"),
             highlightNearest = list(enabled = TRUE, degree = 1, 
                                     hideColor =  "#C8C8C8", algorithm = "hierarchical")) %>%
  visInteraction(selectConnectedEdges = FALSE)

visSave(kellogg_interactive, "Kellogg_coauthor_network.html", selfcontained = T)
# check
# unique(unlist(kellogg.coauthors.filtered[grepl(pattern = "LJ Hwang", names(kellogg.coauthors.filtered))]))

  # c. dynamic net ####

kellogg$year = as.numeric(kellogg$year)
table(kellogg$year)
slices = seq(min(kellogg$year), max(kellogg$year), by = 1)
start1= min(slices); end1 = max(slices)
author.first = sapply(kellogg.coauthors.unique.filtered,
                      function(x) {
                        min(kellogg[which(sapply(kellogg.coauthors.filtered, function(y) {x %in% y})),"year"], na.rm = T)
                      })

kellogg.network.list = lapply(slices, function(i) {
  authors.sub = author.first <= i
  kellogg.sort = subset(kellogg, kellogg$year <= i)
  N.sub = sum(authors.sub)
  P.sub = nrow(kellogg.sort)
  kellogg.edges.sub = lapply(kellogg.edges[kellogg$year <= i], function(x) x[authors.sub])
  tmp = matrix(do.call(cbind, kellogg.edges.sub), nrow = N.sub, ncol = P.sub)
  
  rownames(tmp) = kellogg.coauthors.unique.filtered[authors.sub]
  coauthor.mat = tmp %*% t(tmp)
  coauthor.net = as.network(coauthor.mat, directed = F, names.eval = "edge.lwd", ignore.eval = F)
  coauthor.net%v%"author" = kellogg.coauthors.unique.filtered[authors.sub]
  coauthor.net%v%"x" = kellogg.layout$x[authors.sub]
  coauthor.net%v%"y" = kellogg.layout$y[authors.sub]
  coauthor.net%v%"vertex.pid" = which(authors.sub)
  coauthor.net%v%"vertex.cex" = author.codegree[authors.sub]
  return(coauthor.net)
})

tmp = vector(mode = "list", length=min(slices)-1)
tmp = lapply(tmp, function(x) kellogg.network.list[[1]])
kellogg.network.list = c(tmp, kellogg.network.list)

kellogg.dynamic = networkDynamic(base.net=kellogg.net,
                                 network.list = kellogg.network.list,
                                 vertex.pid = "vertex.pid", create.TEAs = T)
kellogg.dynamic 

compute.animation(kellogg.dynamic, animation.mode = "useAttribute",
                  slice.par=list(start=start1, end=end1, interval=1, aggregate.dur = 1, rule='latest'),
                  weight.attr = c("edge.lwd"))

render.d3movie(kellogg.dynamic, usearrows = F, displaylabels = T,
               label= "author",
               vertex.cex = "vertex.cex",
               vertex.tooltip=paste(kellogg.net%v%'author', sep = "<br>"),
               label.col = "white",
               label.cex = .8,
               vertex.col = "skyblue4",
               edge.col = "navy",
               edge.lwd = "edge.lwd",
               main = "Louise Kellogg Co-Author Network over Time: 1986 - 2019",
               xlab = "test",
               bg="black",
               vertex.border="#333333",
               render.par = list(show.time = TRUE, show.stats = "~edges"),
               launchBrowser=F, filename="Kellogg_coauthor_over_time.html", 
               d3.options = list(slider = TRUE))

