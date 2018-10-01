# EXPLORATORY check for significant variables in determining leak location
# 10-1-18
# Jane Carlen

# Notes:
# We find no outstanding differences between groups
# Complete analysis would control for confounding variables
# We used permutation tests because of the small sample sizes and bc distributions around a circle can have strange properties
# We used pairwise diffs because we care about grouping and spread around circle not "average" location
# IF visualization shows strong clumping we could examine groupwise diff from average cluster location or other tests. 

# 0. Setup ####
library(readxl)
project.dir = "~/Documents/DSI/med_center_sarah_westcott/"
setwd(project.dir)
leak = read_xlsx("LAA_Project_Data_Consultation.xlsx")
#helper for pairwise diffs on circle
circle.pairwise = function(v) { 
  tmp = as.numeric(dist(v))
  tmp = pmin(tmp, abs(tmp - 360))
  tmp
}

# 1. Test for each (2-factor) var (output in "results" list) ####
# Permutations tests since samples are small and distributions around a circle may have strange properties
# Use pairwise diffs because we care about grouping and spread around circle not "average" location


test.vars = c("shoulder", "m_angio_morphology", "chf", "afib_type", "chads2vasc", "cad",
"dm2", "pad", "htn", "prior_major_bleed")
summary(leak[,test.vars])
for(elem in test.vars) {leak[[elem]] = as.factor( leak[[elem]] )}
summary(leak[,test.vars])

# Note that two variables (m_angio_morphology & chads2vasc) have multiple levels, some with very small sizes.
# I'll ignore those for now
# For afib_type I'll just compare 0 and 2 and exclude the one value with level 1.
leak$afib_type2 = leak$afib_type
levels(leak$afib_type2) = c(0, NA, 1) #note the switch in level reference
test.vars2 = c("shoulder", "chf", "afib_type2", "cad",
               "dm2", "pad", "htn", "prior_major_bleed")

n.perm = 1000 #number of permutations
# assume two groups, A and B
results = vector("list", length(test.vars2))
for (elem in test.vars2) {
  leak2 = leak[!is.na(leak[[elem]]),]
  n = table(leak2[[elem]])[1]
  S = replicate(n.perm, sample(1:nrow(leak2), n))
  
  results[[which(test.vars2==elem)]] = 
    sapply(1:n.perm, function(x) {
        A.pairwise.diffs = circle.pairwise(leak2[S[,x],"s_leak_location"])
        B.pairwise.diffs = circle.pairwise(leak2[-S[,x],"s_leak_location"])
        C.pairwise.diffs = circle.pairwise(leak2[,"s_leak_location"]) #overall
        data.frame( 
                A.mean = mean(A.pairwise.diffs),
                A.var = var(A.pairwise.diffs),
                B.mean = mean(B.pairwise.diffs),
                B.mean = var(B.pairwise.diffs),
                C.mean = mean(C.pairwise.diffs),
                C.mean = var(C.pairwise.diffs)
              )
    })
}

# 2. Visualize results ####

names(results) = test.vars2
par(mfrow = c(1, length(test.vars2)), oma=c(0,0,2,0))

for (elem in test.vars2) {
  hist(unlist(results[[elem]][1,]) - unlist(results[[elem]][3,]), main = elem, breaks = 20) #difference of means, 0 level (A) - other level (B)
  # observed groupwise avg. pairwise diff
  print(c(mean(circle.pairwise(leak[leak[[elem]] == 0, "s_leak_location"]), na.rm = T), 
          mean(circle.pairwise(leak[leak[[elem]] == 1, "s_leak_location"]), na.rm = T)))
  #observed difference in average pairwise difference by group
  print(mean(circle.pairwise(leak[leak[[elem]] == 0, "s_leak_location"]), na.rm = T) -
          mean(circle.pairwise(leak[leak[[elem]] == 1, "s_leak_location"]), na.rm = T))
  abline(v = mean(circle.pairwise(leak[leak[[elem]] == 0, "s_leak_location"]), na.rm = T) - 
           mean(circle.pairwise(leak[leak[[elem]] == 1, "s_leak_location"]), na.rm = T), col = "red")
}
title("Distribution of difference in mean pairwise difference", outer=TRUE)

for (elem in test.vars2) {
  hist(unlist(results[[elem]][2,]) - unlist(results[[elem]][4,]), main =  elem, breaks = 10) #difference of means, 0 level (A) - other level (B)
  # observed groupwise var of pairwise diff
  print(c(var(circle.pairwise(leak[leak[[elem]] == 0, "s_leak_location"]), na.rm = T), 
          var(circle.pairwise(leak[leak[[elem]] == 1, "s_leak_location"]), na.rm = T)))
  #observed difference in variance of pairwise diff by group
  print(var(circle.pairwise(leak[leak[[elem]] == 0, "s_leak_location"]), na.rm = T) -
          var(circle.pairwise(leak[leak[[elem]] == 1, "s_leak_location"]), na.rm = T))
  abline(v = var(circle.pairwise(leak[leak[[elem]] == 0, "s_leak_location"]), na.rm = T) - 
           var(circle.pairwise(leak[leak[[elem]] == 1, "s_leak_location"]), na.rm = T), col = "red")
}
title("Distribution of difference in variance of pairwise differences", outer=TRUE)

# to look at a set of positions broken down by a specific factor
ggplot(leak, aes(x = s_leak_location, fill = htn)) +
  geom_histogram(position = "dodge", binwidth = 20)

##### bonus ####
# compare group 0 to overall
for (elem in test.vars2) {
  hist(unlist(results[[elem]][1,]) - unlist(results[[elem]][5,]), main = elem, breaks = 20) #difference of means, 0 level (A) - other level (B)
  #observed difference in average pairwise difference by group
  print(mean(circle.pairwise(leak[leak[[elem]] == 0, "s_leak_location"]), na.rm = T) -
          mean(circle.pairwise(leak[, "s_leak_location"]), na.rm = T))
  abline(v = mean(circle.pairwise(leak[leak[[elem]] == 0, "s_leak_location"]), na.rm = T) - 
           mean(circle.pairwise(leak[, "s_leak_location"]), na.rm = T), col = "red")
}
title("Distribution of difference in mean pairwise difference", outer=TRUE)

# compare group 1 to overall
for (elem in test.vars2) {
  hist(unlist(results[[elem]][3,]) - unlist(results[[elem]][5,]), main = elem, breaks = 20) #difference of means, 0 level (A) - other level (B)
  #observed difference in average pairwise difference by group
  print(mean(circle.pairwise(leak[leak[[elem]] == 1, "s_leak_location"]), na.rm = T) -
          mean(circle.pairwise(leak[, "s_leak_location"]), na.rm = T))
  abline(v = mean(circle.pairwise(leak[leak[[elem]] == 1, "s_leak_location"]), na.rm = T) - 
           mean(circle.pairwise(leak[, "s_leak_location"]), na.rm = T), col = "red")
}
title("Distribution of difference in mean pairwise difference", outer=TRUE)
