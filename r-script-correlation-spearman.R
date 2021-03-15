# =================================
#   Spearman's (rank) correlation 
# =================================

# install packages
install.packages("ggplot2")

# ---------------------------------
# libraries
library(ggplot2)

# ---------------------------------
# set directory 
setwd("/home/visnu/MEGA/projects/icddrb/projects_dsdc/task_correlation_dsdc")
getwd()

# ---------------------------------
# import 
spc <- read.table(file.choose(), h = T)
spc <- read.csv(file.choose(), h = T)


# ---------------------------------
# others
attach(spc)
names(spc)
View(spc)

# ---------------------------------
# Assumption 
# 1: Samples consist of related pairs

# Spearman's rank order correlation (rho) measures 
# the strength (and direction) of association 
# between two variables
# while, 
# Correlation does NOT equal causation!

cor.test(Forest_size, Highest_tree, 
         method="spearman", data = spc)
# Interpretation e.g.: 
#   In this analysis, we can see that there 
# is a strong positive relationship
# (Spearman’s rho value 0.91) between 
# the forest size and the tree height, 
# where p-value is < 2.2×10^16 (<0.001). 


# ---------------------------------
# Visualization
qplot(Forest_size, Highest_tree, data = spc, 
      geom="point",
      xlab= expression(" Forest size " ~(ha)),
      ylab= expression(" Highest tree " ~(m)),
      size=I(2))+
  theme_bw()


# ---------------------------------
# ---------------------------------
# task: correlation - spearman 
# at office 20210315

cor.test(MPO, NEO, method="spearman", data = spc) # 0.0008445
cor.test(MPO, A1AT, method="spearman", data = spc) # 0.000887
cor.test(MPO, Total.Neu.3, method="spearman", data = spc) # 1.514e-09
cor.test(MPO, Lactulose, method="spearman", data = spc) # 0.9716
cor.test(MPO, Mannitol, method="spearman", data = spc) # 0.7133
cor.test(MPO, LM.ratio, method="spearman", data = spc) # 0.805
cor.test(MPO, LAZ.score, method="spearman", data = spc) # 0.561
cor.test(MPO, IAP, method="spearman", data = spc) # 0.0002094
cor.test(MPO, Neu.3.activity, method="spearman", data = spc) # 0.3544
cor.test(MPO, Claudin.3, method="spearman", data = spc) # 0.2583
cor.test(MPO, Occludin, method="spearman", data = spc) # 0.8294
cor.test(MPO, Claudin.1, method="spearman", data = spc) # 0.5003
cor.test(MPO, Zonulin, method="spearman", data = spc) # 0.01124

cor.test(NEO, A1AT, method="spearman", data = spc) # 0.7901
cor.test(NEO, Total.Neu.3, method="spearman", data = spc) # 0.1617
cor.test(NEO, Lactulose, method="spearman", data = spc) # 0.05848
cor.test(NEO, Mannitol, method="spearman", data = spc) # 0.04764
cor.test(NEO, LM.ratio, method="spearman", data = spc) # 0.6509
cor.test(NEO, LAZ.score, method="spearman", data = spc) # 0.2935
cor.test(NEO, IAP, method="spearman", data = spc) # 0.689
cor.test(NEO, Neu.3.activity, method="spearman", data = spc) # 0.6756
cor.test(NEO, Claudin.3, method="spearman", data = spc) # 0.9621
cor.test(NEO, Occludin, method="spearman", data = spc) # 0.04675
cor.test(NEO, Claudin.1, method="spearman", data = spc) # 0.4023
cor.test(NEO, Zonulin, method="spearman", data = spc) # 0.7148

cor.test(A1AT, Total.Neu.3, method="spearman", data = spc) # 0.3117
cor.test(A1AT, Lactulose, method="spearman", data = spc) # 0.8962
cor.test(A1AT, Mannitol, method="spearman", data = spc) # 0.2101
cor.test(A1AT, LM.ratio, method="spearman", data = spc) # 0.3143
cor.test(A1AT, LAZ.score, method="spearman", data = spc) # 0.5182
cor.test(A1AT, IAP, method="spearman", data = spc) # 4.558e-07
cor.test(A1AT, Neu.3.activity, method="spearman", data = spc) # 0.8818
cor.test(A1AT, Claudin.3, method="spearman", data = spc) # 0.157
cor.test(A1AT, Occludin, method="spearman", data = spc) # 0.9213
cor.test(A1AT, Claudin.1, method="spearman", data = spc) # 0.02641
cor.test(A1AT, Zonulin, method="spearman", data = spc) # 0.1354

cor.test(Total.Neu.3, Lactulose, method="spearman", data = spc) # 0.8566
cor.test(Total.Neu.3, Mannitol, method="spearman", data = spc) # 0.3983
cor.test(Total.Neu.3, LM.ratio, method="spearman", data = spc) # 0.1747
cor.test(Total.Neu.3, LAZ.score, method="spearman", data = spc) # 0.3918
cor.test(Total.Neu.3, IAP, method="spearman", data = spc) # 0.4533
cor.test(Total.Neu.3, Neu.3.activity, method="spearman", data = spc) # 0.2513
cor.test(Total.Neu.3, Claudin.3, method="spearman", data = spc) # 0.04418
cor.test(Total.Neu.3, Occludin, method="spearman", data = spc) # 0.8455
cor.test(Total.Neu.3, Claudin.1, method="spearman", data = spc) # 0.3534
cor.test(Total.Neu.3, Zonulin, method="spearman", data = spc) # 0.01271

cor.test(Lactulose, Mannitol, method="spearman", data = spc) # < 2.2e-16
cor.test(Lactulose, LM.ratio, method="spearman", data = spc) # 1.973e-08
cor.test(Lactulose, LAZ.score, method="spearman", data = spc) # 0.1909
cor.test(Lactulose, IAP, method="spearman", data = spc) # 0.7062
cor.test(Lactulose, Neu.3.activity, method="spearman", data = spc) # 0.6385
cor.test(Lactulose, Claudin.3, method="spearman", data = spc) # 0.5943
cor.test(Lactulose, Occludin, method="spearman", data = spc) # 0.4611
cor.test(Lactulose, Claudin.1, method="spearman", data = spc) # 0.9818
cor.test(Lactulose, Zonulin, method="spearman", data = spc) # 0.6832

cor.test(Mannitol, LM.ratio, method="spearman", data = spc) # 3.278e-06
cor.test(Mannitol, LAZ.score, method="spearman", data = spc) # 0.003329
cor.test(Mannitol, IAP, method="spearman", data = spc) # 0.4519
cor.test(Mannitol, Neu.3.activity, method="spearman", data = spc) # 0.937
cor.test(Mannitol, Claudin.3, method="spearman", data = spc) # 0.9168
cor.test(Mannitol, Occludin, method="spearman", data = spc) # 0.8095
cor.test(Mannitol, Claudin.1, method="spearman", data = spc) # 0.8295
cor.test(Mannitol, Zonulin, method="spearman", data = spc) # 0.1785

cor.test(LM.ratio, LAZ.score, method="spearman", data = spc) # 0.05669
cor.test(LM.ratio, IAP, method="spearman", data = spc) # 0.1322
cor.test(LM.ratio, Neu.3.activity, method="spearman", data = spc) # 0.7076
cor.test(LM.ratio, Claudin.3, method="spearman", data = spc) # 0.1076
cor.test(LM.ratio, Occludin, method="spearman", data = spc) # 0.9151
cor.test(LM.ratio, Claudin.1, method="spearman", data = spc) # 0.6437
cor.test(LM.ratio, Zonulin, method="spearman", data = spc) # 0.0235

cor.test(LAZ.score, IAP, method="spearman", data = spc) # 0.2686
cor.test(LAZ.score, Neu.3.activity, method="spearman", data = spc) # 0.1927
cor.test(LAZ.score, Claudin.3, method="spearman", data = spc) # 0.8317
cor.test(LAZ.score, Occludin, method="spearman", data = spc) # 0.7077
cor.test(LAZ.score, Claudin.1, method="spearman", data = spc) # 0.2361
cor.test(LAZ.score, Zonulin, method="spearman", data = spc) # 0.1786

cor.test(IAP, Neu.3.activity, method="spearman", data = spc) # 0.1812
cor.test(IAP, Claudin.3, method="spearman", data = spc) # 0.05198
cor.test(IAP, Occludin, method="spearman", data = spc) # 0.581
cor.test(IAP, Claudin.1, method="spearman", data = spc) # 0.04375
cor.test(IAP, Zonulin, method="spearman", data = spc) # 0.8903

cor.test(Neu.3.activity, Claudin.3, method="spearman", data = spc) # 0.4805
cor.test(Neu.3.activity, Occludin, method="spearman", data = spc) # 0.4475
cor.test(Neu.3.activity, Claudin.1, method="spearman", data = spc) # 0.5484
cor.test(Neu.3.activity, Zonulin, method="spearman", data = spc) # 0.312

cor.test(Claudin.3, Occludin, method="spearman", data = spc) # 3.57e-09
cor.test(Claudin.3, Claudin.1, method="spearman", data = spc) # 0.2443
cor.test(Claudin.3, Zonulin, method="spearman", data = spc) # 0.9331

cor.test(Occludin, Claudin.1, method="spearman", data = spc) # 0.006355
cor.test(Occludin, Zonulin, method="spearman", data = spc) # 0.3143

cor.test(Claudin.1, Zonulin, method="spearman", data = spc) # 1.464e-15
