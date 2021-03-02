# ============================
# MERGING AND CLEANING DATA 
# ============================

# importing data 
dataRaw <-read.csv(file.choose(),header=T) 

dataWashMain <- dataRaw
dataVaxTiter <- dataRaw

View(dataWashMain)
View(dataVaxTiter)

dataVaxTiter$pid <- dataVaxTiter$Pid
