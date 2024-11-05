# ================================= A test case =================================
library(pcalg)
library(graph)
library(MASS)
library(tictoc)
library(igraph)
library(mice)
library(micd)
library(miceadds)
library(dplyr)
source("cuPCMI.R")

# read data as imputed_data
dataset_path <- file.path("dataset_imputed/dataset_imputed.Rdata", fsep = .Platform$file.sep)
load.Rdata(dataset_path, "imputed_data")

# make suffStat
suffStatMI <- micd::getSuff(imputed_data, test="gaussMItest") 

# input params to pc
p <- imputed_data[[1]] %>% length()
alpha <- 0.05
max_order <- 2

cat("Fitting with alpha =", alpha, "\n")
tic()
cuPCMI_fit <- cu_pc_MI(suffStatMI, p = p, alpha = alpha, m.max = max_order)
cat("\n")
cat("cuPCMI\n")
print(cuPCMI_fit)
cat("\n")
cat("cuPC ord =", cuPCMI_fit@max.ord, "\n")
cat("alpha    =", alpha, "\n\n")
cat("Time consumed:\n")
toc()
cat("\n")

suffStatMICD <- micd::getSuff(imputed_data, test="gaussMItest")
tic()
micd_PC <- pc(suffStatMICD, indepTest = gaussMItest, p = p, alpha = alpha, skel.method = "stable.fast", m.max = max_order)
print("The total time consumed by micd_PC is:")
toc()
cat("\n")
cat("micd_PC\n")
print(micd_PC)
cat("\n")
cat("micdPC ord:", micd_PC@max.ord, "\n")

mask <- near(micd_PC@pMax, cuPCMI_fit@pMax, tol = 0.001)
lst1 <- micd_PC@pMax[!mask]
lst2 <- cuPCMI_fit@pMax[!mask]
h <- 0
j <- 0
for (i in 1:length(lst1)){
    if (lst2[i] != -Inf && lst1[i] != -1){
        cat(lst1[i], lst2[i], "\n")
        h <- h + 1
    }
    j <- j + 1
}
cat("Number of pvals not equal             : ", j, "\n")
cat("Number of pvals not equal without -Inf: ", h, "\n")