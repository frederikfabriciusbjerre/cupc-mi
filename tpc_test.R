# ================================= A test case =================================
library(pcalg)
library(tpc)
library(graph)
library(MASS)
library(tictoc)
library(igraph)
library(mice)
library(micd)
library(miceadds)

source("cuPCMI.R")

# read data as imputed_data
dataset_path <- file.path("dataset_imputed/dataset_imputed.Rdata", fsep = .Platform$file.sep)
load.Rdata(dataset_path, "imputed_data")

# make suffStat
suffStatMI <- micd::getSuff(imputed_data, test="gaussMItest") 

# input params to pc
p <- imputed_data[[1]] %>% length()
alpha <- 0.01
max_order <- 10

cat("Fitting with alpha =", alpha, "\n")
tic()
cuPCMI_fit <- cu_pc_MI(suffStatMI, p = p, alpha = alpha, m.max = max_order, maj.rule = FALSE)
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
micd_PC <- tpc(suffStatMICD, indepTest = gaussMItest, p = p, alpha = alpha,
                skel.method = "stable", m.max = max_order, tiers=NULL,
                conservative = FALSE, maj.rule = TRUE)
print("The total time consumed by micd_PC is:")
toc()
cat("\n")
cat("micd_PC\n")
print(micd_PC)
cat("\n")
cat("micdPC ord:", micd_PC@max.ord, "\n")
