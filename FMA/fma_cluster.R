### Frequentist Model Averaging Using Cluster-Robust Standard Errors ###

library(foreign)
library(xtable)
library(LowRankQP)

# Read data (df_FMA has to contain study_id)
df_FMA <- df_FMA


run_FMA <- function(df_FMA){


# Read cluster IDs
cluster_id <- df_FMA$study_id
df_FMA$study_id <- NULL


# Prepare independent variables
x.data <- df_FMA[,-1] 
const_ <- rep(1, nrow(df_FMA)) 
x.data <- cbind(const_, x.data) 
x <- sapply(1:ncol(x.data), function(i) x.data[, i] / max(x.data[, i]))
scale.vector <- as.matrix(sapply(1:ncol(x.data), function(i) max(x.data[, i]))) 
Y <- as.matrix(df_FMA[, 1]) 
output.colnames <- colnames(x.data)



# Full model fit
full.fit <- lm(Y ~ x - 1) 
beta.full <- as.matrix(coef(full.fit)) 
M <- k <- ncol(x) 
n <- nrow(x) 
beta <- matrix(0, k, M)
e <- matrix(0, n, M) 
K_vector <- matrix(1:M) 
var.matrix <- matrix(0, k, M)
bias.sq <- matrix(0, k, M) 



# Cluster-robust vcov 
cluster_vcov <- function(X, e, cluster) { 
  cluster <- as.factor(cluster)
  n <- nrow(X)
  k <- ncol(X)
  G <- length(unique(cluster))
  dfc <- G / (G - 1) * (n - 1) / (n - k) 
  u <- X * e 
  cluster_levels <- levels(cluster)
  cluster_sum <- matrix(0, nrow = G, ncol = k)
  for (j in 1:G) { 
    cl <- cluster_levels[j]
    cluster_sum[j, ] <- colSums(u[cluster == cl, , drop = FALSE])
  }
  meat <- t(cluster_sum) %*% cluster_sum  
  bread_inv <- solve(t(X) %*% X)          
  vcov_cl <- dfc * bread_inv %*% meat %*% bread_inv
  return(vcov_cl)
}



# Model averaging loop
for (i in 1:M) { 
  X <- as.matrix(x[, 1:i]) 
  ortho <- eigen(t(X) %*% X) 
  Q <- ortho$vectors 
  lambda <- ortho$values 
  x.tilda <- X %*% Q %*% diag(lambda^(-0.5), i, i)
  beta.star <- t(x.tilda) %*% Y 
  beta.hat <- Q %*% diag(lambda^(-0.5), i, i) %*% beta.star 
  beta[1:i, i] <- beta.hat
  e[, i] <- Y - x.tilda %*% beta.star
  bias.sq[, i] <- (beta[, i] - beta.full)^2
 
   # Clustered SEs
  e_i <- e[, i] 
  cl_vcov <- cluster_vcov(X, e_i, cluster_id) 
  var.matrix[1:i, i] <- diag(cl_vcov)
  var.matrix[1:i, i] <- var.matrix[1:i, i] + bias.sq[1:i, i] 
}


# Model weights via QP
e_k <- e[, M] 
sigma_hat <- as.numeric((t(e_k) %*% e_k) / (n - M)) 
G <- t(e) %*% e
a <- sigma_hat^2 * K_vector 
A <- matrix(1, 1, M) 
b <- matrix(1, 1, 1) 
u <- matrix(1, M, 1)
optim <- LowRankQP(Vmat = G, dvec = a, Amat = A, bvec = b, uvec = u, method = "LU", verbose = FALSE) 
weights <- as.matrix(optim$alpha) 


# Final estimates
beta.scaled <- beta %*% weights
final.beta <- beta.scaled / scale.vector
std.scaled <- sqrt(var.matrix) %*% weights
final.std <- std.scaled / scale.vector
results.reduced <- cbind(final.beta, final.std)
rownames(results.reduced) <- output.colnames
colnames(results.reduced) <- c("Coefficient", "Sd. Err")


# P-values and formatting
MMA.fls <- round(results.reduced, 4)
MMA.fls <- data.frame(MMA.fls)
t <- MMA.fls$Coefficient / MMA.fls$Sd..Err
MMA.fls$pv <- round((1 - pnorm(abs(t))) * 2, 3)
MMA.fls$names <- rownames(MMA.fls)
# Ensure correct row order
names <- c(colnames(df_FMA), "const_")
MMA.fls <- MMA.fls[match(names, MMA.fls$names), ]
MMA.fls$names <- NULL

# Output results
return(MMA.fls)
}

run_FMA(df_FMA)











