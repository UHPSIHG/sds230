#' A permutation test and plotting function
#' 
#' This function performs a permutation test for the correlation of two samples, returning the p-value associated with your original Pearson's correlation, relative to null permutation distribution.
#' @param x One continuous variable
#' @param y Another continuous variable
#' @param n_samp The number of permutation iterations. Defaults to 10000
#' @param plot Return a histogram of the permutation distribution with your sample correlation? Defaults to TRUE
#' @keywords permutation
#' @export
#' @examples
#' x <- 1:100
#' y <- jitter(x, factor=100)
#' permCor <- function(x, y, n_samp = 1000, plot = T)


permCor <- function(x, y, n_samp = 10000, plot = T){
  corResults <- rep(NA, n_samp)
  for (i in 1:n_samp){
    corResults[i] <- cor(x, sample(y))
  }
  pval <- mean(abs(corResults) >= cor(x,y))
  if (plot == T){
    #Make histogram of permuted correlations
    hist(corResults, col = "yellow", main = "", xlab = "Correlations", breaks = 50,
         xlim = range(corResults,cor(x,y)))
    mtext("Permuted Sample Correlations", cex = 1.2, line = 1)
    mtext(paste("Permuted P-value =",round(pval,5)), cex = 1, line = 0)
    abline(v = cor(x,y), col="blue", lwd=3)
    text(cor(x,y)*1.01, 0,paste("Actual Correlation =", round(cor(x,y),2)),srt = 90, adj = 0)
  }
  if (plot == F){
    return(round(pval,5))
  }  
}
