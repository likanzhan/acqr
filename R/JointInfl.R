JointInfl <- function(
n = 5,
slope = -1, 
NP = c(n + 1, 0, n+1, -1), 
adj = 1
){
XO <- 1:n
YO <- slope * XO
Points <- matrix(NP, nrow = 2, byrow = FALSE)
XN <- Points[1, ]
YN <- Points[2, ]
XT <- c(XN, XO)
YT <- c(YN, YO)
plot(YT ~ XT, cex = 2.5,
     xlim = c(min(XT) - adj, max(XT) + adj), 
     ylim = c(min(YT) - adj, max(YT) + adj),
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", font.lab = 2, 
     pch = ifelse(XT %in% XO, 1, c(10, 19)[1:length(XN)])
     )
for (i in 0:length(XN)) {
	abline(
	lm(c(YO, YN[0:i]) ~ c(XO, XN[0:i])), 
	lty = ifelse(i %in% c(0, length(XN)), 1, 1 + i),
	lwd = ifelse(i == length(XN), 2, 1)
	)
}
mtext("Y", side = 2, at = max(YT) + adj, las = 2, adj = 1.5)
mtext("X", side = 1, at = max(XT) + adj, las = 1, padj = 0.2)	
}
