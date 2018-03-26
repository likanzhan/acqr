Modified_Frequency_Distribution_Mean <- function (sample, mu = 10, sst = FALSE,...
){
acqr::Modified_Frequency_Distribution(sample, ...)	
if (sst){
 center <- mu;
 mtext(side = 3, at = 10, text = bquote(italic(mu[0]) == .(mu)))	
} else {
 center <- mean(sample)	
}
mtext(side = 3, at = mean(sample), text = bquote(italic(M) == .(round(mean(sample), 2))) )
ssample <- sample[sample != center]
unique <- unique(ssample)
length <- length(unique)
segments(
  x0 = unique, y0 = as.numeric(table(ssample)),
  x1 = unique, y1 = seq(max(table(ssample)) + 0.1 , max(table(ssample)) + 0.9, length.out = length))
segments(
  x0 = center, y0 = length(sample[sample == center]), x1 = center, y1 = max(table(ssample)) + 1 )
segments(
  x0 = unique, y0 = seq(max(table(ssample)) + 0.1 , max(table(ssample)) + 0.9, length.out = length),
  x1 = rep(center, length), y1 = seq(max(table(ssample)) + 0.1 , max(table(ssample)) + 0.9, length.out = length),
  col = "red")	
}