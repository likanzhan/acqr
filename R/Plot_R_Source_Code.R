Plot_R_Source_Code <- function (
directory = "~/Documents/ADMIN/R-Source-Code/R-3.5.1/src"
){
#dir <- "https://github.com/wch/r-source/src"
rfile <- list.files(path = directory, full.names = TRUE, recursive = TRUE, pattern = "\\.R$")
cfile <- list.files(path = directory, full.names = TRUE, recursive = TRUE, pattern = "\\.c$")
ffile <- list.files(path = directory, full.names = TRUE, recursive = TRUE, pattern = "\\.f$")
numbers <- c(length(rfile), length(cfile), length(ffile))
propnum <- prop.table(numbers)

rline <- sum(sapply(rfile, R.utils::countLines))
cline <- sum(sapply(cfile, R.utils::countLines))
fline <- sum(sapply(ffile, R.utils::countLines))
lines <- c(rline, cline, fline)
propline <- prop.table(lines)

par(mfrow = c(1, 2))
barplot(propnum, names.arg = c("r", "C", "Fortran"), main = "File Numbers")
text(x = 0.5 + 0.2 + 1.2 * (0:2), y = propnum, labels = numbers, pos = 1)
barplot(propline, names.arg = c("r", "C", "Fortran"), main = "Code Lines")
text(x = 0.5 + 0.2 + 1.2 * (0:2), y = propline, labels = lines, pos = 1)
}
#Plot_R_Source_Code()