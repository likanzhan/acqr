dp <- function(id, year = 2018){
    id <- formatC(id, width = 2, format = "d", flag = "0")
    web <- "https://www.frontiersin.org/articles/10.3389/frym."
    #web <- "http://dx.doi.org/10.3389/frym."
    url <- paste(web, year, ".000", id, "/pdf", sep = "")
    # file_name <- paste("frym.", year, ".000", id, ".pdf", sep = "")
    vol <- formatC(year - 2012, width = 2, format = "d", flag = "0")
    file_name <- paste("frym-", vol, "-000", id, ".pdf", sep = "")
    download.file(url, file_name, mode = "wb")
}
#for (i in  1:43) dp(i, year = 2018)
#for (i in 55:71) dp(i, year = 2017)
#try(RCurl::url.exists("https://www.frontiersin.org/articles/10.3389/frym.2017.00072/pdf", .header = TRUE))