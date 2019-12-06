#' SCI Impact Factors June 2019
#' @importFrom stringr str_to_title
#' @export

# et <- tabulizer::extract_tables("~/Downloads/revistasJCR.pdf", pages = 2:362)
# dt <- do.call(rbind, et)
# dt <- gsub("\r", " ", dt)
# write.csv(dt, "dt.csv")
# JCR2018 <- read.csv("~/Downloads/Journals_in_the_2018_release_of_JCR.csv", stringsAsFactors = FALSE)

# SCI <- read.csv("~/Downloads/SCIimpactfactors-June2019.csv", stringsAsFactors = FALSE)
# SCI[, "Full.Journal.Title"] <- stringr::str_to_title(SCI[, "Full.Journal.Title"])
# SCI <- dplyr::left_join(SCI, JCR2018)

# CAS <- read.csv("~/Downloads/中科院最新SCI分区表.csv", stringsAsFactors = FALSE)
# CAS[, "期刊名"] <- stringr::str_to_title(CAS[, "期刊名"])
# CAS <- dplyr::left_join(CAS, JCR2018, by = list(x = "期刊名", y = "Full.Journal.Title"))
# CAS <- CAS[, -c(13, 14)]
# names(CAS)[11:12] <- c("期刊缩写", "国家地区")
# CAS <- CAS[, c(1, 11:12, 2:10)]
# save(SCI, CAS, file = "Journal_Citation_Index.RData")


IF <- function(journal, exact = TRUE, source = "SCI") {
  data(Journal_Citation_Index, package = "acqr", envir = environment())
  ImpactFactors <- eval(parse(text = source))
  if (source == "SCI") {
    Journal_Name_Column <- "Full.Journal.Title"
  } else if (source == "CAS") {
    Journal_Name_Column <- "期刊名"
  } else {
    warnings("No Such Source !")
  }
  if (is.null(journal)) {
    searched_results <- ImpactFactors
  } else {
    journal <- stringr::str_to_title(journal)
  }
  ImpactFactors[, Journal_Name_Column] <- stringr::str_to_title(ImpactFactors[, Journal_Name_Column])
  if (exact) {
    searched_journals <- journal
  } else {
    searched_journals <- grep(journal, ImpactFactors[, Journal_Name_Column], ignore.case = TRUE, value = TRUE)
  }
  searched_results <- ImpactFactors[ImpactFactors[, Journal_Name_Column] %in% searched_journals, ]
  searched_results <- unique(searched_results)
  searched_results <- as.data.frame(searched_results)
  return(searched_results)
}
