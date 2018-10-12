mls <- function(path = "~/Downloads/mls") {
    filelist <- list.files(path, pattern = "餐饮专票", recursive = FALSE, full.names = TRUE)
    datalist <- lapply(filelist, function(x) openxlsx::read.xlsx(x))
    datafr <- do.call("rbind", datalist)
    datafr[, "佣金总和"] <- as.numeric(datafr[, "佣金总和"])
    SMR <- aggregate(datafr["佣金总和"], datafr["商户名（producer）"], sum)
    readr::write_excel_csv(SMR, dir.create(path, "Miles.csv"))	
}
