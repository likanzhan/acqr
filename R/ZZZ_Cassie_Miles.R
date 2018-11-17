#' Miles for Cassie
#' @export
#' @importFrom openxlsx read.xlsx
#' @importFrom readr write_excel_csv

mls <- function(Directory = "~/Downloads/mls") {
	if (!dir.exists(Directory)) stop(paste0("路径 ",  dirname(Directory), " 下，文件夹 ",  basename(Directory), " 不存在"))
    filelist <- list.files(Directory, pattern = "^餐饮专票", recursive = FALSE, full.names = TRUE)
    if (length(filelist) == 0) stop(paste0("文件夹 ",  basename(Directory), " 内没有可分析的文件"))
    datalist <- lapply(filelist, function(x) openxlsx::read.xlsx(x))
    datafr <- do.call("rbind", datalist)
    datafr[, "佣金总和"] <- as.numeric(datafr[, "佣金总和"])
    SMR <- aggregate(datafr["佣金总和"], datafr["商户名（producer）"], sum)
    File_Name <- paste0(abbreviate("Commission"), "_", paste(range(datafr[, "时间周期"]), collapse = "_"), ".csv")
    readr::write_excel_csv(SMR, file.path(Directory, File_Name))
}