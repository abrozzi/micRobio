#'
#' @export
#' @author Alessandro Brozzi
#' @param a gd.file
#' @return a list of one item "SNP" containing a dataframe with data extracted from SNP lines
#'
read.breseq.gd <- function(file.gd) {

  RES = list()

  types = c("SNP", "SUB", "DEL", "INS", "MOB", "AMP", "CON", "INV", "RA", "MC", "JC", "UN", "TSEQ", "PFLP", "RFLP", "PFGE", "PHYL", "CURA")

  fileOuts = c()

    for (ty in types) {

      fileOut = paste0(tempfile(), "_", ty)

      system(paste0("grep ^", ty, " ", file.gd, " > ", fileOut))

      fileOuts = c(fileOuts,fileOut)
  }

  for ( file in fileOuts ) {

    type = unlist(strsplit(basename(file),"_"))[2]

    if(type=="SNP") {

      RES[["SNP"]] = readSNP(file)

    } else { RES[[type]] = NULL}

  }

  return(RES)

}
