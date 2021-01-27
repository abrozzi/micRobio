# file = "/Users/abrozzi/projects/microbio/inst/JC.tsv"

readJC <- function(file) {

  RL = read.delim(file, sep="\t", header = FALSE, stringsAsFactors = FALSE)

  OBJ = data.frame(V1=NA,
                   V2=NA,
                   V3=NA,
                   V4=NA,
                   V5=NA,
                   V6=NA,
                   V7 = NA,
                   V8 = NA,
                   gene_name=NA,
                   gene_position=NA,
                   gene_product= NA,
                   gene_strand=NA,
                   left_inside_cov=NA,
                   left_outside_cov=NA,
                   locus_tag=NA,
                   right_inside_cov=NA,
                   right_outside_cov=NA
  )


  totLines = nrow(RL)

  for ( i in 1 : totLines ) {

    cat(i,"\n")

    line = as.character(RL[i,])

    field_values = sapply(strsplit(line,"="),function(s) s[2])

    newEntry = c(line[1:8], field_values[9:17])

    OBJ = rbind(OBJ, newEntry)
  }

  OBJ = OBJ[-1,]

  return(OBJ)

}
