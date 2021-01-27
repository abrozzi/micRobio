readINS <- function(file) {

  RL = read.delim(file, sep="\t", header = FALSE, stringsAsFactors = FALSE)

  OBJ = data.frame(V1=NA,
                   V2=NA,
                   V3=NA,
                   V4=NA,
                   V5=NA,
                   V6=NA,
                   frequency=NA,
                   gene_name=NA,
                   gene_position=NA,
                   gene_product=NA,
                   gene_strand=NA,
                   genes_promoter=NA,
                   insert_position=NA,
                   locus_tag=NA,
                   locus_tags_promoter=NA,
                   mutation_category=NA,
                   position_end=NA,
                   position_start=NA,
                   ref_seq=NA
  )


  totLines = nrow(RL)

  for ( i in 1 : totLines ) {

    cat(i,"\n")

    line = as.character(RL[i,])

    field_values = sapply(strsplit(line,"="),function(s) s[2])

    newEntry = c(line[1:6], field_values[7:19])

    OBJ = rbind(OBJ, newEntry)
  }

  OBJ = OBJ[-1,]

  return(OBJ)


}
