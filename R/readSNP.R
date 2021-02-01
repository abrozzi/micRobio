readSNP <- function(file) {

  RL = readLines(file)

  totLines = length(RL)
  M = matrix (NA, ncol= 23, nrow= totLines)
  colnames(M) = c(
    "aa_new_seq",
    "aa_position",
    "aa_ref_seq",
    "codon_new_seq",
    "codon_number",
    "codon_position",
    "codon_ref_seq",
    "frequency",
    "gene_name",
    "gene_position",
    "gene_product",
    "gene_strand",
    "genes_promoter",
    "locus_tag",
    "locus_tags_promoter",
    "mutation_category",
    "position_end",
    "position_start",
    "ref_seq",
    "snp_type",
    "genes_overlapping",
    "locus_tags_overlapping",
    "transl_table"
  )

  OBJ = as.data.frame(M)

  for ( i in 1 : totLines ) {

    cat(i,"\n")

    line = unlist(strsplit(RL[i],"\t"))

    start = grep("=", line)[1]
    end   = length(line)

    keys   = sapply(strsplit(line,"="),function(s) s[1])[start:end]
    values = sapply(strsplit(line,"="),function(s) s[2])[start:end]

    for ( j in 1:ncol(OBJ)) {
      idx = match(colnames(OBJ)[j], keys)
      if(!is.na(idx)) {
        OBJ[i,j] = values[idx]
      }
    }
  }

  OBJ = OBJ[-1,]

  return(OBJ)

}
