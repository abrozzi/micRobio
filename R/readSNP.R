readSNP <- function(file) {

  OBJ = data.frame(
    frequency = NA,
    gene_name = NA,
    gene_position = NA,
    gene_product = NA,
    gene_strand = NA,
    genes_promoter    = NA,
    locus_tag  = NA,
    locus_tags_promoter = NA,
    mutation_category = NA,
    position_end = NA,
    position_start = NA,
    ref_seq = NA,
    snp_type      = NA,
    aa_new_seq = NA,
    aa_position = NA,
    aa_ref_seq = NA,
    codon_new_seq    = NA,
    codon_number = NA,
    codon_position = NA,
    codon_ref_seq = NA,
    genes_overlapping = NA,
    locus_tags_overlapping = NA,
    transl_table = NA
  )

  RL = readLines(file)

  totLines = length(RL)

  for ( i in 1 : totLines ) {

    cat(i,"\n")

    line = unlist(strsplit(RL[i],"\t"))

    start = grep("=", line)[1]
    end   = length(line)

    keys   = sapply(strsplit(line,"="),function(s) s[1])[start:end]
    values = sapply(strsplit(line,"="),function(s) s[2])[start:end]

    idx = match(keys, colnames(OBJ))

    OBJ[,idx] = values
  }

  OBJ = OBJ[-1,]

  return(OBJ)

}
