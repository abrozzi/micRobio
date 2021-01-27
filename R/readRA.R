readRA <- function(file) {

  RL = read.delim(file, sep="\t", header = FALSE, stringsAsFactors = FALSE)

  OBJ = data.frame(
                 V1= NA,
                 V2= NA,
                 V3= NA,
                 V4= NA,
                 V5= NA,
                 V6= NA,
                 V7= NA,
                 V8= NA,
                 bias_e_value = NA,
                 bias_p_value= NA,
                 consensus_score= NA,
                 fisher_strand_p_value= NA,
                 frequency= NA,
                 gene_name= NA,
                 gene_position= NA,
                 gene_product= NA,
                 gene_strand= NA,
                 ks_quality_p_value= NA,
                 locus_tag= NA,
                 major_base= NA,
                 major_cov= NA,
                 major_frequency= NA,
                 minor_base= NA,
                 minor_cov= NA,
                 new_cov= NA,
                 polymorphism_frequency= NA,
                 polymorphism_score= NA,
                 prediction= NA,
                 ref_cov= NA,
                 snp_type= NA,
                 total_cov= NA
                 )

  totLines = nrow(RL)

  for ( i in 1 : totLines ) {

    cat(i,"\n")

    line = as.character(RL[i,])

    field_values = sapply(strsplit(line,"="),function(s) s[2])

    newEntry = c(line[1:8], field_values[9:31])

    OBJ = rbind(OBJ, newEntry)
  }

  OBJ = OBJ[-1,]

  return(OBJ)

}
