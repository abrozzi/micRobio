subset.nexus <- function(nexus.file, tips, outfile, fromBIGSdb=FALSE){

  file = readLines(nexus.file)
  start.MATRIX = match("MATRIX", file)
  ends = which(file == "END;")
  end.MATRIX = ends[length(ends)]-2

  L = list()
  tips=c()

  for(i in (start.MATRIX+1):(end.MATRIX-1)) {

  tip=NA

  line = unlist(strsplit(file[i],"\\\t"))

  if(fromBIGSdb){

    tip = unlist(strsplit(line,"\\|"))[2]
    tips = c(tips, tip)

  } else {

    tip=line[1]
    tips = c(tips, tip)

  }

  L[[tip]]=line


}

} # closes fun
