#' @title Color SplitsTree dendrograms
#' @description Reads a nexus file and adds colors to the leaves
#' @param nexus.file
#' @param tips (leaves of the dendrogram)
#' @param colors
#' @return a new nexus file with color
#' @examples
#' inputfile <- file.path(path.package('microbio'),'input.nexus')
#' outfile <- file.path(path.package('microbio'),'output.nexus')
#' tips <- c("A","R","G","B")
#' col <- c("red","blue","green","yellow")
#' add.nex.features(nexus.file=inputfile, tips=tips, colors=col, outfile=outfile)
#' @export
#'
add.nex.features <- function(nexus.file, tips, colors, outfile,fromBIGSdb=FALSE) {

  file = readLines(nexus.file)
  startTRANS = match("TRANSLATE", file)
  ends = which(file == ";")
  endTRANS = ends[ends>startTRANS][1]

  map=list()

  for(i in (startTRANS+1):(endTRANS-1)) {
    line = file[i]
    item0 = unlist(strsplit(line," "))

    key = item0[1]

    values0 = item0[2:length(item0)]

    values1 = sapply(values0, function(x) gsub(x=x, pat=",", rep=""))
    values2 = sapply(values1, function(x) gsub(x=x, pat="'", rep=""))

    if(fromBIGSdb){
      values3 = sapply(values2, function(x) unlist(strsplit(x,"\\|"))[2])
    } else {
      values3 = values2
      }

    map[[key]] = values3

  }

map.df = data.frame(vertex_id = rep(names(map), sapply(map, length)), tips=unlist(map), stringsAsFactors = FALSE)
map.df$vertex_id = as.character(map.df$vertex_id)
rownames(map.df)=NULL

if( length(tips)!=nrow(map.df) ) {
  stop("'tips' must be a character vector of the same length of taxa")
  }

if( any(is.na(match(tips, map.df$tips))) ) {
  stop("tips and taxa don't match")
  }

if( !is.character(colors) ) {
  stop("'colors' must be a character vector of the same length of tips")
  }

if( length(tips)!=length(colors) ) {
  stop("'colors' must be a character vector of the same length of tips")
  }

map.df$color = colors[match(tips, map.df$tips)]
map.df$color = as.character(map.df$color)

start.VERTICES = match("VERTICES", file)
end.VERTICES = ends[ends>start.VERTICES][1]

for (i in 1:length(file)){
  lineToPrint = file[i]

  if(i > (start.VERTICES+1) & i < (end.VERTICES-1) ) {

    bits = unlist(strsplit(lineToPrint," "))
    vertex_id = bits[1]

    pos = match(vertex_id, map.df$vertex_id)

    if(!is.na(pos)) {

      rgb = paste(col2rgb(map.df$color[pos]), collapse=" ")

      tmp0 = paste(bits[1:3], collapse = " ")
      tmp0 = gsub(x=tmp0, pat=",", rep="")
      lineToPrint = paste0(tmp0, " w=8 h=8 s=r fg=", rgb," bg=", rgb,",")

    }

  }

  if(file.exists(outfile)) {
  system(paste("rm", outfile))
  }

  cat(lineToPrint,"\n", file=outfile, append=TRUE,sep = "")

}

}
