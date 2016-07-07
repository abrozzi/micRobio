#' @title Color SplitsTree dendrograms
#' @description Reads a nexus file and adds colors to the leaves
#' @param nexus.file
#' @param tips (leaves of the dendrogram)
#' @param colors
#' @param vlabels
#' @param w width of the tip
#' @param h height of the tip
#' @param s shape of the tip "o" is oval
#' @return a new nexus file with color
#' @examples
#' inputfile <- file.path(path.package('microbio'),'input.nexus')
#' outfile <- file.path(path.package('microbio'),'output.nexus')
#' tips <- c("A","R","G","B")
#' col <- c("red","blue","green","yellow")
#' add.nex.features(nexus.file=inputfile, tips=tips, colors=col, outfile=outfile)
#' @export
#'
add.nex.features <- function(nexus.file, tips, colors, outfile,fromBIGSdb=FALSE, fg="1 1 1", vlabels="default",w=20,h=20,s="o") {

  if(file.exists(outfile)) {
    system(paste("rm", outfile))
  }

  file = readLines(nexus.file)
  startTRANS = match("TRANSLATE", file)
  ends = which(file == ";")
  endTRANS = ends[ends>startTRANS][1]

  map.df = get.nexus.tips(nexus.file=nexus.file, fromBIGSdb = fromBIGSdb)

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

map.df$color = colors[match(map.df$tips, tips)]
map.df$color = as.character(map.df$color)

if(length(vlabels)>1){

  map.df$vlabels = vlabels[match(map.df$tips, tips)]
  map.df$vlabels = as.character(map.df$vlabels)

}

start.VERTICES = match("VERTICES", file)
end.VERTICES = ends[ends>start.VERTICES][1]

start.VLABELS =  match("VLABELS", file)
end.VLABELS =  ends[ends>start.VLABELS][1]

total = length(file)

progBar <- txtProgressBar(min = 0, max = total, style = 3)

for (i in 1:total){

  setTxtProgressBar(progBar, i)

  lineToPrint = file[i]

  if(i > (start.VERTICES+1) & i < (end.VERTICES-1) ) {

    bits = unlist(strsplit(lineToPrint," "))
    vertex_id = bits[1]

    pos = match(vertex_id, map.df$vertex_id)

    if(!is.na(pos)) {

      rgb = paste(col2rgb(map.df$color[pos]), collapse=" ")

      tmp0 = paste(bits[1:3], collapse = " ")
      tmp0 = gsub(x=tmp0, pat=",", rep="")
      lineToPrint = paste0(tmp0, " w=",w, " h=",h, " s=",s, " fg=", fg," bg=", rgb,",")

    } else {
      tmp0 = paste(bits[1:3], collapse = " ")
      tmp0 = gsub(x=tmp0, pat=",", rep="")
      lineToPrint = paste0(tmp0, " fg=1 1 1 bg=1 1 1,")
    }

  } #closes VERTEX BLOCK

  ###################
  if(length(vlabels)==1){

    if(vlabels=="none"){

    if(i > (start.VLABELS) & i < (end.VLABELS) ) {

      b = unlist(strsplit(lineToPrint,""))
      idx = grep("'",b)
      M = matrix(c(1,idx,length(b)), ncol=2, byrow = T)
      pieces = apply(M, 1, function(x) substr(lineToPrint, start = x[1], stop=x[2]))
      lineToPrint = paste(pieces, collapse="")
    }
  }
}
  ###################

  if(length(vlabels)>1){

    if(i > (start.VLABELS) & i < (end.VLABELS) ) {

      bits = unlist(strsplit(lineToPrint," "))
      vertex_id = bits[1]

      pos = match(vertex_id, map.df$vertex_id)

      vlab = map.df$vlabels[pos]

      pieces = c(bits[1],"'",vlab,"'", bits[3:length(bits)])

      lineToPrint = paste(pieces, collapse=" ")
    }
  } # closes IF VLABELS


  cat(lineToPrint,"\n", file=outfile, append=TRUE,sep = "")

} # closes for()

}
