#' @title Color SplitsTree dendrograms
#' @description Reads a nexus file and adds colors to the leaves
#' @param nexus.file
#' @param tips (leaves of the dendrogram)
#' @param colors
#' @param vlabels
#' @param f fonts of the labels f='Courier-PLAIN-24'. Other option are: Calibri, Arial, Times. For bold -BOLD and not -PLAIN
#' @param lc
#' @param lk
#' @param w width of the tip
#' @param h height of the tip
#' @param s shape of the tip "o" is oval. The other oprion is "r" rectangle.
#' @return a new nexus file with colors
#' @examples
#' inputfile <- file.path(path.package('microbio'),'input.nexus')
#' outfile <- file.path(path.package('microbio'),'output.nexus')
#' tips <- c("A","R","G","B")
#' col <- c("red","blue","green","yellow")
#' ws <- c(10,20,30,40)
#' hs <- c(10,20,30,40)
#' add.nex.features(nexus.file=inputfile, tips=tips, colors=col, w=ws, h=hs, outfile=outfile)
#' @export
#'
add.nex.features <- function(nexus.file,
                             tips,
                             colors,
                             outfile,
                             fromBIGSdb=FALSE,
                             fg="1 1 1",
                             vlabels,
                             w=20,
                             h=20,
                             s="o",
                             f="Dialog-BOLD-16",
                             lc,
                             lk
                             ) {

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

# set colors
if( !is.character(colors) ) {
  stop("'colors' must be a character vector of the same length of tips")
  }

if( length(tips)!=length(colors) ) {
  stop("'colors' must be a character vector of the same length of tips")
  }

map.df$color = colors[match(map.df$tips, tips)]
map.df$color = as.character(map.df$color)

# set valabels
map.df$vlabels =NA

if(length(vlabels) != length(tips)){stop("vlabels must be a character vector of the same length of tips.")}

map.df$vlabels = vlabels[match(map.df$tips, tips)]
map.df$vlabels = as.character(map.df$vlabels)

map.df$lc =NA
map.df$lc = lc[match(map.df$tips, tips)]
map.df$lc = as.character(map.df$lc)

map.df$lk =NA
map.df$lk = lk[match(map.df$tips, tips)]
map.df$lk = as.character(map.df$lk)

map.df$f =NA
map.df$f = f[match(map.df$tips, tips)]
map.df$f = as.character(map.df$f)

# set w and h fpr each node
map.df$w = NA
map.df$h = NA

if (length(w)==1) {
map.df$w = w
} else {

  map.df$w = w[match(map.df$tips, tips)]
  map.df$w = as.character(map.df$w)

}

if (length(h)==1) {
  map.df$h = h
} else {
  map.df$h = h[match(map.df$tips, tips)]
  map.df$h = as.character(map.df$h)
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

  if(i > (start.VERTICES) & i < (end.VERTICES) ) {

    bits = unlist(strsplit(lineToPrint," "))
    vertex_id = bits[1]

    pos = match(vertex_id, map.df$vertex_id)

    if(!is.na(pos)) {

      rgb = paste(col2rgb(map.df$color[pos]), collapse=" ")

      tmp0 = paste(bits[1:3], collapse = " ")
      tmp0 = gsub(x=tmp0, pat=",", rep="")
      lineToPrint = paste0(tmp0, " w=",map.df$w[pos], " h=",map.df$h[pos], " s=",s, " fg=", fg," bg=", rgb,",")

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

    if(i > (start.VLABELS) & i < (end.VLABELS) ) {

       bits = unlist(strsplit(lineToPrint," "))
       vertex_id = bits[1]

       pos = match(vertex_id, map.df$vertex_id)

       vlab = map.df$vlabels[pos]
       lc = paste(col2rgb(map.df$lc[pos]), collapse=" ")
       lk = paste(col2rgb(map.df$lk[pos]), collapse=" ")
       f = map.df$f[pos]

       if(nchar(vlab)!=0) {

         pieces = c(bits[1],paste0("'",vlab,"'"), bits[3:4], paste0("f='",f,"'"), paste0("lc=", lc), paste0("lk=", lk, ","))

       } else {


         pieces = c(bits[1],paste0("'",vlab,"'"), bits[3:4], paste0("f='",f,"'"), paste0("lc=", lc,","))

       }

       lineToPrint = paste(pieces, collapse=" ")

      #atomic = unlist(strsplit(lineToPrint,""))
      #limits = which(atomic %in% "'")
      #start = limits[1]
      #end = limits[2]

      #lineToPrint = paste0(
      #    paste(atomic[1:start], collapse=""),
      #  vlab,
      #  paste(atomic[end:length(atomic)], collapse="")
      #  )

    }

  cat(lineToPrint,"\n", file=outfile, append=TRUE,sep = "")

} # closes for()

}
