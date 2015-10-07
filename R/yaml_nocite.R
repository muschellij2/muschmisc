#' @title Add nocite to Rmarkdown Document
#'
#' @description Will add all the bibliography entries to the YAML of an Rmarkdown document
#' @param file input filename (Rmd)
#' @param outfile output filename (Rmd). Will overwrite \code{file} by default
#' @export
#' @importFrom yaml yaml.load
#' @importFrom RefManageR ReadBib
#' @return Character of name of outfile
yaml_nocite <- function(file, # input filename (Rmd)
                        outfile = file # output filename (Rmd).  Will overwrite \code{file} by default
){ 
  old_dir = getwd()
  on.exit({
    setwd(old_dir)
  })
  dn = dirname(file)
  setwd(dn)
  text = readLines(file)
  nocite_ind = grep("^(\\s*|)nocite:", text)
  bib_ind = grep("^(\\s*|)bibliography:", text)    
  if ( length(nocite_ind) == 0 | length(bib_ind) == 0 ) {
    stop("nocite or bibliography not specified in YAML")
  }
  # Take the first one (in the YAML header)
  bib = text[bib_ind[1]]
  nocite = text[nocite_ind[1]]
  
  # paste together for yaml.load
  keep = paste(c(bib, nocite), collapse = "\n")
  yam = yaml.load(keep)
  # Read in the entries, but don' tcheck them
  bib = ReadBib(yam$bibliography, check = FALSE)
  
  # Get all the handles from biblio
  handles = sapply(bib, names)
  handles = paste("@", handles, sep = "", 
                  collapse = ",")
  # Add in the handles to nocite  
  nocite_filled = gsub("^(\\s*|)nocite:.*", 
                       paste0("\\1nocite: |\n  ", handles),
                       nocite)
  # Put pack into text and write out 
  text[nocite_ind] = nocite_filled
  writeLines(text, con = outfile)
  return(outfile)
}

