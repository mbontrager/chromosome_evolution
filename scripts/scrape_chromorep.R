# -----------------------------------------------------------------------------
# Scrape data from chromorep about reptile chromosome number
# Author: Martin Bontrager
# email: mbontrager@gmail.com
# -----------------------------------------------------------------------------
#
# Works as of 7 February, 2016. If chromorep is updated with additional nodes
# or pages this will need to be modified. The data is oddly formatted so this 
# is prone to breaking. Be careful.
#
# User-defined Parameters------------------------------------------------------
require(rvest)
require(stringr)
require(gsubfn)

# There are 72 nodes in chromorep, only some of which contain information.
nodes <- c(1:72)
reptileChr <- NULL
labels <- c("Family", "Genus", "Species", "DipChrNum", "MacroChr", "Biarm", 
            "Uniarm", "MicroChr", "Refs")

scrapeRep <- function(nodes){
    for (i in nodes){
    html <- read_html(paste("http://chromorep.univpm.it/node/", i, sep=""))
    taxa <- html_nodes(html, "p")
    splitEntry(taxa)
    }
}

splitEntry <- function(taxList){
    fam <- 
        taxList %>% 
        html_nodes("strong span") %>% 
        html_text()
    
    for (i in taxList[-1]){
        species <- i %>% 
            html_nodes("em") %>%
            html_text()
        genus <- unlist(strsplit(species, " "))[1]
        species <- paste(unlist(strsplit(species, " "))[-1], collapse = " ")
        chrNum <- i %>%
            html_nodes("strong") %>%
            html_text()
        chrData <- parseChrNum(chrNum)
        refs <- splitRefs(as.character(i))
        entry <- c(fam, genus, species, chrData, paste(refs, collapse=" | "))
        
    }
}

splitRefs <- function(htmlstring){
    inst <- unlist(strsplit(htmlstring, "<strong>"))
    reflist <- c()
    for (i in inst[-1]){
        ref <- trimws(strapplyc(i, ".*</strong>(.*?)", simplify=c ))
        ref <- gsub("&amp;", "&", ref)
        ref <- gsub("</p>", "", ref)
        print(ref)
        reflist <- c(reflist, ref)
    }
    return(reflist)
}

# Parse the data about chromosome number and morphology. For entries with 
# multiple reported values, report the most common values per species
parseChrNum <- function(obs){
    val <- c()
    fin <- c(rep(NA, 5))
    for (i in obs){
        val <- c(val, splitChrString(i))
    }
    val <- matrix(val, nrow=length(obs), byrow=TRUE)
    print(val)
    for (i in (1:ncol(val))){
        a <- val[,i][!is.na(val[,i])]
        if (length(a) >= 0){
            fin[i] <- Mode(a)
        }
    }
    return(fin)
}

# Split the chromosome num/info string
splitChrString <- function(chrStr){
    dipNum <- as.integer(str_extract(chrStr, "^[0-9]+"))
    macro <- as.integer(str_match(chrStr, ": ([0-9]+)*")[2])
    biarm <- as.integer(str_match(chrStr, "[0-9]+\\(([0-9]+)")[2])
    uniarm <- as.integer(str_match(chrStr, ",([0-9]+)")[2])
    micro <- as.integer(str_match(chrStr, "\\) ([0-9]+)")[2])
    
    return(c(dipNum, macro, biarm, uniarm, micro))
}

# Thanks to this stack overflow answer for a function to determine mode:
# http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}