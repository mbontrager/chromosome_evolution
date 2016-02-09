# -----------------------------------------------------------------------------
# Scrape data from chromorep about reptile chromosome number
# Author: Martin Bontrager
# email: mbontrager@gmail.com
# -----------------------------------------------------------------------------
#
# Works as of 8 February, 2016. If chromorep is updated with additional nodes
# or pages this will need to be modified. The data is oddly formatted so this 
# is prone to breaking. Be careful.
#
# User-defined Parameters------------------------------------------------------
require(rvest)
require(stringr)
require(gsubfn)

# There are 72 nodes in chromorep, only some of which contain information.
nodes <- c(2:24, 26, 27, 29:72)
raw_data <- list()

# Wrapper function to scrape Chromorep
scrapeRep <- function(nodes){
    labels <- c("Family", "Genus", "Species", "DipChrNum", "MacroChr", "Biarm", 
                "Uniarm", "MicroChr", "fn", "Refs", "Fulltext")
    reptileChr <<- matrix(labels, ncol=11)
    
    for (i in nodes){
        site <- paste("http://chromorep.univpm.it/node/", i, sep="")
        html <- read_html(site)
        taxa <- html_nodes(html, "p")
    
    if (checkForTaxa(taxa)){
        splitEntry(taxa)
    }
    print(paste("Node", i, "complete", sep=" "))
    }
    return(reptileChr)
}

# Split the chromorep entry into Chr data
splitEntry <- function(taxList){
    fam <- 
        (taxList %>% 
        html_nodes("strong"))[1] %>% 
        html_text()
    
    for (i in 1:length(taxList)){
        write(as.character(taxList[i]), "data/full_list.html", append = TRUE)
        
#         if (checkForTaxa(taxList[i])){
#             species <- taxList[i] %>% 
#                 html_nodes("em") %>%
#                 html_text()
#             genus <- unlist(strsplit(species, " "))[1]
#             if (length(unlist(strsplit(species, " "))) > 1){
#             species <- paste(unlist(strsplit(species, " "))[-1], 
#                              collapse = " ")
#             } else {
#                 species <- NA
#             }
#             chrNum <- taxList[i] %>%
#                 html_nodes("strong") %>%
#                 html_text()
#             chrData <- parseChrNum(chrNum)
#             refs <- splitRefs(as.character(taxList[i]))
#             text <- html_text(taxList[i])
#             entry <- c(fam, genus, species, chrData, 
#                        paste(refs, collapse=" && "), text)
#             reptileChr <<- rbind(reptileChr, entry)
#         }
    }
}

# Is the html line an actual taxa?
checkForTaxa <- function(taxa){
    exist <- FALSE
    for (i in taxa){
        if (str_detect(as.character(i), "<em>") && 
            str_detect(as.character(i), "strong")){
            exist <- TRUE
        }
    }
    return(exist)
}

# Split references in the entry
splitRefs <- function(htmlstring){
    htmlstring <- str_replace(htmlstring, 
                       "<span class=.*</span></span></span></span>", 
                       "")
    htmlstring <- str_replace(htmlstring, 
                       "<span class=.*</span></strong></span><strong> </strong></span>", 
                       "")
    inst <- unlist(strsplit(htmlstring, "<strong>"))
    reflist <- c()
    for (i in inst[-1]){
        ref <- trimws(strapplyc(i, ".*</strong>(.*?)", simplify=c ))
        ref <- str_replace(ref, "&amp;", "&")
        ref <- str_replace(ref, "</p>", "")
        reflist <- c(reflist, ref)
    }
    return(reflist)
}

# Parse the data about chromosome number and morphology. For entries with 
# multiple reported values, report the most common values per species
parseChrNum <- function(obs){
    val <- c()
    fin <- c(rep(NA, 6))
    for (i in obs){
        val <- c(val, splitChrString(i))
    }
    val <- matrix(val, nrow=length(obs), byrow=TRUE)
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
    if (length(chrStr) < 4){
        dipNum <- str_extract(chrStr, "^[0-9]+")
    }
    dipNum <- str_extract(chrStr, "^[0-9]+")
    macro <- str_match(chrStr, ": ([0-9]+)*")[2]
    biarm <- str_match(chrStr, "[0-9]+\\(([0-9]+)")[2]
    uniarm <- str_match(chrStr, ",([0-9]+)")[2]
    micro <- str_match(chrStr, "\\) ([0-9]+)")[2]
    fn <- str_match(chrStr, " an ([0-9]+)")[2]
    
    return(c(dipNum, macro, biarm, uniarm, micro, fn))
}

# Thanks to this stack overflow answer for a function to determine mode:
# http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

scrapeRep(nodes)
reptiles <- as.data.frame(reptileChr, stringsAsFactors = FALSE)
rownames(reptiles) <- NULL
colnames(reptiles) = reptiles[1, ] # the first row will be the header
reptiles = reptiles[-1, ]
#write.csv(reptiles, "Projects/chromosome_evolution/data/reptiles.csv")
