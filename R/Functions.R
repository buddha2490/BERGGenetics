## Functions for querying the Genetics files


# Functions ---------------------------------------------------------------
findID <- function(idv){
  df <- BERGSamples$Summary
  indf <- df[df$ID %in% idv,]
  studies <- names(indf)
  studies <- studies[!studies %in% c("ID","GWAS.DATA")]
  foo <- lapply(idv,function(x) {
    bar <- indf[indf$ID==x,]
    instudies <- ""
    for (i in 1:length(studies)){
      if (bar[[studies[i]]]==1) instudies <- paste0(instudies,studies[i]," ")
    }
    result <- data.frame(ID=x,
                         Studies=instudies,
                         GWAS_DATA=df$GWAS.DATA[df$ID==x],
                         stringsAsFactors=F)
    return(result)
  })
  return(Reduce(function(x,y) rbind(x,y), foo))
}


# Do we have this SNP?

findSNP <- function(findthesesnps){
  studies <- names(BERGSNPs)
  studies <- studies[studies != "Summary"]
  snps <- data.frame(SNP=findthesesnps,stringsAsFactors=F)

  allBERGSNPs <- lapply(studies,function(x){
    df <- data.frame(SNP=as.character(BERGSNPs[[x]])[BERGSNPs[[x]] %in% findthesesnps],
                     stringsAsFactors=F)

    if (nrow(df) != 0) {
      df[[x]] <- x
      return(df)
    } else {
      df <- data.frame(SNP="None",stringsAsFactors=F)
      df[[x]] <- x
      return(df)
    }}) %>%
    Reduce(function(x,y) dplyr::full_join(x,y,"SNP"), .) %>%
    dplyr::filter(.,SNP != "None") %>%
    dplyr::left_join(snps,.,"SNP")


  # Here's an ugly loop to drop a lot of missing values
  foo <-  allBERGSNPs
  foo$Summary <- as.character(rep("",nrow(foo)))

  for (a in 1:nrow(foo)) {
    v <- vector(mode="character",length=0)
    for (b in 2:ncol(foo)){
      if(!is.na(foo[a,b])) v <- paste(v,foo[a,b])
    }
    foo$Summary[a] <- v
  }
  foo <- foo[,c("SNP","Summary")]
  return(foo)
}



