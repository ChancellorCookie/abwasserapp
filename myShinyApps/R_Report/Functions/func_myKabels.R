myKabel <- function(df,Caption = "",Rownames = F){
  
  maxCols<- 6
  maxRows<-45
  
  if (ncol(df)<maxCols & nrow(df)<maxRows) {
    kable(df, 
          "latex", 
          booktabs = T, 
          row.names = Rownames,
          caption = Caption) %>% kable_styling(latex_options =c("striped",
                                                                "scale_down"))
  }else if (ncol(df)<maxCols & nrow(df)> maxRows) {
    kable(df, 
          "latex", 
          booktabs = T, 
          longtable = T,
          row.names = Rownames,
          caption = Caption) %>% kable_styling(latex_options =c("striped",
                                                                "repeat_header"))
  }else if (ncol(df)>maxCols & nrow(df)<maxRows) {
    
    NumberOfSplitDfs <- ceiling(ncol(df %>% select(-Labels))/(maxCols-1))
    
    for (i in 0:NumberOfSplitDfs-1) {
      
      n1 <- i*(maxCols-1) + 1
      n2 <- (i+1)*(maxCols-1)
      
      linseq <- seq(n1,n2)+1
      linseqDF <- c(1,linseq)
      
      if(n2>ncol(df)){n2 <- ncol(df)} 
      df2Names <- names(df[linseqDF])
      df2<-data.frame(df[,1],df[,linseq],stringsAsFactors = F) %>% `names<-`(.,df2Names)  
      kable(df2,
            "latex", 
            booktabs = T, 
            row.names = Rownames,
            caption = Caption) %>% kable_styling(latex_options =c("striped",
                                                                  "scale_down"))
    }
  }else if (ncol(df)>maxCols & nrow(df)>maxRows) {
    
    NumberOfSplitDfs <- ceiling(ncol(df %>% select(-Labels))/(maxCols-1))
    
    for (i in 0:NumberOfSplitDfs-1) {
      
      n1 <- i*(maxCols-1) + 1
      n2 <- (i+1)*(maxCols-1)
      
      linseq <- seq(n1,n2)+1
      linseqDF <- c(1,linseq)
      
      if(n2>ncol(df)){n2 <- ncol(df)} 
      df2Names <- names(df[linseqDF])
      df2<-data.frame(df[,1],df[,linseq],stringsAsFactors = F) %>% `names<-`(.,df2Names)  
      kable(df2,
            "latex", 
            booktabs = T, 
            row.names = Rownames,
            caption = Caption) %>% kable_styling(latex_options =c("striped",
                                                                  "scale_down"))
    }
  }
}