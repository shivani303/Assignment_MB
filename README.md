# Assignment_MB

# TAKING INPUT FROM A USER
file1= readline(prompt = "Enter a path to gene_info.gz file : ");

file2= readline(prompt = "Enter a path to .gmt file : ");

# DATA FETCHING FROM gene_info.gz FILE 
zz=gzfile(file1 ,open = "r")

gene_info= read.csv(zz, check.names = FALSE, sep = "\t")

#View(gene_info)

gene_info_selected= gene_info[,c(2,3,5)]

#View(gene_info_selected)

**-> creating an empty dataframe** 
ID= c(0)
Sym= c(0)
df= data.frame(ID,Sym)

**-> adding synonymous column data into symbol data**

for(i in 1:nrow(gene_info_selected)) {

  row <- gene_info_selected[i, ]
  
  new_row = c(ID = row$GeneID, Sym= row$Symbol)
  
  df = rbind(df,new_row)
  
  gene_symbol= strsplit(row$Synonyms, "\\|")
  
  #print(gene_symbol)
  
  temp= row$GeneID
  
  for(item in gene_symbol){
    for(item1 in item){
      if(item1 != "-"){
    row1= c(ID = temp, Sym= item1)
    #print(row1)
    df= rbind(df,row1)}
    }
  }
}

df= df[-1,]
View(df)

# FILE 2 DATA FETCHING AND MATCHING 
library(tidyr)

gmt_file <- read.delim(file2, header = F)

gmt_file <- gmt_file %>% unite(genes,3:ncol(gmt_file), sep=",")

colnames(gmt_file) <- c("pathway", "source", "genes")

**create empty dataframe**
pathway_name= c(0)

pathway_description= c(0)

gene_id_sets= c(0)

gmt_df= data.frame(pathway_name,pathway_description,gene_id_sets)

**create empty vector**
gene_vec= c()

**-> loop for mapping Entrez id from dataframe-1 (df) and replace it with Gene name in .gmt files**

for(each in 1:nrow(gmt_file)){
  line= gmt_file[each,]
  
  for(each1 in line$genes){
  
    items <- strsplit(each1, ",")[[1]]
    for (i in seq_along(items)) {
    
      item <- items[i]
      for(each2 in 1:nrow(df)){
      
        df_row= df[each2,]
        if(item == df_row$Sym){
        
          gene_vec=append(gene_vec,df_row$ID)
        }
      }
    }
    
  }
  
  new_line= c(pathway_name = line$pathway,pathway_description= line$source,
              tibble(gene_id_sets=list(gene_vec)))
              
  gmt_df= rbind(gmt_df,new_line)
  
  gene_vec= NULL
}

gmt_df= gmt_df[-1,]

View(gmt_df)
