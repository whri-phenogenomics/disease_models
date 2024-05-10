load_phenodigm <- function() {
  
  phenodigm_df <- read.delim("data/phenodigm_matches_dr20.1.txt", 
                             stringsAsFactors = F, 
                             header = T, 
                             sep = "\t") 
  ;
  
  return(phenodigm_df)
  
}


load_phenodigm_other <- function() {
  
  phenodigm_other_df <- read.delim("data/phenodigm_other_dr20.1.txt.gz", 
                             stringsAsFactors = F, 
                             header = T, 
                             sep = "\t") 
  ;
  
  return(phenodigm_other_df)
  
}



load_gene_summary <- function() {

    gene_summary_df <- read.table("data/gene_summary_dr20.1.txt", 
                          stringsAsFactors = F, 
                          header = T, 
                          sep = "\t",
                          quote = "");

    return(gene_summary_df)

}


create_phenodigm_table <- function() {
  
  phenodigm_table  <- load_phenodigm() %>%
    select(disorder_id, 
           disorder_name,
           gene_symbol,
           description,
           score, 
           query_phenotype, 
           match_phenotype
    ) %>%
    rename('Disorder id' = disorder_id,
           'Disorder name' = disorder_name,
           'Human gene symbol' = gene_symbol,
           'Mouse model description' = description,
           'PhenoDigm % score' = score,
           'Matching human phenotypes' = query_phenotype,
           'Matching mouse phenotypes' = match_phenotype)
  
  return(phenodigm_table)
  
}

create_phenodigm_other_table <- function() {
  
  phenodigm_other_table  <- load_phenodigm_other() %>%
    select(description,
           mgi_id,
           gene_symbol,
           query,
           disorder_name,
           score, 
           query_phenotype, 
           match_phenotype
    ) %>%
    rename('Mouse model description' = description,
           'Mouse gene id' = mgi_id,
           'Human ortholog gene symbol' = gene_symbol,
           'Disorder id' = query,
           'Disorder name' = disorder_name,
           'PhenoDigm % score' = score,
           'Matching human phenotypes' = query_phenotype,
           'Matching mouse phenotypes' = match_phenotype)
  
  return(phenodigm_other_table)
  
}


create_gene_summary_table <- function() {
  
  gene_summary_table  <- load_gene_summary() %>%
    filter(disorder_id !="-") %>%
    rename('Human gene symbol' = gene_symbol,
           'Human gene HGNC id' = hgnc_id,
           'Mouse one2one ortholog gene id' = mgi_id,
           'Associated disorders id' = disorder_id,
           'Associated disorders name' = disorder_name,
           'Gene in IMPC_pipeline' = IMPC_pipeline,
           'Mouse Phenotypes avilable' = IMPC_phenotypes,
           'Any PhenoDigm match' = PhenoDigm_match,
           'Maximum PhenoDigm score' = max_score) 
  
  return(gene_summary_table)
  
}






