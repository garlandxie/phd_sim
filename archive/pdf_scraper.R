# load libraries ----
library(tabulapdf) # for extracting tabular data from PDFs
library(here)      # for creating relative file-paths
library(dplyr)     # for manipulating data 

# import ------

# need to get neighbourhood equity index scores 
# for all 140 neighbourhoods in Toronto
# there's many tables in this PDF, but the one I need is located in pages 15-18
url <- "https://www.toronto.ca/wp-content/uploads/2017/11/97eb-TSNS-2020-NEI-equity-index-methodology-research-report-backgroundfile-67350.pdf"

# a link to the pdf file within local repo (in case url is broken )
# just replace the url object with the pdf object in line 20
pdf <- here(
  "data", "input_data", 
  "neighbourhood_equity","TSNS_equity_index.pdf"
  )

# extract ----

# this is a bit slow to run, but faster than manually entering data in Excel
pdf_tables <- extract_tables(url) 

eq_index1 <- pdf_tables[[5]]
eq_index2 <- pdf_tables[[6]]
eq_index3 <- pdf_tables[[7]]
eq_index4 <- pdf_tables[[8]]

# verify if the column names of the equity index df are correct
# because the indexing on lines 25-28 might give you the wrong table
# i.e., Rank, Neighbourhood Number Name, and Score as column names
# if not, double-check the code and pdf!

# clean ----

eq_index_tidy <- eq_index1 %>%
  rbind(eq_index2, eq_index3, eq_index4) %>%
  janitor::clean_names() %>%
  as.data.frame() 

# save to disk ----

write.csv(eq_index_tidy, here("data", "intermediate_data", "eq_index.csv"))

