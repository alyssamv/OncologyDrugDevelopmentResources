library(tidyverse)

# file location of PMID list
path = '' # location of file
ext = '' # name of .txt file
file = paste(path, ext, sep = '')

### Read in list of PMIDs
pmid = readr::read_lines(file)

if (length(pmid) > 200) # split list into subsets so that we don't get kicked off the server
  d = split(pmid, ceiling(seq_along(pmid)/200))
if (length(pmid) <= 200)
  d = pmid

### define Pubmed url tag
url <- 'https://www.ncbi.nlm.nih.gov/pubmed/'


### append PubMed ID to url tag
links = list()
for (i in 1:length(d))
  links[[i]] = setNames(paste(url, d[[i]], sep = ''), d[[i]])


### Retrieve HTML from PubMed
htMl <- unlist(sapply(links, function(l) lapply(seq_along(l),function(i){
  RCurl::getURL(l[[i]], ssl.verifypeer = FALSE)})))


### Parse HTML
df = lapply(seq_along(htMl),function(i){
  zz = textConnection(htMl[[i]])
  datalines = readLines(zz)
  close(zz)
  title = gsub('<.*?>', '', Filter(Negate(is.na), unique(stringi::stri_extract_all_regex(datalines, ('<h1>([^*]+)</h1>')))))
  authors = gsub('[0-9]', '', gsub('<.*?>','', Filter(Negate(is.na), unique(stringi::stri_extract_all_regex(datalines, ('<a href="/pubmed/\\?term=.*?>([^*]+)</a>[^.]*?\\.</div>'))))))
  strt = Filter(Negate(is.na),stringi::stri_extract_first_regex(datalines, ('(?<=class="cit">).*?(?=.</div>)')))
  j.meta = gsub('</a>','', gsub('.\">','', Filter(Negate(is.na), stringi::stri_extract_first_regex(strt, ('(?<=.\">).*?(?=.</a>).*?(?=$)')))))
  abstract = gsub('Abstract','', gsub('<.*?>', '', Filter(Negate(is.na), stringi::stri_extract_first_regex(datalines, ('<h3>Abstract</h3>([^*]+)</p></div>')))))
  ids = gsub(' \\[.*?\\]', '',(gsub('<.*?>',"", gsub('</dt> <dd>',"", gsub('</dd> <dt>'," ", gsub('<a href.*?">',"", Filter(Negate(is.na),stringi::stri_extract_first_regex(datalines, ('<dt>PMID([^*]+)</a></dd> </dl></div>')))))))))
  c(if (identical(as.character(title), character(0))) NA_character_ else as.character(title),
    if (identical(authors, character(0))) NA_character_ else authors,
    if (identical(as.character(j.meta), character(0))) NA_character_ else as.character(j.meta),
    if (identical(abstract, character(0))) NA_character_ else abstract,
    if (identical(ids, character(0))) NA_character_ else ids)
})


### Convert to data.frame
pub_info <- data.frame(do.call(rbind, df), stringsAsFactors = FALSE) %>% # convert to data frame
  `colnames<-`(c("Title", "Authors", "Publication Details", "Abstract", "IDs")) %>% # specify column names
  separate(data = .,
           col = "Publication Details", 
           into = c("Journal", "Publication_Date"), # split Publication_Details into separate information
           sep = "[.] ",
           extra = "merge",
           fill = "right") %>% # if there is missing information, return NA
  separate(data = .,
           col = "IDs",
           into = c("PMID", "PMCID", "DOI"), # split IDs into separate IDs
           sep = " ",
           extra = "drop",
           fill = "right") %>%
  mutate(PMID = str_remove(PMID, "PMID:"),
         PMCID = str_remove(PMCID, "PMCID:")) %>%
  rowwise %>%
  mutate(`Publication Date` = head(str_split(Publication_Date, pattern = ";")[[1]], n = 1)) %>%
  dplyr::select("PMID", "PMCID", "DOI", "Title", "Journal", "Authors", "Abstract", "Publication Date") # reorder columns

