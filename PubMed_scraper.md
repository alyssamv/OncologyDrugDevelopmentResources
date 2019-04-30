PubMed Scraper
================

This file gives the user a way of getting cleaned information about a publication. This is superior to simply downlaoding the CSV file from PubMed directly, because it gives parsed results. This leaves very little left to the researcher, and can leave more time to extract important information from the text itself (such as methods, etc). If you find this useful and share, please credit me!

The code infrastructure is below for reading in a .txt file of PMID (PubMed identifiers). This file may be downloaded directly from the [PubMed website](https://www.ncbi.nlm.nih.gov/pubmed).

For an example, I will input a list (vector) of 10 PMIDs to show the output of the code.

``` r
## Get PMIDs
# file location of PMID list
# path = '' # location of file  # <- uncomment these lines if you are reading in a .txt file
# ext = '' # name of .txt file
# file = paste(path, ext, sep = '')
# pmid = readr::read_lines(file) 

pmid = c("29074098", "27932068", "27044938", "26858122", "28588020", "28813164", "28089635", "28039367", "28765328", "29037983")

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
```

Next, we access the PubMed API and retrieve the HTML. Depending on the number of PMIDs you are fetching, this code may take some time to run. I have not run into any issues for large samples other than the time.

``` r
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
```

Finally, we can take the HTML and convert into a data frame of information that we care about.

``` r
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

head(pub_info)
```

    ## Source: local data frame [6 x 8]
    ## Groups: <by row>
    ## 
    ## # A tibble: 6 x 8
    ##   PMID   PMCID  DOI   Title   Journal Authors   Abstract   `Publication Da…
    ##   <chr>  <chr>  <chr> <chr>   <chr>   <chr>     <chr>      <chr>           
    ## 1 29074… PMC57… DOI:… Lorlat… Lancet… Shaw AT,… BACKGROUN… 2017 Dec        
    ## 2 27932… PMC54… DOI:… Rovalp… Lancet… Rudin CM… BACKGROUN… 2017 Jan        
    ## 3 27044… PMC53… DOI:… Phase … J Clin… Hong D, … PURPOSE: … 2016 May 20     
    ## 4 26858… PMC55… DOI:… Safety… Lancet… Antonia … BACKGROUN… 2016 Mar        
    ## 5 28588… PMC55… DOI:… Enasid… Blood   Stein EM… Recurrent… 2017 Aug 10     
    ## 6 28813… ""     DOI:… Pembro… J Clin… Ott PA, … Purpose T… 2017 Dec 1
