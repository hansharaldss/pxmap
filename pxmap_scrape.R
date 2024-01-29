
library(RSelenium)
library(jsonlite)
library(pxweb)

# Directory needs to have a folder named uttak.
# Data will be saved in uttak

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dname<-dirname(rstudioapi::getActiveDocumentContext()$path)

#Open web driver ----

#'This should be improved by someone with a better understanding of docker.
#'Docker must be running before script is executed.
#'There is nn pariticular reason for using this firefox version (but evereything runs!).

shell("docker pull selenium/standalone-firefox:2.53.0")
shell('docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0')
Sys.sleep(10)
remDr <- remoteDriver(port = 4445L, browserName = "firefox")
remDr$open()

#'Timing start of scraping session
timi0<-Sys.time()

#Get table of level 1 categories ----

remDr$navigate("https://px.hagstofa.is/pxis/pxweb/is/")

takki_1<-remDr$findElements(using = "xpath", "//a[contains(@href, 'pxweb')]")

tafla_1_nofn<-vector()
tafla_1_slodir<-vector()

for(i in 1:length(takki_1)){
  tafla_1_nofn[i]<-unlist(takki_1[[i]]$getElementText())
  tafla_1_slodir[i]<-unlist(takki_1[[i]]$getElementAttribute("href"))
}

tafla1<-data.frame("nofn"=tafla_1_nofn, "slodir"=tafla_1_slodir)
tafla1<-tafla1[tafla1$nofn!="",]



# Scrape table names and urls ----

teljari<-1
timi1<-Sys.time()
tafla2<-data.frame(nofn2="", slod2="")

# Loop iterates though level 1 categories
for(k in 1:nrow(tafla1)){
  curr_path_1<-tafla1$slodir[k]
  curr_nafn_1<-tafla1$nofn[k]
  remDr$navigate(curr_path_1)
  
  #Finding all expandable tree elements
  takki_2<-remDr$findElements(using = "class", "AspNet-TreeView-Expand")
  
  #While there are expandable elements, click first element and search again
  while(length(takki_2)>0){
    takki_2[[1]]$clickElement()
    takki_2<-remDr$findElements(using = "class", "AspNet-TreeView-Expand")
  }
  
  #Once all elements are expanded, find all elements with a hypertext reference attribute
  #This line woukd have to be changed to scrape the english version
  takki_3<-remDr$findElements(using = "xpath", "//a[contains(@href, 'pxis')]")
  
  nofn2<-vector()
  slodir2<-vector()
  #Loop iterates though all elements extracting the text and href elements
  for(i in 1:length(takki_3)){
    nofn2[i]<-unlist(takki_3[[i]]$getElementText())
    slodir2[i]<-unlist(takki_3[[i]]$getElementAttribute("href"))
    print(paste0("Skröpun 1: tafla númer: ", teljari))
    print(nofn2[i])
    teljari<-teljari+1
  }
  
  tafla2_temp<-data.frame("nofn2"=nofn2, "slod2"=slodir2)
  #Remove links that are not tables
  tafla2_temp$nchar<-nchar(tafla2_temp$slod2)
  tafla2_temp<-tafla2_temp[tafla2_temp$nchar>37,]
  tafla2_temp$nchar<-NULL
  
  tafla2<-rbind(tafla2, tafla2_temp)
  
}

print("Skröpun 1 lokið")
Sys.time()-timi1

# Make API-urls from px urls ----
#' This part is problematic
#' Code would need to be updated if new level 1 databases were added.
#' Using the the API to list tables and make the pxweb links from those would be faster and more robust if it were possible.
#' There are occasional eccentricities in pxweb links preventing the use of the API for listing.
#' The API approach would also miss duplicates on the pxweb.
#' If these problems are fixed I recommend using https://github.com/bgautijonsson/hagstofa_scraping/tree/master/R instead

tafla2<-tafla2[tafla2$nofn2!="",]
tafla2$apiurl2<-gsub("__", "/", tafla2$slod2)
tafla2$apiurl3<-gsub("https://px.hagstofa.is/pxis/pxweb/is/", "https://px.hagstofa.is:443/pxis/api/v1/is/", tafla2$apiurl2)
tafla2$apiurl3<-gsub("/Atvinnuvegir/Atvinnuvegir", "/Atvinnuvegir", tafla2$apiurl3)
tafla2$apiurl3<-gsub("/Efnahagur/Efnahagur", "/Efnahagur", tafla2$apiurl3)
tafla2$apiurl3<-gsub("/Ibuar/Ibuar", "/Ibuar", tafla2$apiurl3)
tafla2$apiurl3<-gsub("/Samfelag/Samfelag", "/Samfelag", tafla2$apiurl3)
tafla2$apiurl3<-gsub("/Sogulegar/Sogulegar", "/Sogulegar", tafla2$apiurl3)
tafla2$apiurl3<-gsub("/Umhverfi/Umhverfi", "/Umhverfi", tafla2$apiurl3)
tafla2$apiurl3<-gsub(".px/", ".px", tafla2$apiurl3)
tafla2$apiurl3<-gsub("%20", " ", tafla2$apiurl3)
tafla2$apiurl3<-gsub("px_autoopen=true", "", tafla2$apiurl3)
tafla2$apiurl3<-gsub("\\?", "", tafla2$apiurl3)
tafla2$apiurl3<-gsub("%C3%B0", "ð", tafla2$apiurl3)

tafla2$apiurl<-tafla2$apiurl3

tafla2$apiurl2<-NULL
tafla2$apiurl3<-NULL

# Make simple id strings from urls ---- 

#' The id is made from filenames at end of url
#' These are not unique for duplicate tables, a row number integer is added at end in case metadata might differ.

ids<-strsplit(tafla2$apiurl, split = "/")

id<-vector()
for(i in 1:length(ids)){
  temp<-ids[[i]]
  id[i]<-temp[length(temp)]
}
id<-gsub("px", "", id)
id<-gsub("\\.", "", id)

tafla2$id<-paste0(id, "_", 1:nrow(tafla2))

# Use API to get metadata -----

metadatalisti<-list()
timi2<-Sys.time()
breytur<-vector()

for(k in 1:nrow(tafla2)){
  #' First API query returns list of variables
  #' No data is requested so the API does not return other metadata
  #' The variable list is used to make a minimal query for data in order to get metadata
  pxd0<-pxweb_get(tafla2$apiurl[k])
  vars0<-pxd0$variables
  codes<-vector()
  values<-vector()
  for(i in 1:length(vars0)){
    codes[i]<-vars0[[i]]$code
    values[i]<-vars0[[i]]$values[1]
  }
  qlisti<-list()
  for(i in 1:length(vars0)){
    qlisti[[codes[i]]]<-values[i]
  }
  pxq <- pxweb_query(qlisti)
  pxd <- pxweb_get(tafla2$apiurl[k], pxq) #Second query
  pxd$data<-NULL #Remove data
  #' Metadata is placed in a list in an item named by table id
  idtemp<-tafla2$id[k]
  metadatalisti[[idtemp]]<-pxd
  #' Make vector of variable names and paste to make column of variable names for tafla2
  breytur_sub<-vector()
  for(i in 1:length(pxd$pxweb_metadata$variables)){
    breytur_sub[i]<-pxd$pxweb_metadata$variables[[i]]$text
  }
  breytur_sub<-paste0(breytur_sub, collapse = " ; ")
  breytur[k]<-breytur_sub
  print(paste0("Skröpun 2: tafla númer: ", k))
  print(tafla2$nofn2[k])
  # This approach could be improved using package polite! 
  Sys.sleep(0.5)
}
print("Skröpun 2 lokið")
Sys.time()-timi2

tafla2$breytur<-breytur

# Add value column ----

#' Make column of all values of all variables.
#' Can be used to add hidden searchable column in main table in app

valvec<-vector()

for(i in 1:nrow(tafla2)){
  sel1<-metadatalisti[[i]]$pxweb_metadata$variables
  skil<-""
  for(j in 1:length(sel1)){
    sel2<-sel1[[j]]$valueTexts
    skil<-paste0(skil, " ; ", paste0(sel2, collapse = " ; "))
  }
  valvec[i]<-skil
}

tafla2$gildi<-valvec

# Add oldest and most recent time series value ----

first<-vector()
last<-vector()

for(i in 1:nrow(tafla2)){
  sel1<-metadatalisti[[i]]$pxweb_metadata$variables
  for(j in 1:length(sel1)){
    sel2<-sel1[[j]]
    if(sel2$time){
      first[i]<-sel2$valueTexts[1]
      last[i]<-sel2$valueTexts[length(sel2$valueTexts)]
    }
  }
}

tafla2$first_time<-first
tafla2$last_time<-last

# Add date of last update ----

updatevec<-vector()

for(i in 1:nrow(tafla2)){
  updatevec[i]<-metadatalisti[[i]]$metadata[[1]]$updated
}

updatevec2<-as.Date.character(updatevec)

tafla2$last_update<-updatevec2

# Add column of duplicate titles ----

tafla2<-tafla2 |> 
  dplyr::left_join(
    tafla2 |> 
      dplyr::group_by(nofn2) |> 
      dplyr::summarise(n_duplicates=dplyr::n())
  )

# Scrape breadcrumbs, about table and comments from pxweb-----

#' Getting sensible hierarchical organisation of table from the API is a bit of a challenge
#' Scraping metadata from pxweb is also necessary at his point as it is not always consistent with API metadata
#' The inconsistency should of course be resolved!

skyringalisti<-list()
crumbs<-vector()
timi3<-Sys.time()
teljari<-1
remDr$open()
for(k in 1:nrow(tafla2)){
  remDr$navigate(tafla2$slod2[k])
  saekja1<-remDr$findElements(using = "id", "breadcrumb")
  crumb_sub<-unlist(saekja1[[1]]$getElementText())
  crumbs[k]<-crumb_sub
  skyringar<-remDr$findElements(using = "class", "accordion-header")
  for(i in 1:length(skyringar)){
    skyringar[[i]]$clickElement()
  }
  skyringatexti_em<-remDr$findElements(using = "class", "accordion-body")
  if(length(skyringatexti_em)>1){
    skyringatexti<-unlist(skyringatexti_em[[2]]$getElementText())
  }else{
    skyringatexti<-"null"
  }
  sub_headers<-remDr$findElements(using = "class", "nested-accordion-header")
  for(i in 1:length(sub_headers)){
    sub_headers[[i]]$clickElement()
  }
  sub_headers<-remDr$findElements(using = "class", "nested-accordion-header")
  sub_textar<-vector()
  for(i in 1:length(sub_headers)){
    sub_textar[i]<-sub_headers[[i]]$getElementText()
  }
  sub_bodies<-remDr$findElements(using = "class", "nested-accordion-body")
  sub_bodies_textar<-vector()
  for(i in 1:length(sub_headers)){
    sub_bodies_textar[i]<-sub_bodies[[i]]$getElementText()
  }
  idtemp<-tafla2$id[k]
  skyringalisti0<-list()
  skyringalisti0[["skyringatexti"]]<-skyringatexti
  skyringalisti0[["sub_textar"]]<-sub_textar
  skyringalisti0[["sub_bodies_textar"]]<-sub_bodies_textar
  print(paste0("Skröpun 3: tafla númer: ", teljari))
  skyringalisti[[idtemp]]<-skyringalisti0
  teljari<-teljari+1
}

print("Skröpun 3 lokið")
Sys.time()-timi3

# Split breadcrumbs to make categories

crumbsplit<-strsplit(crumbs, split = "/ ")
crumblengd<-sapply(crumbsplit, length)

crumb1<-sapply(crumbsplit, "[[", 2)
crumb2<-sapply(crumbsplit, "[[", 3)
crumb3<-sapply(crumbsplit, "[[", 4)

tafla2$crumbs<-crumbs
tafla2$crumbs1<-crumb1
tafla2$crumbs2<-crumb2
tafla2$crumbs3<-crumb3

# Add time stamp

metadatalisti$timastimpill_char<-as.character.Date(timi0)
metadatalisti$timastimpill_POSIXct<-timi0


saveRDS(tafla2, paste0(dname,"/uttak/maintab", as.numeric(Sys.time()), ".rds"))
saveRDS(metadatalisti, paste0(dname,"/uttak/apimeta", as.numeric(Sys.time()), ".rds"))
saveRDS(skyringalisti, paste0(dname,"/uttak/pxwebmeta", as.numeric(Sys.time()), ".rds"))

print("Uppfæringu lokið")
Sys.time()-timi0
