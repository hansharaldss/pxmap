
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(openxlsx)
library(shinyjs)

# Functions ----

#' Function that checks API-metadata variable info.
#' If the variable is a time variable the range is returned formatted into a string.
#' If the value text is more than 4 characters the first 4 characters are used
#' parameter node an item from apimeta$id$pxweb_metadata$variables
#' #' Note: id is the unique identifier for the variable in main table

timecheck<-function(node){
  tekk1<-node$time
  
  if(tekk1){
    vt<-node$valueTexts
    lns<-nchar(vt)
    tekk2<-sum(lns==4)==length(lns)
    if(!tekk2){
      vt<-substring(vt, 1, 4)
    }
    vt2<-as.numeric(vt)
    r<-range(vt2)
    skil<-paste0(", árabil: ", r[1], " - ", r[2], ", ")
  }else{
    skil<-""
  }
  return(skil)
}

#' A function that returns the number of distinct values a variable takes
#' parameter node an item from apimeta$id$pxweb_metadata$variables
#' Note: id is the unique identifier for the variable in main table

lengd<-function(node){
  vt<-node$valueTexts
  ln<-length(vt)
  skil<-paste0(ln, " gildi")
}

# Data and setup ----

# Icelandic language labels for datatable

DTsetup2<-list(
  emptyTable="Ekkert fannst",
  info="Sýnir _START_ til _END_ af _TOTAL_ töflum",
  infoFiltered="(fækkað úr _MAX_ töflum samtals)",
  lengthMenu="Sýna mest _MENU_ töflur",
  search="Aðalleit",
  zeroRecords="Ekkert fannst",
  paginate=list(
    "first"="Fyrsta",
    "last"="Síðasta",
    "next"="Næsta",
    "previous"="Til baka"
  )
)

# Main table
tafla<-readRDS("maintab1726517111.01599.rds")
# Metadata returned by API
apimeta<-readRDS("apimeta1726517111.26501.rds")
#Metadata from pxweb
pxwebmeta<-readRDS("pxwebmeta1726517111.74485.rds")

# Extra variable added after last scrape. Should be moved to scraping script.

timi<-rep("Nei", nrow(tafla))
timi[!is.na(tafla$first_time)]<-"Já"
tafla$timi<-timi


# UI ----

ui <- fluidPage(
    tags$head(tags$style('body {font-family: Arial;}')),
    tags$style(HTML(
      ".shiny-output-error { visibility: hidden; }
       .shiny-output-error:before { visibility: hidden;}"
    )),
    useShinyjs(),
    titlePanel("pxmap"),
    br(),
    "Yfirlit yfir px-töflur á vef Hagstofu Íslands",
    uiOutput("dateui"),
    #br(),
    hr(),
    ## Filter inputs ----
    fluidRow(
      style="padding: 10px;",
      column(2,
             br(),
             dropdown(inputId ="crumbs1_dropdown", label="Flokkur 1",
                      awesomeCheckboxGroup("crumbs1_inp",
                                           label="",
                                           choices = unique(tafla$crumbs1),
                                           selected = unique(tafla$crumbs1)
                      ),
                      br(),
                      actionButton("unsel_crumbs1", "Afhaka allt"),
                      shinyjs::disabled(actionButton("sel_crumbs1", "Velja allt")) 
             ),
             #br(),
             textOutput("filtexti1")
      ),
      column(2,
             uiOutput("crumbfilter2")
             ),
      column(2,
             uiOutput("crumbfilter3")
      )
    ),
    ## Optional columns (Aukadálkar) ----
    fluidRow(
      style="padding: 10px;",
      dropdown(inputId ="extravars", label="Aukadálkar",
               awesomeCheckboxGroup("extravars_inp",
                                    label="",
                                    choices = c(`Elsta skráning`="first_time",
                                                `Nýjasta skráning`="last_time",
                                                `Síðast uppfært`="last_update",
                                                `Fjöldi endurtekninga`="n_duplicates",
                                                `Með tímabreytu`="timi"),
                                    selected = NULL)
               ),
      br(),
      uiOutput("extravarcomment"),
      ## Main table ----
      dataTableOutput("adaltafla"),
      textOutput("toflutexti"),
      #helpText("Þegar aðallleit er notuð til að leita er einnig leitað í öllum gildum sem skiptibreytur taka"),
      tags$div(
        title="Hlaða niður yfirlitstöflu eins og hún hefur verið valin hér að ofan",
        downloadButton('tafla.xlsx', label="Hlaða niður yfirlitstöflu (xlxs)")
      ),
      br()
    ),
    ## Selected table information ----
    fluidRow(
      style="padding: 20px;",
      uiOutput("heiti_ui"),
      br(),
      tabsetPanel(
        ### Paths (Slóðir)----
        tabPanel("Slóðir",
                 uiOutput("slodir_ui")
                 ),
        ### Variables (Skiptibreytur) ----
        tabPanel("Skiptibreytur",
                 checkboxInput("syna_gildi", "Sýna gildi", value = F),
                 br(),
                 uiOutput("gildatexti")
                 ),
        ### Metadata section (Lýsigögn) ----
        tabPanel("Lýsigögn",
                 tabsetPanel(
                   tabPanel(
                     #### pxweb metadata ----
                     "Lýsigögn (px-vefur)",
                     fluidRow(
                       style="padding: 40px;",
                       "Upplýsingar um töflu af px-vef:",
                       br(),
                       br(),
                       "Athugið að dagsetning fyrir nýjustu uppfærslu er sú sem hún var þegar gögn voru síðast skröpuð af vef Hagstofunnar. Töflur kunna að hafa verið uppfærðar eftir það.",
                       br(),
                       br(),
                       fluidRow(
                         style = "border: 1px solid black; padding: 10px",
                         br(),
                         uiOutput("about"),
                         br()
                       ),
                       br(),
                       "Skýringar sem fylgja töflu á px-vef:",
                       br(),
                       br(),
                       fluidRow(
                         style = "border: 1px solid black; padding: 10px",
                         br(),
                         uiOutput("skyringar"),
                         br()
                       )
                     )
                   ),
                   tabPanel(
                     #### API-metadata ----
                      "Lýsigögn (API)",
                      br(),
                      "Hér birtast lýsigögn úr skilalista þegar gögn eru sótt í gegn um API með R-pakkanum pxweb. Úttak birtist eins og það kemur af kúnni.",
                      br(),
                      br(),
                      "Athugið að dagsetning fyrir nýjustu uppfærslu er sú sem hún var þegar gögn voru síðast skröpuð af vef Hagstofunnar. Töflur kunna að hafa verið uppfærðar eftir það.",
                      br(),
                      br(),
                      checkboxGroupInput("apisyna", "Sýna", choices = c("columns", "comments", "metadata",
                                                                        "pxweb_metadata", "url", "time_stamp"),
                                         selected = "metadata", inline = T),
                      textOutput("apitext1"),
                      verbatimTextOutput("apiout1"),
                      textOutput("apitext2"),
                      verbatimTextOutput("apiout2"),
                      textOutput("apitext3"),
                      verbatimTextOutput("apiout3"),
                      textOutput("apitext4"),
                      verbatimTextOutput("apiout4"),
                      textOutput("apitext5"),
                      verbatimTextOutput("apiout5"),
                      textOutput("apitext6"),
                      verbatimTextOutput("apiout6")
             )
                   
            )
          )
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br()
    )
    
)

# Server ----

server <- function(input, output, session) {
  
  ## Date ----
  
  # The date when data was last scraped is logged in apimeta list. 
  # UI output displaying formatted date.
  
  output$dateui<-renderUI({
    skil<-paste0("Gögn síðast sótt: ", format(apimeta$timastimpill_POSIXct, "%d.%m.%Y"))
    HTML(skil)
  })
  
  
  #Extravar comments ----
  
  # Some "Aukadálkar" inputs require explanation. 
  # UI output displaying relevant comments.
  
  output$extravarcomment<-renderUI({
    skil<-""
    if("first_time"%in%input$extravars_inp|"last_time"%in%input$extravars_inp){
      skil<-paste0(skil, "Aðeins er hægt að sjá elstu og nýjustu skráningu fyrir þær tölfur sem eru með tímabreytu skilgreinda í API-skilum<br><br>")
    }
    if("last_update"%in%input$extravars_inp){
      skil<-paste0(skil, "Tímasetning fyrir síðustu uppfærslu birtist eins og hún kemur fram í API-skilum. Sum gildi eru ómöguleg.<br><br>")
    }
    if("n_duplicates"%in%input$extravars_inp){
      skil<-paste0(skil, "Fjöldi endurtekninga segir til um hversu oft nafn töflu kemur fyrir á px-vef.<br><br>")
    }
    if("timi"%in%input$extravars_inp){
      skil<-paste0(skil, "Breytan \"Með tímabreytu\" segir til um hvort tafla er með skilgreinda tímabreytu í API-skilum.<br><br>")
    }
    return(HTML(skil))
  })

  
  
  ## filters---- 
  
  # Main table is filtered sequentially by inputs crumbs1_inp, crumbs2_inp and crumbs3_inp
  
  # Reactive table filtered by input$crumbs1_inp

  tafla_filt1<-reactive({
    tafla_ut<- tafla%>%
      filter(crumbs1%in%input$crumbs1_inp)
    return(tafla_ut)
  })
  
  # Text output displaying the number of level 1 categories in unfiltered main table and the number of selected categories.
  
  output$filtexti1<-renderText({
    teljari<-length(input$crumbs1_inp)
    nefnari<-length(unique(tafla$crumbs1))
      
    paste0("( ", teljari, " / ", nefnari, " )")
  })
  
  # Udates to select/unselect all when action buttons sel_crumbs1 or unsel_crumbs1 are used
  
  observeEvent(input$unsel_crumbs1, {
    updateCheckboxGroupInput(session = session, inputId = "crumbs1_inp", selected = "")
  })
  
  observeEvent(input$sel_crumbs1, {
    updateCheckboxGroupInput(session = session, inputId = "crumbs1_inp", selected = unique(tafla$crumbs1))
  })
  
  observe(
    if(length(input$crumbs1_inp)<length(unique(tafla$crumbs1))){
      enable("sel_crumbs1")
    }else{
      disable("sel_crumbs1")
    }
  )
  
  observe(
    if(length(input$crumbs1_inp)==0){
      disable("unsel_crumbs1")
    }else{
      enable("unsel_crumbs1")
    }
  )
  
  # Dynamic input for selecting level 2 categories from those represented in table after first filtering
  
  output$crumbfilter2<-renderUI({
    tafla2<-tafla_filt1()
      
    fluidRow(
      useShinyjs(),
      style="padding: 20px;",
      dropdown(inputId ="crumbs2_dropdown", label="Flokkur 2",
                        awesomeCheckboxGroup("crumbs2_inp",
                                             label="",
                                             choices = unique(tafla2$crumbs2),
                                             selected = unique(tafla2$crumbs2)
                      ),
               br(),
               actionButton("unsel_crumbs2", "Afhaka allt"),
               shinyjs::disabled(actionButton("sel_crumbs2", "Velja allt")) 
      ),
      textOutput("filtexti2")
      )
    })
  
  # Udates to select/unselect all when action buttons sel_crumbs2 or unsel_crumbs2 are used
  
  observeEvent(input$unsel_crumbs2, {
    updateCheckboxGroupInput(session = session, inputId = "crumbs2_inp", selected = "")
  })
  
  observeEvent(input$sel_crumbs2, {
    updateCheckboxGroupInput(session = session, inputId = "crumbs2_inp", selected = unique(tafla_filt1()$crumbs2))
  })
  
  observe(
    if(length(input$crumbs2_inp)<length(unique(tafla_filt1()$crumbs2))){
      enable("sel_crumbs2")
    }else{
      disable("sel_crumbs2")
    }
  )
  
  observe(
    if(length(input$crumbs2_inp)==0){
      disable("unsel_crumbs2")
    }else{
      enable("unsel_crumbs2")
    }
  )
    
  # Text output displaying the number of level 2 categories in main table after first filtering and the number of selected categories.
  
  output$filtexti2<-renderText({
    tafla2<-tafla_filt1()
    
    teljari<-length(input$crumbs2_inp)
    nefnari<-length(unique(tafla2$crumbs2))
    
    paste0("( ", teljari, " / ", nefnari, " )")
  })
  
  # Reactive table filtered by input$crumbs2_inp
  
  tafla_filt2<-reactive({
    tafla2<-tafla_filt1()
    
    tafla_ut<-tafla2%>%
      filter(crumbs2%in%input$crumbs2_inp)
    
    return(tafla_ut)
  })
  
  # Dynamic input for selecting level 3 categories from those represented in table after second filtering
    
  output$crumbfilter3<-renderUI({
    tafla3<-tafla_filt2()
    
    fluidRow(
      style="padding: 20px;",
      dropdown(inputId ="crumbs3_dropdown", label="Flokkur 3",
               awesomeCheckboxGroup("crumbs3_inp",
                                    label="",
                                    choices = unique(tafla3$crumbs3),
                                    selected = unique(tafla3$crumbs3)
               ),
               br(),
               actionButton("unsel_crumbs3", "Afhaka allt"),
               shinyjs::disabled(actionButton("sel_crumbs3", "Velja allt")) 
      ),
      #br(),
      textOutput("filtexti3")
    )
  })
  
  # Udates to select/unselect all when action buttons sel_crumbs3 or unsel_crumbs3 are used
  
  observeEvent(input$unsel_crumbs3, {
    updateCheckboxGroupInput(session = session, inputId = "crumbs3_inp", selected = "")
  })
  
  observeEvent(input$sel_crumbs3, {
    updateCheckboxGroupInput(session = session, inputId = "crumbs3_inp", selected = unique(tafla_filt2()$crumbs3))
  })
  
  observe(
    if(length(input$crumbs3_inp)<length(unique(tafla_filt2()$crumbs3))){
      enable("sel_crumbs3")
    }else{
      disable("sel_crumbs3")
    }
  )
  
  observe(
    if(length(input$crumbs3_inp)==0){
      disable("unsel_crumbs3")
    }else{
      enable("unsel_crumbs3")
    }
  )
  
  # Text output displaying the number of level 3 categories in main table after second filtering and the number of selected categories.
  
  output$filtexti3<-renderText({
    tafla3<-tafla_filt2()
    
    teljari<-length(input$crumbs3_inp)
    nefnari<-length(unique(tafla3$crumbs3))
    
    paste0("( ", teljari, " / ", nefnari, " )")
  })
  
  # Reactive table filtered by input$crumbs3_inp
  
  tafla_filt3<-reactive({
    tafla3<-tafla_filt2()
    
    tafla_ut<-tafla3%>%
      filter(crumbs3%in%input$crumbs3_inp)
    
  return(tafla_ut)
  })
    
    
  ## Table count ----
  
  # Text output indicating the total number of tables in filtered main table, number of distinct table names 
  # and number of table names appearing more than once
    
  output$toflutexti<-renderText({
    tafla4<-tafla_filt3()
    tafla_ut<-tafla4[input$adaltafla_rows_all,]
    
    tala1<-length(unique(tafla_ut$nofn2))
    
    tidnitafla<-tafla_ut %>%
      group_by(n_duplicates) %>%
      tally() %>%
      filter(n_duplicates>1) %>%
      mutate(fj_tafl=n/n_duplicates)
    
    tala2<-sum(tidnitafla$fj_tafl)
    
    paste0(tala1, " einstök töfluheiti, ", tala2, " töfluheiti koma oftar en einu sinni fyrir")
  })
    
  ## Main table ----
  
  # Reactive table pairing variable names in main table and display names
  # Not really reactive, this is a workaround to solve problems that may be particular to server mvst
  
  labtab<-reactive({
    data.frame("tnames"=c("first_time", "last_time", "last_update", "n_duplicates", "timi"),
               "dnames"=c("Elsta skráning", "Nýjasta skráning", "Síðast uppfært", "Fjöldi endurtekninga",
                          "Með tímabreytu"))
  })
  
  # Reactive vector of display names for selected "Aukadálkar"   
   
  labs_varsel<-reactive({
    tab<-labtab()[labtab()$tnames%in%input$extravars_inp,]
    skil<-tab$dnames
    return(skil)
  })
  
  # Reactive datatable with main table
  
  preptafla<-reactive({
    tafla4<-tafla_filt3()
    
    tafla_ut<-tafla4%>%
      select("crumbs2", "crumbs3", "nofn2", "breytur", input$extravars_inp)%>%
      datatable(colnames=c("Flokkur 2", "Flokkur 3", "Heiti", "Skiptibreytur", labs_varsel()),
                rownames= FALSE, filter = 'top',
                selection = list(mode='single', selected=1),
                options = list(
                  language=DTsetup2),
        )
      
      
    return(tafla_ut)
  })
  
  # Main table output 
  
  output$adaltafla<-renderDataTable({preptafla()})
    
    
  ## Table information  ----
  
  ###  Paths (Slóðir) ----
  
  # Reactive string with name of selected table
    
  heiti_rctv<-reactive({
    tafla4<-tafla_filt3()
    tala<-input$adaltafla_rows_selected
    
    return(tafla4$nofn2[tala])
  })
  
  # UI output. Name of selected table formated in html.  
  
  output$heiti_ui<-renderUI({
    skil<-paste0("<font size=\"+2\"><b>Heiti: </b>", heiti_rctv(), "</font>")
    HTML(skil)
  })
  
  # Reactive string with selected table pxweb url.
  
  slod_px_rctv<-reactive({
    tafla4<-tafla_filt3()
    tala<-input$adaltafla_rows_selected
    
    return(tafla4$slod2[tala])
  })
  
  # Reactive string with selected table API url.
  
  slod_api_rctv<-reactive({
    tafla4<-tafla_filt3()
    tala<-input$adaltafla_rows_selected
    
    return(tafla4$apiurl[tala])
  })
  
  # Reactive string with selected table breadcrumbs trail.
  
  tre_rctv<-reactive({
    tafla4<-tafla_filt3()
    tala<-input$adaltafla_rows_selected
    
    return(tafla4$crumbs[tala])
  })
  
  # UI output. pxweb url, API url and breadcrumbs formated in html.  
  
  
  output$slodir_ui<-renderUI({
    skil<-"<br>"
    skil<-paste0(skil, "<b>Slóð á px-vef:</b><br> <a href=\"", slod_px_rctv(), "\">",slod_px_rctv(),"</a>")
    skil<-paste0(skil, "<br><br>")
    skil<-paste0(skil, "<b>API-slóð:</b><br>", slod_api_rctv())
    skil<-paste0(skil, "<br><br>")
    skil<-paste0(skil, "<b>Staðsetning í tré:</b><br>", tre_rctv())
    HTML(skil)
  })
  
  ### Variables (Skiptibreytur) ----
  
  # Reactive string. Selected table unique ID
  
  id_rctv<-reactive({
    tafla4<-tafla_filt3()
    tala<-input$adaltafla_rows_selected
    
    return(tafla4$id[tala])
  })
  
  # Reactive list with variable description from API
  
  vars_rctv<-reactive({
    nota<-apimeta[[id_rctv()]]$pxweb_metadata$variables
    return(nota)
  })

  # UI output. Displays information about variables formatted in HTML
  
  output$gildatexti<-renderUI({
    vartre<-vars_rctv()
    
    if(input$syna_gildi){
      skil<-""
      for(i in 1:length(vartre)){
        skil<-paste0(skil, "<b>", vartre[[i]]$text, "</b> ")
        skil<-paste0(skil, "&nbsp&nbsp&nbsp<font size=\"-1\">",lengd(vartre[[i]]), "</font>")
        skil<-paste0(skil, " <font size=\"-1\">",timecheck(vartre[[i]]), "</font>")
        skil<-paste0(skil, "<br>")
        gildi<-paste0(vartre[[i]]$valueTexts, collapse = ", ")
        skil<-paste0(skil,"<font size=\"-1\">", gildi, "</font><br><br>")
      }
    }else{
      skil<-""
      for(i in 1:length(vartre)){
        skil<-paste0(skil, "<b>", vartre[[i]]$text, "</b> ")
        skil<-paste0(skil, "&nbsp&nbsp&nbsp<font size=\"-1\">",lengd(vartre[[i]]), "</font>")
        skil<-paste0(skil, "<font size=\"-1\">",timecheck(vartre[[i]]), "</font>")
        skil<-paste0(skil, "<br><br>")
      }
    }
      
    skil<-paste0(skil, "<br><br><font size=\"-1\">Árabil birtast aðeins við hlið breytuheita ef skiptibreytur eru skilgreindar sem tímabreytur í lýsigögnum</font>")
      
    return(HTML(skil))
      
  })
    
    
  ### Metadata section (Lýsigögn) ----
  
  #### pxweb metadata ----
  
  # Reactive string with table comment (skýringar) from pxweb.
  
  skyr_rctv<-reactive({
    return(pxwebmeta[[id_rctv()]]$skyringatexti)
  })
  
  # UI output.Returns skyr_rctv formatted in html if a comment was found during scraping.
  # If no comment was found a notification is displayed.
    
  output$skyringar<-renderUI({
    
    ut<-""
    
    if(skyr_rctv()=="null"){
      ut<-paste0(ut,"<font color=\"red\">Enginn skýringatexti fannst þegar gögn voru sótt</font>")
    }else{
      hratt<-skyr_rctv()
      hratt<-gsub("\\n", "<br><br>", hratt)
      ut<-paste0(ut, "<i>", hratt, "</i>")
    }
    HTML(ut)
  })
  
  # Reactive list of two char vectors
  # One list contains headings from "Um töflu" section on pxweb, other contains the text contents 
    
  about_rctv<-reactive({
    nota<-list()
    nota$heiti<-pxwebmeta[[id_rctv()]]$sub_textar
    nota$textar<-pxwebmeta[[id_rctv()]]$sub_bodies_textar
    return(nota)
  })
    
  # UI ouput with contents of about_rctv formatted in html
  
  output$about<-renderUI({
    
    ut<-""
    heiti<-unlist(about_rctv()$heiti)
    texti<-unlist(about_rctv()$textar)
    
    for(i in 1:length(heiti)){
      ut<-paste0(ut, "<i><b>", heiti[i], ":</b> ", texti[i], "<br></i>")
    }
    
    HTML(ut)
  })
    
  #### API-metadata ----
  
  # Outputs named apioutput are verbatim text outputs diplaying the contents of each item in list returned by API
  # Outputs named apitext are text outputs with headings for each section
  # Both outputs are only displayed if the item is seleced in input apisyna
  
  output$apiout1<-renderPrint({
    if("columns"%in%input$apisyna){
      print(apimeta[[id_rctv()]]$columns)
    }
  })
  output$apitext1<-renderText({if("columns"%in%input$apisyna){"columns"}})
    
  output$apiout2<-renderPrint({
    if("comments"%in%input$apisyna){
      print(apimeta[[id_rctv()]]$comments) 
    }
  })
  output$apitext2<-renderText({if("comments"%in%input$apisyna){"comments"}})
    
  output$apiout3<-renderPrint({
    if("metadata"%in%input$apisyna){
      print(apimeta[[id_rctv()]]$metadata)
    }
  })
  output$apitext3<-renderText({if("metadata"%in%input$apisyna){"metadata"}})
  
  # Output apioutput4 prints the contents of two items 
  
  output$apiout4<-renderPrint({
    if("pxweb_metadata"%in%input$apisyna){
      print(apimeta[[id_rctv()]]$pxweb_metadata$title)
      print(apimeta[[id_rctv()]]$pxweb_metadata$variables)
    }
  })
  output$apitext4<-renderText({if("pxweb_metadata"%in%input$apisyna){"pxweb_metadata"}})
    
  output$apiout5<-renderPrint({
    if("url"%in%input$apisyna){
      print(apimeta[[id_rctv()]]$url)
    }
  })
  output$apitext5<-renderText({if("url"%in%input$apisyna){"url"}})
    
  output$apiout6<-renderPrint({
    if("time_stamp"%in%input$apisyna){
      print(apimeta[[id_rctv()]]$time_stamp)
    }
  })
  output$apitext6<-renderText({if("time_stamp"%in%input$apisyna){"time_stamp"}})
    
  ## Download main table----
  
  # Reactive table. Displayed rows from main table selected. Columns selected and renamed for xlsx export. 
    
  dwnld_rctv<-reactive({
    tafla4<-tafla4<-tafla_filt3()
    tafla_ut<-tafla4[input$adaltafla_rows_all,]%>%
      select("Flokkur1"=crumbs1,
             "Flokkur2"=crumbs2,
             "Flokkur3"=crumbs3,
             "Heiti"=nofn2,
             "PX_slod"=slod2,
             "API_slod"=apiurl,
             "Skiptibreytur"=breytur,
             "Fjoldi_med_sama_titil"=n_duplicates,
             "Fyrsta_timaildi"=first_time,
             "Sidasta_timagildi"=last_time)
  })
  
  # Download handler for dwnld_rctv
  
  output$tafla.xlsx <- downloadHandler(
    filename = function() {
      paste("tafla_ut.xlsx")
    },
    content = function(file) {
      write.xlsx(dwnld_rctv(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
