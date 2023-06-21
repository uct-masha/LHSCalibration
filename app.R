library(tidyverse)
library(glue)
library(here)
library(lubridate)
library(shiny)
library(shinyjs)
library(bslib)
library(thematic)
library(rlog)
library(readxl)
library(shinyBS)
library(bsplus)
library(reactable)
source("LHS.R")

CUSTOMIZE_THEME <- F
DEBUG <- T
COLOUR_FG <- "#073B82"  # Dark blue from MASHA logo is 073B82, light blue is 16B5F0

appName <- "Latin Hypercube Sampling"

Sys.setenv("LOG_LEVEL" = "WARN")

# https://shiny.rstudio.com/reference/shiny/0.14/shiny-options.html
options(shiny.launch.browser=FALSE,
        shiny.maxRequestSize=30*1024^2
        )

if (DEBUG) {
  options(shiny.port=7777,
          shiny.autoreload=TRUE,
          shiny.fullstacktrace=TRUE  # Shiny prints a line number in the trace.
  )
}

theme <- bs_theme(version=5)
if (!CUSTOMIZE_THEME) {
  theme <- bs_theme_update(theme, primary = COLOUR_FG, base_font = font_google("Fira Sans"), 
                           font_scale = NULL, bootswatch = "vapor")
}

thematic::thematic_shiny(font = "auto")

# Load initial data ####
tbParmsExample <- readxl::read_excel('ParametersExample.xlsx', sheet=1)

# UI ####
ui <- fluidPage(
  theme = theme,
  title = appName,
  lang = 'en',
  # bg=COLOUR_FG,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel="icon", href=base64enc::dataURI(file = 'www/favico.ico', mime = 'image/x-icon')),
    useShinyjs()
  ),
  fluidRow(column(12,div(h4(img(src='square.png',width='32px'),"Latin Hypercube Sampling"),style='padding:1rem 0 0 1rem'))),
  fluidRow(column(12,
    tabsetPanel(id = "tabNav",
      # Tab 1: Upload Parameter Sheet ####
      tabPanel(title = "1. Upload Parameter Sheet", value='tab1', tagList(
        fluidRow(
          column(offset = 2, width = 8,
                 h3("How this app works"),
                 p("This app will lead you through model calibration with Latin Hypercube Sampling which is a way to choose parameter sets from given distributions. You will be able to see the distributions of the parameters in the resulting parameter sets and once you've run these through your model you can see the how resulting goodness of fit typically varies with each parameter."))
        ),
        fluidRow(
          column(offset = 2, width = 8,
                 h3("Getting parameters in the right format:"),
                 p("To get started, you will need to put your particular parameters into a format this app understands. You can download a template below and modify it accordingly. Note that the only (currently) supported values for ",tags$code('Distribution')," are ",tags$code('tri/trirate/unif/unifrate/norm/normrate/const/constrate'),". Adding 'rate' to a Distribution has the effect of sampling the distribution of that parameter (call it x) as days (calculated as 365.25/x) but then returning the value for x as a rate. See nu in the example parameter sheet for an example of this and note the distribution of the sampled values of nu in step 2."),
                 downloadLink('downloadTemplateParams', label="Download Example Parameter Sheet"))
        ),
        fluidRow(
          column(offset = 2, width = 8,
                 h3("Uploading your parameter sheet:"),
                 p("When you have set up your excel workbook with parameters on the first sheet, and you have the headers ",tags$code("Name"),",",tags$code("Distribution"),",",tags$code("mean"),",",tags$code("low"),", and ",tags$code("high"),", you can upload the sheet below to continue."),
                 fileInput('fileParamsBasic', label='Upload Sheet'))
        )
      )),
      # Tab 2: Make LHS Parameter Set ####
      tabPanel(title = "2. Create Parameter Sets", value='tab2', tagList(
          fluidRow(
            column(offset = 2, width = 8,
                   h3("Create parameter sets using LSH"),
                   p("In this section you will create parameter sets from the parameters you uploaded previously. If you would like to use the example parameter sheet instead you can leave the checkbox ticked. Select how many parameter sets you wish to create and click the button to proceed."),
                   shiny::checkboxInput(inputId = 'useExampleParms', label = 'Use the example parameter set', value=T),
                   shiny::numericInput(inputId = 'numParmSets', label = 'Number of parameter sets:', value=1000, min=1,max=1e7),
                   shiny::actionButton(inputId = 'goMakeLHS', label = "Create LHS Parameter Sets"))
          ),
          fluidRow(
            column(offset = 2, width = 8,
                   shiny::htmlOutput(outputId = 'lhsParmSetOutput')
            )
          )
        )),
        # Tab 3: Perform Calibration ####
      tabPanel(title = "3. Perform Calibration", value='tab3', tagList(
          fluidRow(
            column(offset = 2, width = 8, 
                   h3("Performing Calibration:"),
                   p('Once you have downloaded your LHS parameter sets, you should record how well each parameter set fits your data. To do this, run your model for each parameter set and compare the model output to your data, forming a Goodness-Of-Fit estimate(GOF).'),
                   p('For example suppose you have weekly reported disease incidence data and your model predicts daily reported disease incidence. You may then need to calculate the weekly incidence predicted by your model so you can compare it with your data. I would usually use ',tags$code('lubridate'),' for this ',tags$a('(see here)',href='https://lubridate.tidyverse.org/'),'.'),
                   p('Other functions from tidyverse you may wish to look into include ',tags$code('mutate()'),', ',tags$code('group_by()'),', ',tags$code('rowwise()'),', ',tags$code('group_map()'),' and ',tags$code('summarise()'),'. You may also be interested in the ',tags$code('multidplyr'),' library since this fits its ideal usecase quite well.'),
                   h4('Negative Log-Likelihood'),
                     p('Supposing that your data is weekly and you have a comparable weekly model output, you could evaluate the likelihood of observing the model output given the data and combine these over all weeks using the multiplication rule. Since the log of a product is a sum of the logs, the log of the probability is frequently used, so we sum the logs to come up with the joint likelihood of seeing all of the model output given all of the data.'),
                     p('Lastly, since the log of a very small number is negative we negate this number to form the negative log likelihood. Since we negated it we now wish to minimize this number in order to maximize the likelihood of our model matching the data. In R we would write something like:',tags$code("NLL=-sum(dpois(inc_data, lambda=inc_model, log=TRUE))")),
                   h4('Coefficient of Determination'),
                     p('A simple measure of goodness of fit is the coefficient of determination. To calculate this you first form the sum of the squares of the residuals. Recall that the residuals are just the difference between your data and your model at each point. You compare this value to the sum of squares of your data minus the mean of your data.'),
                     p('The ratio of these gives a number which is 1 when your model varies from the data as much as the data varies from its mean. Another property of this ratio is that when the model fits the data perfectly it gives zero. Lastly for facetiously awful models it will be arbitrarily large since the residuals will be large.'),
                     p('We call one minus this ratio the coefficient of determination, denoted R^2, and it is a measure of how well the model fits the data. In R we would write something like:',tags$code("GOF=1-sum((inc_data-inc_model)^2)/sum((inc_data-mean(inc_data))^2)")),
                   h4('Discrete Values'),
                     p('You may want to consider all parameters which satisfy some logical criteria and thus your measure could be the number of criteria satisfied. This may be one way to hone in on better parameter distributions or to subset your model by parameter distributions which give particular outcomes. Usually though, if you are doing this, you have a particular reason for doing so but I wanted to mention it regardless.'),
                   h4('Goodness of fit'),
                     p('Once you form the GOF (or NLL, which measures Badness-Of-Fit) for each parameter set you should include it in a ',tags$code('GOF'),' column and upload the result in the ',actionLink('goToTab4','next tab'),'.')
            )
          )
      )),
      # Tab 4: Visualise Calibration Results ####
      tabPanel(title = "4. Visualise Calibration Results", value='tab4', tagList(
        fluidRow(
          column(offset = 2, width = 8, 
                 h3("Visualise Calibration Results:"),
                 p('If you would like to view and/or upload an example results file based on our example parameter sheet and 1000 LHS parameter sets, you can find one here:'),
                 p(downloadLink('downloadGOFExample', label="Download Example Results File")),
                 p('When you are ready you can upload your results below:'),
                 fileInput('fileGOFResults', label='Upload Calibration Results'),
                 htmlOutput('resultsHTML'))
        ))
      ) # /tabPanel
    )  # /tabsetPanel
  )) # /column, /fluidRow
)    # /fluidPage

server <- function(input, output, session) {
  log_trace(glue::glue("Session started: {session$token}"))
  
  if(CUSTOMIZE_THEME) {bs_themer(gfonts_update = T)}
  shinyjs::disable("useExampleParms")
  
  uploadedFileParms <- reactiveValues(
    name = 'ParametersExample.xlsx',
    location = 'ParametersExample.xlsx',
    isCustom = F)
  
  lhsResult <- reactiveValues(
    fname=NULL,
    parmsDynamic=NULL,
    parmsStatic=NULL)
  
  # Tab 1 ####
  output$downloadTemplateParams <- downloadHandler(
    filename = function() {
      'ParametersForLHSTemplate.xlsx'
    },
    content = function(file) {
      fs::file_copy('ParametersExample.xlsx', file)
    },
    contentType='application/vnd.ms-excel'
  )
  
  observeEvent(input$fileParamsBasic, {
    rlog::log_trace(glue::glue("fileParamsBasic uploaded: {input$fileParamsBasic}"))
    uploadedFileParms$name <- input$fileParamsBasic$name
    uploadedFileParms$location <- input$fileParamsBasic$datapath
    uploadedFileParms$isCustom <- T
    rlog::log_trace(glue::glue("enabling `useExampleParms`"))
    shinyjs::enable("useExampleParms")
    # also deselect useExampleParms since use probably wants to use their
    # uploaded data
    shiny::updateCheckboxInput(inputId = "useExampleParms", value = F)
    rlog::log_trace(glue::glue("showing the tab 'Download LHS Parameter Set'"))
    showTab(inputId = 'tabNav', target = 'tab2', select=T)
  })
  
  # Tab 2 ####
  #input$numParmSets,input$goMakeLHS,output$lhsParmSetOutput
  tbParmsCustom <- reactive({
    readxl::read_excel(uploadedFileParms$location, sheet=1)
  })
  tbParms <- reactive({
    if(input$useExampleParms){
      tbParmsExample
    } else {
      tbParmsCustom()
    }
  })
  observeEvent(input$goMakeLHS, {
    nSamples <- input$numParmSets
    log_trace(glue::glue("Making {nSamples} parameter sets from {ifelse(input$useExampleParms, 'example','custom')} parameter sheet."))
    
    # Validate:
    expectedColumnHeaders <- c("Name", "Distribution", "mean", "low", "high")
    requiredColumnHeaders <- setdiff(expectedColumnHeaders, colnames(tbParms()))
    log_trace(glue::glue("Validating {requiredColumnHeaders}"))
    validate(
      need(length(requiredColumnHeaders)==0,
           paste0('Missing column headers: ',
                  paste(requiredColumnHeaders,sep=', ')
                  )
           )
    )
    parms <- tbParms() %>% 
      transmute(parm=Name,
                dist=Distribution, # Each dist should be defined in tbDists in LHS.R, if it ends with rate we convert it as 365.25/x and sample on the transformed variables
                mean=mean, # The mean/expected value for the distributions
                lo=low,  # Make sure you call the lower number "lo"
                hi=high) # As above, call this "hi"
    
    parms.split = parms %>%
      mutate(mean=ifelse(dist=='constrate', 365.25/mean, mean)) %>% 
      # if it starts with const, it's not a dynamic parameter
      group_by(is.static=dist %>% str_starts("const")) %>% 
      group_split()
    
    parms.dynamic = parms.split[[1]]
    parms.static = parms.split[[2]]
    
    # Make a set of parameters
    log_trace(glue::glue("Calling lhs.sample"))
    parms.set.dynamic <- lhs.sample(tbParmsBase = parms.dynamic, nSamples = nSamples)
    
    parms.set.static <- parms.static %>%
      select(parm,mean) %>%
      pivot_wider(names_from=parm, values_from = mean)
    
    parms.sheets <- list(Dynamic=parms.set.dynamic, Static=parms.set.static)
    # Save them to disk
    fname <- tempfile(glue::glue('ParmSetsLHS_{nSamples}_'), fileext='.xlsx')
    
    log_trace(glue::glue("- Saving parms sheet as {fname}"))
    openxlsx::write.xlsx(parms.sheets, fname)
    
    log_trace(glue::glue("- Overwriting reactiveValues lhsResult[fname,parmsDynamic,parmsStatic]"))
    lhsResult$fname <- fname
    lhsResult$parmsDynamic <- parms.set.dynamic
    lhsResult$parmsStatic <- parms.set.static
  })
  
  output$lhsParmSetOutput <- renderUI({
    if (is.null(lhsResult$fname)) {
      tagList(
        br(),
        p("When you have created parameter sets they will show up here.")
      )
    } else {
      # https://datatables.net/reference/option/dom
      rateParms <- tbParms() %>% 
        transmute(parm=Name,
                  dist=Distribution) %>%
        filter(!str_starts(dist,'const'),str_ends(dist,'rate')) %>% 
        pull(parm)
      tbRates <- lhsResult$parmsDynamic %>% 
        pivot_longer(!sampleID, names_to = 'parameter', values_to = 'value')
      tbRatesInverted <- tbRates %>% 
        filter(parameter %in% rateParms) %>% 
        mutate(parameter = paste0('365/',parameter),
               value = 365/value
        )
      tbResult <- tbRates %>%
        arrange(parameter %in% rateParms) %>% 
        bind_rows(tbRatesInverted) %>% 
        mutate(parameter=as_factor(parameter))
      shiny::tagList(br(),
                     shiny::downloadLink(outputId = 'downloadLHS', label = 'Download table'),
                     br(),
                     renderPlot({
                       tbResult %>% 
                         ggplot(aes(x=value)) +
                          geom_histogram(bins = 50L, fill = "#EA39B8") +
                          # ggthemes::theme_hc() +
                          facet_wrap(vars(parameter), scales = 'free') +
                          labs(x="Parameter Value",
                               y="Frequency (Count)",
                               title = "Frequency for each parameter in LHS parameter sets")
                          
                     })# ,
                       # DT::renderDataTable(lhsResult$parmsDynamic, options = list(dom = 'tp'))
      )
    }
  })
  output$downloadLHS <- downloadHandler(filename=function(){"ParametersLHS.xlsx"},
                                        content=function(file) {fs::file_copy(lhsResult$fname, file)},
                                        contentType = 'application/vnd.ms-excel')
  
  # Tab 3 ####
  output$downloadGOFExample <- downloadHandler(
    filename = function() {
      'ParametersLHSWithGOF.xlsx'
    },
    content = function(file) {
      fs::file_copy('ParametersLHSWithGOF.xlsx', file)
    },
    contentType='application/vnd.ms-excel'
  )
  observeEvent(input$goToTab4, 
               showTab(inputId = 'tabNav', target = 'tab4', select=T)
  )
  
  # Tab 4 ####
  output$resultsHTML <- renderUI({
    if(is.null(input$fileGOFResults)) {
      tagList(
        p("A plot will appear here showing the best fits when you upload your results.")
      )
    } else {
      #
      rateParms <- tbParms() %>% 
        transmute(parm=Name,
                  dist=Distribution) %>%
        filter(!str_starts(dist,'const'),str_ends(dist,'rate')) %>% 
        pull(parm)
      tbRates <- read_excel(input$fileGOFResults$datapath, sheet=1) %>%
        select(!sampleID) %>%
        pivot_longer(!GOF, names_to='parameter')
      tbRatesInverted <- tbRates %>% 
        filter(parameter %in% rateParms) %>% 
        mutate(parameter = paste0('365/',parameter),
               value = 365/value
        )
      tbResult <- tbRates %>%
        arrange(parameter %in% rateParms) %>% 
        bind_rows(tbRatesInverted) %>% 
        mutate(parameter=as_factor(parameter))
      #----#
      tagList(
        renderPlot({
          tbResult %>%
            mutate(GOF=cut(GOF,breaks = 10) %>% as_factor) %>% 
            ggplot(aes(x=value,fill=GOF)) +
            geom_histogram(bins = 50L, position = position_stack()) +
            # ggthemes::theme_hc() +
            facet_wrap(vars(parameter), scales = 'free') +
            labs(x="Parameter Value",
                 y="Frequency (Count)",
                 title = "Frequency for each parameter in LHS parameter sets")
         
        }),
        reactable::renderReactable(tbResult %>%
                                     arrange(desc(GOF)) %>%
                                     pivot_wider(names_from=parameter,values_from=value) %>%
                                     reactable(style = 'background-color: #1A0933',
                                               defaultColDef = colDef(format=colFormat(digits = 3))))
      )
    }
  })
  
  session$allowReconnect(TRUE)
}

shinyApp(ui, server)