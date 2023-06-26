# Ontology-powered faceted fairness metric browser

library(shiny)
library(shinyjs)
library(bslib)

source("fmo-interface.R")
# also installs sparql

# note: rdflib for R (requires redland)

# Helper functions 
select_id <- function(...) {
  uri = paste("select",...,sep="_")
  return(gsub(":","_",uri))
}
hover_id <- function(...) {
  uri = paste("hover",...,sep="_")
  return(gsub(":","_",uri))
}
select_label_id <- function(...) {
  uri = paste(select_id(...),"-label",sep="")
  return(uri)
}

# customized extension of the checkboxGroupInput widget

extCheckboxGroupInput <- function(...) {
  
  # temporarily disable htmlEscape
  saved.htmlEscape <- htmltools::htmlEscape
  assignInNamespace("htmlEscape", function(x, attribute) return(x), "htmltools")
  
  cbg <- checkboxGroupInput(...)
  
  ## restore htmlEscape function
  assignInNamespace("htmlEscape", saved.htmlEscape, "htmltools")
  
  nChoices <- length(cbg$children[[2]]$children[[1]])
  lapply(1:min(nChoices),function(i){
    choice <- cbg$children[[2]]$children[[1]][[i]]
    choice_uri <- choice$children[[1]]$children[[1]]$attribs$value
    cbg$children[[2]]$children[[1]][[i]][["attribs"]][["id"]] <<- hover_id(choice_uri)
    #print(hover_id(choice_uri))
  })
  #browser()
  cbg
}
## App

all_cat <- get_categories()
# removing individual fairness as a high-level notion:
# inelegant temporary hack 
all_cat <- subset(all_cat, category_uri!="fmo:individual-level_fairness_notion")

categorizations <- unique(na.omit(all_cat$categorization_uri))

all_subcat <- get_subcategories()
subcategorizations <- unique(na.omit(all_subcat$categorization_uri))

named_notions <- get_named_notions(NULL)
named_metrics <- get_named_metrics(NULL)

# Define UI for application
ui <- fluidPage(
    tags$head(HTML("<title>FMO Explorer</title>")),
    #theme = bs_theme(version = 4, bootswatch = "minty"),
    useShinyjs(),
  
    # RPI Logo
    img(src='RensselaerLogo_black.png', align = "left", height="75px"),
    # TWC Logo
    img(src='twc_green.png', align = "right", height="75px"),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$style(type='text/css',"
                .subcat {margin-left: 20px})
    "), 
    # Application title
    titlePanel(h1("Fairness Metric Explorer",align="center")),
    HTML("<p style='text-align: center;'><b>Powered by the <a href='https://github.com/frankj-rpi/fairness-metrics-ontology' target='_blank'>Fairness Metrics Ontology</a></b></p>"),
    hr(),
    fluidRow(
      
      column(4,
         wellPanel(
           # div(class = 'subcat',id="test",
           #     extCheckboxGroupInput("inCh_ckboxGroup", "Input checkbox<i>ital</i>",
           #                    c("Item A", "<i>Item</i> B", "Item C"))
           # ),
           #h3('Filter by:',style="margin-top:0"),
           uiOutput('category_view_title'),
           uiOutput('category_view'),
         )
      ),
      column(3,
             wellPanel(
               h3('List:',style="margin-top:0"),
               tabsetPanel(type = "tabs", id="search_type",
                           tabPanel("Fairness Notions",selectInput('select_notion', 
                                                       '',
                                                       multiple=FALSE,
                                                       selectize = FALSE,
                                                       choices = named_notions,
                                                       selected = NULL,
                                                       size = 20)),
                           tabPanel("Fairness Metrics",selectInput('select_metric', 
                                                       '',
                                                       multiple=FALSE,
                                                       selectize = FALSE,
                                                       choices = named_metrics,
                                                       selected = NULL,
                                                       size = 20)),
               )
             )
      ),
      column(5,
        wellPanel(
            #h3('Class View:',style="margin-top:0"),
            uiOutput('class_view') 
        )
      )
    ),
    #hr(),
    HTML("<footer style='text-align: center;'>
                                             <!--h4 style = 'text-align: left'>ABOUT</h4-->
                                         <div style='margin-left: 1%'>
                                         <p><b>See our most recently published paper on the Fairness Metrics Ontology: <a href=https://dl.acm.org/doi/10.1145/3514094.3534137 target='_blank'>An Ontology for Fairness Metrics</a></b></p> 
                                         </div>
                                     <h1 style = 'color:#990000; font-size: 1.5em;'>
                                     <a href='https://idea.rpi.edu/' target='_blank' style = 'color: #990000; background-color: #f7f7f7;'>About RPI-IDEA</a>
                                     |
                                     <a href='https://tw.rpi.edu/about' target='_blank' style = 'color: #990000; background-color: #f7f7f7;'>About TWC</a>
                                     |
                                     <a href='https://info.rpi.edu/web-privacy-statement' target='_blank' style = 'color: #990000; background-color: #f7f7f7;'>Privacy Policy</a>
                                     |
                                     <a href='https://github.com/tetherless-world/fairness' target='_blank' style = 'color: #990000; background-color: #f7f7f7;'>Explorer GitHub</a>
                                     |
                                     <a href='https://github.com/frankj-rpi/fairness-metrics-ontology' target='_blank' style = 'color: #990000; background-color: #f7f7f7;'>Ontology GitHub</a>
                                     </h1>
                                     </footer>")
)

# Define server logic
server <- function(session, input, output) {
   
    # the last selected class
    #selected_class = reactiveVal(get_class_info("fmo:fairness_notion"))
    
    selected_class <- reactive({
      if(input$search_type == "Fairness Notions" && !is.null(input$select_notion)){
        return(get_class_info(input$select_notion))
      }
      if(input$search_type == "Fairness Metrics" && !is.null(input$select_metric)){
        return(get_class_info(input$select_metric))
      }
    })
    
    # the currently hovered class
    hovered_class = reactiveVal(NULL)

    # the active category filters 
    get_filtered_cat <- function(){
      res = lapply(categorizations, function(cat) {
        choices <- input[[select_id(cat)]]
        queries = unique(all_cat[all_cat$category_uri %in% choices,"query_notion"])
        return(queries)
      })
      res2 = lapply(subcategorizations, function(subcat) {
        res = lapply(categorizations, function(cat) {
          subcat_choices <- input[[select_id(cat,subcat)]]
          queries = unique(all_subcat[all_subcat$category_uri %in% subcat_choices,"query_notion"])
          return(queries)
        })
        return(unlist(res))
      })
      return (c(res,res2))
    }
    
    # populate both notion and metric views
    update_list_view <- function(){
      filtered_cat = get_filtered_cat()
      named_notions <- get_named_notions(filtered_cat)
      updateSelectInput(session, "select_notion",
                        #label = "Fairness notions for selected categories",
                        selected = NULL,
                        choices = named_notions)
      named_metrics <- get_named_metrics(filtered_cat)
      updateSelectInput(session, "select_metric",
                        #label = "Fairness metrics for selected categories",
                        selected = NULL,
                        choices = named_metrics)
    }
    
    update_subcategory_view <- function(cat){
      # determine which subcategories need to be displayed
      category_uri <- input[[select_id(cat)]]
      subcats <- all_subcat[all_subcat$supercategorization_uri==cat & all_subcat$supercategory_uri %in% category_uri,]
      lapply(subcategorizations,function(subcat){
        if(subcat %in% subcats$categorization_uri){
          # get uris
          subcat_choices = unique(subcats$category_uri)
          names(subcat_choices) = unique(subcats$category_label)
          # determine what should stay selected
          selected_choices = intersect(subcat_choices,input[[select_id(cat,subcat)]])
          # update
          updateCheckboxGroupInput(session, select_id(cat,subcat),
                            selected = selected_choices,
                            choices=subcat_choices)
          #browser()
          showElement(select_id(cat,subcat),anim = TRUE,time = 0.5,animType = "slide")
        }
        else{
          updateCheckboxGroupInput(session, select_id(cat,subcat),selected=character(0))
          hideElement(select_id(cat,subcat),anim = TRUE,time = 0.5,animType = "slide")
        }
      })
      
    }
    
    # whenever a category is selected:
    # 1) update the list view
    # 2) update the subcategory views
    lapply(categorizations, function(cat) {
      observeEvent(input[[select_id(cat)]], {
        #shinyjs::hideElement(id=paste('select_',cat),anim = TRUE,animType = "slide",time = 0.5)
        #print(paste('select_',cat))
        #print(input[[paste('select_',cat)]])
        update_subcategory_view(cat)
        update_list_view()
      }, ignoreNULL = FALSE)
      
      # doesn't work bc its for only one thing
      #onevent(event="mouseenter", id=hover_id(cat), print(cat))
      
      # whenever a subcategory is selected:
      lapply(subcategorizations, function(subcat) {
        observeEvent(input[[select_id(cat,subcat)]], {
          update_list_view()
        }, ignoreNULL = FALSE)
      })
      
      # whenever a categorization is hovered 
      onevent(event="mouseenter", id=select_label_id(cat), hovered_class(get_class_info(cat)))
      onevent(event="mouseleave", id=select_label_id(cat), hovered_class(NULL))
      
      # whenever a category is hovered 
      cats <- unique(subset(all_cat,categorization_uri==cat)$category_uri)
      lapply (cats, function(category_uri){
        onevent(event="mouseenter", id=hover_id(category_uri), hovered_class(get_class_info(category_uri)))
        onevent(event="mouseleave", id=hover_id(category_uri), hovered_class(NULL))
      })
      
      
    })

    # onevent(event="mouseenter", id="hover_Item A", print("aaa"))
    # onevent(event="mouseenter", id="hover_1", print("cat"))
    # onevent(event="mouseenter", id="hover_fmo_Classification_Problem", print("cata"))
    
    # whenever a notion is selected:
    # observeEvent(input$select_notion, {
    #   selected_class(get_class_info(input$select_notion))
    # })
    # 
    # # whenever a metric is selected:
    # observeEvent(input$select_metric, {
    #   selected_class(get_class_info(input$select_metric,type="metric"))
    # })
    
    # populate category view
    # 1) for each categorization, get the subset of applicable categories
    # 2) make top-level checkboxgroupinputs for each categorization
    output$category_view = renderUI({
      lapply(categorizations, function(cat) {
        cats <- subset(all_cat,categorization_uri==cat)
        cat_label = unique(all_cat[all_cat$categorization_uri==cat,"categorization_header"])
        subcategorization_checkboxes <- lapply(subcategorizations,function(subcat){
          subcat_label = unique(all_subcat[all_subcat$categorization_uri==subcat,"categorization_header"])
          subcat_choices = unique(all_subcat$category_uri)
          names(subcat_choices) = unique(all_subcat$category_label)
          div(class="subcat",
              hidden(extCheckboxGroupInput(select_id(cat,subcat), 
                             subcat_label,
                             choices=subcat_choices))
          )
        })
        tagList(
          extCheckboxGroupInput(select_id(cat), 
                             cat_label,
                             choiceNames = cats$category_header,
                             choiceValues = cats$category_uri),
          subcategorization_checkboxes

        )
      })
    })
    
    output$category_view_title = renderUI({
      h3(p('Search for',tolower(input$search_type),'that:'),style="margin-top:0")
    })
    
    output$class_view = renderUI({
      
      class_to_show <- selected_class()
      if(!is.null(hovered_class())){
        class_to_show <- hovered_class()
      }
      if(is.null(class_to_show)){
        return(tagList(
          h3('Concept View',style="margin-top:0"),
          strong("Select a concept from the list view, or hover over a concept from the category view on the left, to see details here about the concept.")
        ))
      }
      pdef = div()
      pdef_res = clean_col(class_to_show$probabilistic_definition)
      if(pdef_res!=""){
        pdef = tagList(
          strong("Probabilistic Definition:"),span(pdef_res),
          br()
        )
      }
      mdef = div()
      if(clean_col(class_to_show$mathematical_definition)!=""){
        mdef = tagList(
          strong("Mathematical Definition:"),withMathJax(helpText(clean_col(class_to_show$mathematical_definition))),
          #strong("Mathematical Definition:"),withMathJax(helpText("The ranking equivalent of Statistical Parity, this notion requires that \\(P[f(X) > f(X') | (X,Y)\\in G_i,(X',Y')\\in G_j]=\\kappa\\) for some \\(\\kappa\\in[0,1]\\) for all \\(i\\neq j\\). ")),
          #strong("Mathematical Definition:"),withMathJax(helpText("$$\\min{balance(C_i)} \\forall C_i \\in C$$")),
          br()
        )
      }
      measures_notion = div()
      if(clean_col(class_to_show$notion_label)!=""){
        measures_notion = tagList(
          strong("Measures:"),span(str_to_title(clean_col(class_to_show$notion_label))),br()
        )
      } else if("fmo:fairness_metric" %in% class_to_show$superclass_uri){
        measures_notion = tagList(
          strong("Measurement of any fairness notion"),br(),
        )
      }
      measured_by_metric = div()
      metric_res = clean_col(class_to_show$metric_label,single=FALSE)
      if(length(metric_res)>0){
        metric_taglist = lapply(metric_res, function(metric_label){
          tagList(span(paste("-",str_to_title(metric_label),sep="  ")),br())
        })
        measured_by_metric = tagList(
          strong("Measured specifically by:"),br(),
          metric_taglist,
          br()
        )
      }else if("fmo:fairness_notion" %in% class_to_show$superclass_uri){
        measured_by_metric = tagList(
          strong("Measured by any generic fairness metric"),br(),
          br()
        )
      }
      
      tagList(
        h3(str_to_title(clean_col(class_to_show$label)),style="margin-top:0"),
        h4(str_to_title(clean_col(class_to_show$superclass_label))),
        measures_notion,
        strong("Description:"),span(clean_col(class_to_show$definition)),br(),
        pdef,
        mdef,
        br(),
        measured_by_metric,
        strong("Source:"),a(href=clean_col(class_to_show$source),clean_col(class_to_show$source))
      )
    })

    # # stop app after session ends
    # session$onSessionEnded(function() {
    #   stopApp()
    # })
    
    # Content of modal dialog
  query_modal <- modalDialog(
    title = "Welcome to the Fairness Metric Explorer",
    "PLEASE NOTE: This application is the result of the efforts of students 
    and staff of the Rensselaer IDEA and the Tetherless World Constellation at RPI.
    This is a prototype application and may not meet all of the standards one might 
    expect of a production commercial product.",
    easyClose = F,
    footer = tagList(actionButton("run", "Continue with the Fairness Metric Explorer"))
  )
  
  # Creates modal dialog
  showModal(query_modal)
  
  # Removes modal
  observeEvent(input$run, {removeModal()})
}

# Run the application 
shinyApp(ui = ui, server = server)
