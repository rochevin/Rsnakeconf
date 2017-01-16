#' Create a configuration file for snakemake
#'
#' @details
#' Uses an Shiny miniUI app to create and visualize any type of json file.
#' Use a json file to create forms locate at src/Rsnakeconf.json
#' @return
#' A json format type file using shiny form as values.
#'

#Library
library(jsonlite)
library(shiny)
library(miniUI)
library(shinythemes)
library(shinyAce)
#Functions

##Configuration for app
jsonConf <- function(confdir = "src/Rsnakeconf.json"){
    if (!file.exists(confdir))
        return(NULL)
    jsonfile <- fromJSON(confdir, flatten=TRUE)
    
    return(jsonfile)
}


##Choice of input Type and values
inputType <- function(type,label,id,vals = NULL){
    
    res <- switch(type,
                  "text" = div(class="form-group shiny-input-container",
                               tags$label(class="control-label col-sm-2",label,`for` =id),  
                               div(class="col-sm-10",
                                   tags$input(id=id,class="form-control",type=type,value=vals[1])
                               )
                  ),
                  "number" =  div(class="form-group shiny-input-container",
                                   tags$label(class="control-label col-sm-2",label,`for` =id),  
                                   div(class="col-sm-10",
                                       tags$input(id=id,class="form-control",type=type,value=vals[1],`min` =1)
                                   )
                  ),
                  "checkbox" = checkboxInput(id, label,
                                             value = TRUE),
                  "checkboxGroup" = checkboxGroupInput(id, label,
                                                       choices = vals,
                                                       selected = vals[1]
                  ),
                  "radioButtons" = radioButtons(id, label,
                                                choices = vals,
                                                selected = vals[1]
                  ),
                  "selectInput" = div(class="form-group shiny-input-container",
                                      tags$label(class="control-label col-sm-2",label,`for` =id), 
                                      div(class="col-sm-10",
                                          tags$select(id=id,`size` =length(vals),
                                                      tags$option(selected=T,value=vals[1],vals[1]),
                                                      lapply(2:length(vals),function(i){tags$option(value=vals[i],vals[i])})
                                          ),
                                          tags$script(type="application/json",`data-for` =id,`data-nonempty`="","{}")
                                      )
                  ),
                  "selectInput.multi" = div(class="form-group shiny-input-container",
                                            tags$label(class="control-label col-sm-2",label,`for` =id), 
                                            div(class="col-sm-10",
                                                tags$select(multiple="multiple",id=id,`size` =length(vals),
                                                            tags$option(selected=T,value=vals[1],vals[1]),
                                                            lapply(2:length(vals),function(i){tags$option(value=vals[i],vals[i])})
                                                ),
                                                tags$script(type="application/json",`data-for` =id,"{}")
                                            )
                  ),
                  "textArea" = div(class="form-group shiny-input-container",
                                   tags$label(class="control-label col-sm-2",label,`for` =id),  
                                   div(class="col-sm-10",
                                       tags$textarea(id=id,class="form-control",vals)
                                   )
                  )
    )
    
    return(res)
}
#Load value from file or return default value
getValueFromFile <- function(jsonfile=NULL,type = type,id,default_value=""){
    if (is.null(jsonfile))
        return(default_value);
    
    if(is.null(jsonfile[[id]]))
        return(default_value);
    
    if(type == "textArea"){
        return(paste(jsonfile[[id]],collapse="\n"));
    }else if(type == "selectInput"){
        if(jsonfile[[id]] %in% default_value){
            return(c(jsonfile[[id]],default_value[-which(default_value == jsonfile[[id]])]));
        }else{
            return(default_value);
        }
    }else{
        return(jsonfile[[id]]);
    }
    
    
}


#Load conf
conf <- jsonConf();

#ShinyUI

ui <- miniPage(tags$head(    
    tags$style("label {font-weight: bold;}")
),theme = shinytheme("flatly"),
gadgetTitleBar("Snakemake configuration file",
               right = miniTitleBarButton("done", "Accept", primary = TRUE)
),
miniTabstripPanel(
    miniTabPanel("Parameters", icon = icon("sliders"),
                 miniContentPanel(
                     #Load from file
                     column(10,offset=1,
                        wellPanel(
                            div(style="display: inline-block;",
                                tags$label("Load from file :"),
                                fileInput("file", label = "")
                            )
                        )
                     ),
                     #Dynamical UI Output
                     lapply(1:length(conf), function(i) {
                         uiOutput(names(conf)[i])
                     })
                 )
    ),
    miniTabPanel("Visualize", icon = icon("file-code-o"),
                 miniContentPanel(
                     aceEditor("code",mode="json",theme="terminal",readOnly = TRUE,height="100%")
                 )
    ),
    miniTabPanel("Overview", icon = icon("eye"),
                 miniContentPanel(
                     lapply(1:length(conf), function(i) {
                         uiOutput(paste0("overview_",names(conf)[i]))
                     })
                 )
    )
),
miniButtonBlock(
    actionButton("update", "Update")
)

)


#Shinyserver
server <- function(input, output, session) {
    #TITLE BAR
    # Handle the Done button being pressed.
    observeEvent(input$done, {
        stopApp(cat(ConfigOutput()))
    })
    #PARAMETERS
    ##LOAD
    jsonData <- reactive({
        inFile <- input$file
        
        if (is.null(inFile))
            return(NULL)
        jsonfile <- fromJSON(inFile$datapath, flatten=TRUE)
        
        return(jsonfile)
    })
    #Dynamical UI output
    lapply(1:length(conf), function(i) {
        output[[names(conf)[i]]] <- renderUI({
            column(10,offset=1,
                    wellPanel(
                        div(style="text-align:center;",h3(names(conf)[i])),
                        lapply(1:length(conf[[i]]),function(j){
                            #Get values
                            type <- names(conf[[i]][[j]])
                            id <- names(conf[[i]])[j]
                            label <- paste(names(conf[[i]])[j],":")
                            default_value <- conf[[i]][[j]][[type]]
                            value <- getValueFromFile(jsonfile=jsonData(),type = type,id=id,default_value = default_value)
                            
                            #Compute output
                            div(class="form-horizontal",
                                    inputType(type = type,id = id,label = label,vals = value)
                            )
                        })
                    )        
            )
        })
    })
    # VISUALIZE
    #return conf text
    ConfigOutput <- reactive({
        out <- NULL;
        for(i in 1:length(conf)){
            for(j in 1:length(conf[[i]])){
                input_name <- names(conf[[i]])[j];
                input_data <- input[[names(conf[[i]])[j]]];
                
                sentence <- paste0("\t\"",input_name,"\" : \"",gsub("\n",",",input_data),"\"");
                if(is.null(out)){
                    out <- sentence;
                }else {
                    out <- paste(out,sentence,sep=",\n");    
                }
            }
        }
        out <- paste("{\n",out,"\n}");
        return(out)
    })
    #Update Ace Editor
    observeEvent(input$update, {
        updateAceEditor(session, "code", value=ConfigOutput())
    })
    
    #Overview
    lapply(1:length(conf), function(i) {
        output[[paste0("overview_",names(conf)[i])]] <- renderUI({
            column(4,offset=4,
                   div(style="text-align:center;",h5(strong(names(conf)[i]))),
                   tags$table(class="table table-striped table-hover table-bordered",
                       tags$thead(
                           tags$tr(
                               tags$td("Options"),
                               tags$td("Status")
                           )
                       ),
                       tags$tbody(
                           lapply(1:length(conf[[i]]),function(j){
                               #Get values
                               input_name <- names(conf[[i]])[j];
                               input_data <- input[[names(conf[[i]])[j]]];
                               #Compute output
                               tags$tr(
                                   tags$td(input_name),
                                   if(cc == cc){
                                       
                                   }
                               )
                           })
                       )
                   )        
            )
        })
    })
    
}

#Run miniUI App
runGadget(ui, server,viewer = browserViewer())
