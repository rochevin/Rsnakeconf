#' Create a configuration file for snakemake
#'
#' @details
#' Uses an Shiny miniUI app to create and visualize any type of json file.
#' Use a json file to create forms locate at src/Rsnakeconf.json
#' @return
#' A json format type file using shiny form as values.
#'

#Library
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
                  "text" = textInput(id, label,
                                     value = vals[1]),
                  "numeric" =  numericInput(id, label,
                                            value = vals[1],min=1),
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
                  "selectInput" = selectInput(id, label,
                                              choices = vals,
                                              selected = vals[1]
                  ),
                  "selectInput.multi" = selectInput(id, label,
                                                    choices = vals,
                                                    selected = vals[1],
                                                    multiple = TRUE
                  ),
                  "textArea" = textAreaInput(id, label, value = vals)
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
                     wellPanel(fileInput("file", label = "Load from file :")),
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
    miniTabPanel("Summarize", icon = icon("check"),
                 miniContentPanel(
                     
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
            wellPanel(
                div(style="text-align:center;",h3(names(conf)[i])),
                lapply(1:length(conf[[i]]),function(j){
                    type <- names(conf[[i]][[j]])
                    id <- names(conf[[i]])[j]
                    label <- paste(names(conf[[i]])[j],":")
                    default_value <- conf[[i]][[j]][[type]]
                    value <- getValueFromFile(jsonfile=jsonData(),type = type,id=id,default_value = default_value)
                    inputType(type = type,id = id,label = label,vals = value)
                })
            )
        })
    })
    
    # VISUALIZE
    #Parse sample names
    sampleName <- reactive({
        samples <- input$sample_name
        if(length(samples) == 0){return("")}
        samples <- paste(unlist(strsplit(samples,"\n")),collapse="\", \"")
        return(samples)
    })

    #return conf text
    ConfigOutput <- reactive({
        #GENOME
        gen_dir <- paste0("\t\"gen_dir\" : \"",input$gen_dir,"\"")
        gen_name <- paste0("\t\"gen_name\" : \"",input$gen_name,"\"")
        gen_ext <- paste0("\t\"gen_ext\" : \"",input$gen_ext,"\"")
        #SAMPLES
        sample_dir <- paste0("\t\"sample_dir\" : \"",input$sample_dir,"\"")
        sample_name <- paste0("\t\"sample_name\" : [\"",sampleName(),"\"]")
        sample_ext <- paste0("\t\"sample_ext\" : \"",input$sample_ext,"\"")
        #Alignment
        aln_threads <- paste0("\t\"aln_threads\" : \"",input$aln_threads,"\"")
        
        out <- paste(
            gen_dir,
            gen_name,
            gen_ext,
            sample_dir,
            sample_name,
            sample_ext,
            aln_threads
            ,sep = ",\n")
        
        
        
        return(paste("{\n",out,"\n}"))
    })
    #Update Ace Editor
    observeEvent(input$update, {
        updateAceEditor(session, "code", value=ConfigOutput())
    })
    
}

#Run miniUI App
runGadget(ui, server,viewer = browserViewer())
