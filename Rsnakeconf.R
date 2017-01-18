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
source("src/Rsnakeconf_functions.R")
#GLOBAL PARAMETERS
##Load configuration file for app
conf <- Rsnakeconf.jsonConf();

#ShinyUI

ui <- miniPage(tags$head(    
    tags$style("label {font-weight:bold;display: inline;}")
),theme = shinytheme("flatly"),
gadgetTitleBar("Snakemake configuration file",
               right = miniTitleBarButton("done", "Create", primary = TRUE)
),
miniTabstripPanel(
    miniTabPanel("Parameters", icon = icon("sliders"),
                 miniContentPanel(
                     #Load from file
                     fluidRow(
                         column(6,offset=3,
                                wellPanel(
                                    div(class="form-group shiny-input-container shiny-input-container-inline",
                                        div(class="col-sm-2",style="padding-top: 10px;",
                                            tags$label(class="pull-right","Load from file :",`for` ="file")
                                        ),
                                        div(class="col-sm-10 shiny-options-group",
                                            fileInput("file", label = "",width = "100%")
                                        )
                                    ),tags$hr()
                                )
                         )
                     ),
                     #Dynamical UI Output
                     lapply(1:length(conf), function(i) {
                         fluidRow(
                             column(6,offset=3,uiOutput(names(conf)[i]))
                         )
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
                         column(6,offset=3,uiOutput(paste0("overview_",names(conf)[i])))
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
            wellPanel(
                div(style="text-align:center;",h3(names(conf)[i])),
                lapply(1:length(conf[[i]]),function(j){
                    #Get values
                    type <- conf[[i]][[j]][["type"]]
                    id <- names(conf[[i]])[j]
                    label <- paste(names(conf[[i]])[j],":")
                    default_value <- conf[[i]][[j]][["value"]]
                    value <- Rsnakeconf.getValueFromFile(jsonfile=jsonData(),type = type,id=id,default_value = default_value)
                    
                    #Compute output
                    if(type == "textArea"){
                        div(class="form-horizontal",
                            Rsnakeconf.inputType(type = "radioButtons",id = paste0("sep_",id),label = "Separator :",vals = c("\\n",",",";")),
                            Rsnakeconf.inputType(type = type,id = id,label = label,vals = value)
                        )
                    }else {
                        div(class="form-horizontal",
                            Rsnakeconf.inputType(type = type,id = id,label = label,vals = value)
                        )
                    }
                })
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
                input_data <- input[[input_name]];
                
                meta_data <- conf[[i]][[j]]
                if(meta_data[["type"]] == "textArea"){
                    sep <- switch(input[[paste0("sep_",input_name)]],
                        "\\n" = "\n",
                        "," = ",",
                        ";" = ";"
                    )
                    
                    sentence <- paste0("\t\"",input_name,"\" : [\"",gsub(sep,"\",\"",input_data),"\"]");
                }else {
                    if(is.logical(input_data) || is.numeric(input_data)){
                        sentence <- paste0("\t\"",input_name,"\" : ",tolower(as.character(input_data)));
                    }else {
                        sentence <- paste0("\t\"",input_name,"\" : \"",input_data,"\"");
                    }
                    
                }
                
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
    
    #Generate df for overview
    overview.df <- eventReactive(input$update,{
        df.total <- list();
        #For each global opt
        for(i in 1:length(conf)){
            #for each sub parameters
            #create sub data.frame
            df.sub <- data.frame("Option"=NULL,"Value"=NULL,"Status"=NULL,"Color"=NULL);
            for(j in 1:length(conf[[i]])){
                #get the parameter name
                input_name <- names(conf[[i]])[j];
                #get the user input
                input_data <- input[[input_name]];
                #get the meta information for given parameter
                meta_data <- conf[[i]][[j]]
                #variable for storing temporary values
                ij.status <- ij.color <- NULL;
                #Conditions
                ##DIRECTORY
                if(meta_data[["desc"]] == "directory"){
                    
                    if(input_data == ""){
                        ij.status <- "root";
                        ij.color <- "warning";
                    }else{
                        if(dir.exists(input_data)){
                            ij.status <- "OK";
                            ij.color <- "success";
                        }else{
                            ij.status <- "NO DIRECTORY";
                            ij.color <- "danger";
                        }
                    }
                    df.sub <- rbind(df.sub,data.frame("Option"=input_name,"Value"=input_data,"Status"=ij.status,"Color"=ij.color))
                }else if(meta_data[["desc"]] == "file"){ ##FILE(S)
                    #Test if input_data is empty
                    if(input_data == ""){
                        df.sub <- rbind(df.sub,data.frame("Option"=input_name,"Value"="","Status"="EMPTY","Color"="warning"))
                        next;
                    }
                    #check the extension and directory parameters in parent
                    ext <- ""
                    dir <- ""
                    for(t in 1:length(conf[[i]])){
                        sub_meta_data <- conf[[i]][[t]]
                        sub_input_name <- names(conf[[i]])[t];
                        if(sub_meta_data[["desc"]] == "extension"){
                            ext <- input[[sub_input_name]];
                        }else if(sub_meta_data[["desc"]] == "directory"){
                            dir <- input[[sub_input_name]];
                        }
                    }
                    if(meta_data[["type"]] == "textArea"){
                        #One line per file
                        sep <- switch(input[[paste0("sep_",input_name)]],
                                      "\\n" = "\n",
                                      "," = ",",
                                      ";" = ";"
                        )
                        
                        for(f in unlist(strsplit(input_data,sep))){
                            if(file.exists(paste0(dir,"/",f,ext))){
                                ij.status <- "OK";
                                ij.color <- "success";
                            }else{
                                ij.status <- "NO FILE";
                                ij.color <- "danger";
                            }
                            df.sub <- rbind(df.sub,data.frame("Option"=input_name,"Value"=f,"Status"=ij.status,"Color"=ij.color))
                        }
                    }else { #Just one file, no loop
                        if(file.exists(paste0(dir,"/",input_data,ext))){
                            ij.status <- "OK";
                            ij.color <- "success";
                        }else{
                            ij.status <- "NO FILE";
                            ij.color <- "danger";
                        }
                        df.sub <- rbind(df.sub,data.frame("Option"=input_name,"Value"=input_name,"Status"=ij.status,"Color"=ij.color))
                    }
                    
                    
                }else {
                    ij.color <- ifelse(input_data %in% c(NA,""),"danger","success")
                    ij.status <- ifelse(input_data %in% c(NA,""),"NO VALUE","OK")
                    df.sub <- rbind(df.sub,data.frame("Option"=input_name,"Value"=as.character(input_data),"Status"=ij.status,"Color"=ij.color))
                    
                    
                }
            }
            df.total[[names(conf)[i]]] <- df.sub
        }
        return(df.total)
    })
    
    #Overview
    lapply(1:length(conf), function(i) {
        output[[paste0("overview_",names(conf)[i])]] <- renderUI({
            if(is.null(overview.df()))
                return(NULL);
            #Compute
            sub.df <- overview.df()[[names(conf)[i]]];
            format.cells <- NULL
            for(j in 1:nrow(sub.df)){
                format.cells <- paste(format.cells,"<tr>",tags$td(sub.df[j,1]),tags$td(sub.df[j,2]),tags$td(class=sub.df[j,4],sub.df[j,3]),"</tr>")
            }
            if(is.null(format.cells))
                return(NULL)
                
            #Show
            wellPanel(
                div(style="text-align:center;",h5(strong(names(conf)[i]))),
                tags$table(class="table table-striped table-hover table-bordered table-condensed",
                           tags$thead(
                               tags$tr(
                                   tags$td("Options"),
                                   tags$td("Value"),
                                   tags$td("Status")
                               )
                           ),
                           tags$tbody(
                               HTML(format.cells)
                           )
                )    
            )      
        })
    })
}
#Run miniUI App
runGadget(ui, server,viewer = browserViewer())
