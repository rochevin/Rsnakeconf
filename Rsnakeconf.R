#' Create a configuration file for snakemake
#'
#' @details
#' Uses an Shiny miniUI app to create and visualize any type of json file.
#' Use a json file to create forms locate at src/Rsnakeconf.json
#' @return
#' A json format type file using shiny form as values.
#'

#Library & functions
library(jsonlite)
library(shiny)
library(miniUI)
library(shinythemes)
library(shinyAce)
source("src/Rsnakeconf_functions.R")
#GLOBAL PARAMETERS
##Load configuration file for app
Rsnakeconf.conf.file <- "src/Rsnakeconf.json";
Rsnakeconf.conf.data <- Rsnakeconf.jsonConf();
##confs keys
Rsnakeconf.conf.keys <- list(
    "type" = "input_type",
    "desc" = "output_type",
    "value" = "value",
    "comment" = "description"
)
#Specific background color
#Rsnakeconf.conf.colors <- rep(c("#EE4000", "#4F94CD", "#43CD80"),length(Rsnakeconf.conf.data))[1:length(Rsnakeconf.conf.data)]

#ShinyUI
ui <- miniPage(tags$head(    
    tags$style("label {font-weight:bold;display: inline;}"),
    tags$script("$(document).ready(function(){$('[data-toggle=\"tooltip\"]').tooltip();});")
),theme = shinytheme("flatly"),
#TOP BUTTONS
gadgetTitleBar("Snakemake configuration file",
               right = miniTitleBarButton("done", "Create", primary = TRUE)
),
#PANNELS
miniTabstripPanel(
    #PARAMETERS
    miniTabPanel("Parameters", icon = icon("sliders"),
                 miniContentPanel(
                     #File panel
                     fluidRow(
                         column(6,offset=3,
                                wellPanel(
                                    fluidRow(
                                        div(class="form-group shiny-input-container shiny-input-container-inline",
                                            div(class="col-sm-2",style="padding-top: 10px;",
                                                tags$label(class="pull-right","Load from file :",`for` ="file")
                                            ),
                                            div(class="col-sm-10 shiny-options-group",
                                                fileInput("file", label = "",width = "100%") #Load from file
                                            )
                                        )
                                    ),
                                    fluidRow(Rsnakeconf.checkboxInput(label="Keep section names as first dimension",id="keep_section",vals="checked"))
                                    
                                    
                                )
                         )
                     ),
                     #Dynamical UI Output
                     lapply(1:length(Rsnakeconf.conf.data), function(i) {
                         fluidRow(
                             column(6,offset=3,uiOutput(names(Rsnakeconf.conf.data)[i]))
                         )
                     })
                 )
    ),
    #VISUALIZE
    miniTabPanel("Visualize", icon = icon("file-code-o"),
                 miniContentPanel(
                     aceEditor("code",mode="json",theme="terminal",readOnly = TRUE,height="100%")
                 )
    ),
    #OVERVIEW
    miniTabPanel("Overview", icon = icon("eye"),
                 miniContentPanel(
                     lapply(1:length(Rsnakeconf.conf.data), function(i) {
                         column(6,offset=3,uiOutput(paste0("overview_",names(Rsnakeconf.conf.data)[i])))
                     })
                 )
    )
),
#BUTTON UPDATE
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
    #Dynamical UI output (PARAMETERS UI)
    lapply(1:length(Rsnakeconf.conf.data), function(i) {
        output[[names(Rsnakeconf.conf.data)[i]]] <- renderUI({
            fluidRow(
                div(style="text-align:center;",h3(names(Rsnakeconf.conf.data)[i])),
                #wellPanel(style=paste0("background-color: ",Rsnakeconf.conf.colors[i],";color:white;"),
                wellPanel( 
                         lapply(1:length(Rsnakeconf.conf.data[[i]]),function(j){
                              #Get values
                              type <- Rsnakeconf.conf.data[[i]][[j]][[Rsnakeconf.conf.keys[["type"]]]]
                              id <- names(Rsnakeconf.conf.data[[i]])[j]
                              if(Rsnakeconf.conf.keys[["comment"]] %in% names(Rsnakeconf.conf.data[[i]][[j]])){
                                  label <- Rsnakeconf.addtooltip(id=id,
                                                                 label = names(Rsnakeconf.conf.data[[i]])[j],
                                                                 comment = Rsnakeconf.conf.data[[i]][[j]][[Rsnakeconf.conf.keys[["comment"]]]]
                                  )
                              }else {
                                  label <- paste(names(Rsnakeconf.conf.data[[i]])[j],":")
                              }
                              
                              default_value <- Rsnakeconf.conf.data[[i]][[j]][[Rsnakeconf.conf.keys[["value"]]]]
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
            )      
        })
    })
    # Generate json format with user values (VISUALIZE and Create button)
    #return conf text
    ConfigOutput <- reactive({
        outs <- list()
        for(i in 1:length(Rsnakeconf.conf.data)){
            out <- NULL
            deb_out <- paste0("\t\"",names(Rsnakeconf.conf.data)[i],"\" : {\n");
            tabsep <- ifelse(input$keep_section,"\t\t\"","\t\"")
            for(j in 1:length(Rsnakeconf.conf.data[[i]])){
                input_name <- names(Rsnakeconf.conf.data[[i]])[j];
                input_data <- input[[input_name]];
                
                meta_data <- Rsnakeconf.conf.data[[i]][[j]]
                if(meta_data[[Rsnakeconf.conf.keys[["type"]]]] == "textArea"){
                    sep <- switch(input[[paste0("sep_",input_name)]],
                        "\\n" = "\n",
                        "," = ",",
                        ";" = ";"
                    )
                    
                    sentence <- paste0(tabsep,input_name,"\" : [\"",gsub(sep,"\",\"",input_data),"\"]");
                }else {
                    if(is.logical(input_data) || is.numeric(input_data)){
                        sentence <- paste0(tabsep,input_name,"\" : ",tolower(as.character(input_data)));
                    }else {
                        sentence <- paste0(tabsep,input_name,"\" : \"",input_data,"\"");
                    }
                    
                }
                
                if(is.null(out)){
                    out <- sentence;
                }else {
                    out <- paste(out,sentence,sep=",\n");    
                }
            }
            if(input$keep_section){
                outs[[i]] <- paste0(deb_out,out,"\n\t}");    
            }else{
                outs[[i]] <- out;
            }
            
        }
        out <- paste0("{\n",paste(outs,collapse = ",\n"),"\n}")
        return(out)
    })
    #Update Ace Editor with Update button
    observeEvent(input$update, {
        updateAceEditor(session, "code", value=ConfigOutput())
    })
    
    #Update df for overview with Update button
    overview.df <- eventReactive(input$update,{
        df.total <- list();
        #For each global opt
        for(i in 1:length(Rsnakeconf.conf.data)){
            #for each sub parameters
            #create sub data.frame
            df.sub <- data.frame("Option"=NULL,"Value"=NULL,"Status"=NULL,"Color"=NULL);
            for(j in 1:length(Rsnakeconf.conf.data[[i]])){
                #get the parameter name
                input_name <- names(Rsnakeconf.conf.data[[i]])[j];
                #get the user input
                input_data <- input[[input_name]];
                #get the meta information for given parameter
                meta_data <- Rsnakeconf.conf.data[[i]][[j]]
                #variable for storing temporary values
                ij.status <- ij.color <- NULL;
                #Conditions
                ##DIRECTORY
                if(meta_data[[Rsnakeconf.conf.keys[["desc"]]]] == "directory"){
                    
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
                }else if(meta_data[[Rsnakeconf.conf.keys[["desc"]]]] == "file"){ ##FILE(S)
                    #Test if input_data is empty
                    if(input_data == ""){
                        df.sub <- rbind(df.sub,data.frame("Option"=input_name,"Value"="","Status"="EMPTY","Color"="warning"))
                        next;
                    }
                    #check the extension and directory parameters in parent
                    ext <- ""
                    dir <- ""
                    for(t in 1:length(Rsnakeconf.conf.data[[i]])){
                        sub_meta_data <- Rsnakeconf.conf.data[[i]][[t]]
                        sub_input_name <- names(Rsnakeconf.conf.data[[i]])[t];
                        if(sub_meta_data[[Rsnakeconf.conf.keys[["desc"]]]] == "extension"){
                            ext <- input[[sub_input_name]];
                        }else if(sub_meta_data[[Rsnakeconf.conf.keys[["desc"]]]] == "directory"){
                            dir <- input[[sub_input_name]];
                        }
                    }
                    if(meta_data[[Rsnakeconf.conf.keys[["type"]]]] == "textArea"){
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
                    
                #OTHER TYPE
                }else {
                    ij.color <- ifelse(input_data %in% c(NA,""),"danger","success")
                    ij.status <- ifelse(input_data %in% c(NA,""),"NO VALUE","OK")
                    df.sub <- rbind(df.sub,data.frame("Option"=input_name,"Value"=as.character(input_data),"Status"=ij.status,"Color"=ij.color))
                    
                    
                }
            }
            df.total[[names(Rsnakeconf.conf.data)[i]]] <- df.sub #Add d.f to list
        }
        return(df.total)
    })
    
    #Dynamical UI (OVERVIEW)
    lapply(1:length(Rsnakeconf.conf.data), function(i) {
        output[[paste0("overview_",names(Rsnakeconf.conf.data)[i])]] <- renderUI({
            if(is.null(overview.df()))
                return(NULL);
            #Compute
            sub.df <- overview.df()[[names(Rsnakeconf.conf.data)[i]]];
            format.cells <- NULL
            for(j in 1:nrow(sub.df)){
                format.cells <- paste(format.cells,"<tr>",tags$td(sub.df[j,1]),tags$td(sub.df[j,2]),tags$td(class=sub.df[j,4],sub.df[j,3]),"</tr>")
            }
            if(is.null(format.cells))
                return(NULL)
                
            #Show
            wellPanel(
                div(style="text-align:center;",h5(strong(names(Rsnakeconf.conf.data)[i]))),
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
    
    #Automatically stop a Shiny app when closing the browser tab
    session$onSessionEnded(stopApp)
}
#Run miniUI App
runGadget(ui, server,viewer = browserViewer())
