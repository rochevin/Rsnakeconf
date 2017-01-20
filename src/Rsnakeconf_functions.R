#' Internal fynction used by app to load parameters and generate forms dynamically, stored inside a .json file format
#'
#' @param condfir A character string (default : "src/Rsnakeconf.json")
#' 
#' @return A list of elements for each parameters (see src/Rsnakeconf.json)
#' 
#' 
#' @examples
#' conf <- Rsnakeconf.jsonConf();
#' 
#' conf <- Rsnakeconf.jsonConf(confdir = "src/Rsnakeconf.json");
Rsnakeconf.jsonConf <- function(confdir = "src/Rsnakeconf.json"){
    if (!file.exists(confdir))
        return(NULL)
    jsonfile <- fromJSON(confdir, flatten=TRUE)
    
    return(jsonfile)
}


#' Internal function used by app to select and generate a html form 
#'
#' @param type Specify the form type ( can be text, number, checkbox ... see each separate function for details)
#' @param label An html label to show beside the form
#' @param id The form id
#' @param vals the form values
#' 
#' @return an alternative to HTML shiny forms
#' 
#' 
#' @examples 
#' Rsnakeconf.inputType(type = "text",label = "A text field :", id ="text1",vals = "The value text for this form")
#' 
#' Rsnakeconf.inputType(type = "checkbox",label = "A checked checkbox : :", id ="checbox1",vals = "checked")
Rsnakeconf.inputType <- function(type,label,id,vals = NULL){
    
    res <- switch(type,
                  "text" = Rsnakeconf.textInput(type,label,id,vals),
                  "number" =  Rsnakeconf.numberInput(type,label,id,vals),
                  "checkbox" = Rsnakeconf.checkboxInput(label,id,vals),
                  "checkboxGroup" = Rsnakeconf.checkboxGroupInput(label,id,vals),
                  "radioButtons" = Rsnakeconf.radioButtonsInput(label,id,vals),
                  "selectInput" = Rsnakeconf.selectInput(label,id,vals),
                  "selectInput.multi" = Rsnakeconf.selectInput.multi(label,id,vals),
                  "textArea" = Rsnakeconf.textAreaInput(label,id,vals)
    )
    
    return(res)
}

#Generate text input
Rsnakeconf.textInput <- function(type,label,id,vals){
    textForm <- div(class="form-group shiny-input-container",
        tags$label(class="control-label col-sm-3",label,`for` =id),  
        div(class="col-sm-9",
            tags$input(id=id,class="form-control",type=type,value=vals[1])
        )
    )
    return(textForm)
}
#Generate number input
Rsnakeconf.numberInput <- function(type,label,id,vals){
    numberForm <- div(class="form-group shiny-input-container",
        tags$label(class="control-label col-sm-3",label,`for` =id),  
        div(class="col-sm-9",
            tags$input(id=id,class="form-control",type=type,value=vals[1],`min` =1)
        )
    )
    return(numberForm)
}
#Generate checkbox input
Rsnakeconf.checkboxInput <- function(label,id,vals){
    checkboxForm <- div(class="form-group shiny-input-container",
                      div(class="col-sm-3",
                          tags$label(class="pull-right",label,`for` =id)
                      ),
                      div(class="col-sm-9",
                          div(
                              tags$label(
                                  tags$input(type="checkbox", id = id,checked=vals[1])
                              )    
                          )    
                      )    
    )
    return(checkboxForm)
}
#Generate checkbox group input
Rsnakeconf.checkboxGroupInput <- function(label,id,vals){
    checkboxGroupForm <- div(class="form-group shiny-input-checkboxgroup shiny-input-container shiny-input-container-inline",id=id,
                            div(class="col-sm-3",
                                tags$label(class="pull-right",label,`for` =id)
                            ),
                            div(class="col-sm-9 shiny-options-group",
                                tags$label(class="checkbox-inline",
                                           tags$input(type="checkbox",name=id,value=vals[1],checked="checked"),
                                           tags$span(vals[1])
                                ),
                                lapply(2:length(vals),function(i){
                                    tags$label(class="checkbox-inline",
                                               tags$input(type="checkbox",name=id,value=vals[i]),
                                               tags$span(vals[i])
                                    )
                                })
                            )
    )
    return(checkboxGroupForm)
}
#Generate radio input
Rsnakeconf.radioButtonsInput <- function(label,id,vals){
    radioButtonsForm <- div(class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline",id=id,
                            div(class="col-sm-3",
                                tags$label(class="pull-right",label,`for` =id)
                            ),
                            div(class="col-sm-9 shiny-options-group",
                                tags$label(class="radio-inline",
                                           tags$input(type="radio",name=id,value=vals[1],checked="checked"),
                                           tags$span(vals[1])
                                ),
                                lapply(2:length(vals),function(i){
                                    tags$label(class="radio-inline",
                                               tags$input(type="radio",name=id,value=vals[i]),
                                               tags$span(vals[i])
                                    )
                                })
                            )
    )
    return(radioButtonsForm)
}
#Generate select input
Rsnakeconf.selectInput <- function(label,id,vals){
    selectForm <- div(class="form-group shiny-input-container",
                      tags$label(class="control-label col-sm-3",label,`for` =id), 
                      div(class="col-sm-9",
                          tags$select(id=id,`size` =length(vals),
                                      tags$option(selected=T,value=vals[1],vals[1]),
                                      lapply(2:length(vals),function(i){tags$option(value=vals[i],vals[i])})
                          ),
                          tags$script(type="application/json",`data-for` =id,`data-nonempty`="","{}")
                      )
    )
    return(selectForm)
}
#Generate select input (multiple)
Rsnakeconf.selectInput.multi <- function(label,id,vals){
    selectForm <- div(class="form-group shiny-input-container",
                      tags$label(class="control-label col-sm-3",label,`for` =id), 
                      div(class="col-sm-9",
                          tags$select(multiple="multiple",id=id,`size` =length(vals),
                                      tags$option(selected=T,value=vals[1],vals[1]),
                                      lapply(2:length(vals),function(i){tags$option(value=vals[i],vals[i])})
                          ),
                          tags$script(type="application/json",`data-for` =id,"{}")
                      )
    )
    return(selectForm)
}

#Generate text area
Rsnakeconf.textAreaInput <- function(label,id,vals){
    textAreaForm <- 
        div(class="form-group shiny-input-container",
            tags$label(class="control-label col-sm-3",label,`for` =id),  
            div(class="col-sm-9",
                tags$textarea(id=id,class="form-control",vals)
            )
        )
    return(textAreaForm)
}

Rsnakeconf.addtooltip <- function(id,label,comment){
    label <- paste(label,":")
    if(is.null(comment)){
        return(label)
    }else{
        return(tags$span(id=paste0(id,"_tooltip"),href="#",`data-toggle`="tooltip", `data-placement`="top", `title`=comment,label,
            tags$script(paste0("$('#",paste0(id,"_tooltip"),"').tooltip();"))
        ))
    }
}
#' Internal function used to compute each form values from a specific form
#' 
#'
#' @param jsonfile a json format file
#' @param type Specify the form type (selectInput,numberInput ...) for compute data if needed
#' @param id the field id
#' @param default_value The default values used if the file isn't loaded
#' 
#' @return The value of parameter field from json file, ou default value. Can be samples files, experiment name, extension, parameters ...
Rsnakeconf.getValueFromFile <- function(jsonfile=NULL,type = type,id,default_value=""){
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