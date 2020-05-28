  #In this script include all the server side functions: plots, reactive objects, etc.

## Define a server for the Shiny app
function(input, output, session) {
  
  # Track the number of element input boxes to render
  counter <- reactiveValues(n = 0)
  
  # Empty list to store parameters(ie elements, authors,..) text input vaules
  parameters <- reactiveValues()
  
  #Track the number of input boxes previously
  prevcount <-reactiveValues(n = 0)
  
  #On click: add elelemnt input box
  observeEvent(input$ele_add_btn, {
    counter$n <- counter$n + 1
    prevcount$n <- counter$n - 1})
  
  #On click: remove last elelemnt input box
  observeEvent(input$ele_rm_btn, {
    if (counter$n > 0) {
      counter$n <- counter$n - 1 
      prevcount$n <- counter$n + 1
    }
    
  })
  
  #On click: store values from element input boxes in a list within parameters list
  observeEvent(input$save_btn, {
    if (counter$n > 0) {
      parameters$elementName <- NA
      parameters$elementType <- NA
      for(i in 1:counter$n) {
        parameters$elementName[i] <- input[[paste0("elementName",i)]]
        parameters$elementType[i] <- input[[paste0("elementType",i)]]
        
      }
      output$element_list <- renderText(paste("<b>CREATE (:Element {elementName: '",parameters$elementName,"', elementType: '",parameters$elementType,"'})<b/>","<br/><br/>"))
    }
  })
  
  #Store number of element input boxes
  output$counter <- renderPrint(print(counter$n))
  
  #code to generate element input boxes
  elementboxes <- reactive({
    
    n <- counter$n
    
    if (n > 0) {
      # If the no. of element boxes previously where more than zero, then 
      #save the text inputs in those text boxes 
      if(prevcount$n > 0){
        
        vals = c()
        typ = c()
        if(prevcount$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcount$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
          inpid = paste0("elementName",i)
          vals[i] = input[[inpid]] 
          inptyp = paste0("elementType",i)
          typ[i]=input[[inptyp]]
        }
        if(isInc){
          vals <- c(vals, "New element")
          typ <- c(typ, "Construct")
        }
        
        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("elementName", i),
                    label = paste0("Element ", i), value = vals[i]),
            selectInput(inputId = paste0("elementType",i), label = paste0("What type of element is element ",i, "?"),
                    c("Construct" = "Construct","Concept" = "Concept"), selected = typ[i]))
        })
        
      }else{
        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("elementName", i),
                    label = paste0("Element ", i), value = "New element"),
            selectInput(inputId = paste0("elementType",i), label = paste0("What type of element is element ",i, "?"),
                      c("Construct" = "Construct","Concept" = "Concept")))
        }) 
      }
      
    }
    
  })
  

  
  #display element input boxes
  output$element_ui <- renderUI({ elementboxes() })
  
  
}