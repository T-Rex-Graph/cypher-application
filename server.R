#In this script include all the server side functions: plots, reactive objects, etc.

#Defining functions
to_null <- function(x){
  if(x == "") return("NULL")
  else return(paste0("'",x,"'"))
}

## Define a server for the Shiny app
function(input, output, session) {

  # Track the number of element input boxes to render
  counterElement <- reactiveValues(n = 0)

  # Empty list to store parameters(ie elements, authors,..) text input vaules
  parameters <- reactiveValues()

  #Track the number of input boxes previously
  prevcountElement <-reactiveValues(n = 0)

  #On click: add element input box
  observeEvent(input$ele_add_btn, {
    counterElement$n <- counterElement$n + 1
    prevcountElement$n <- counterElement$n - 1})

  #On click: remove last element input box
  observeEvent(input$ele_rm_btn, {
    if (counterElement$n > 0) {
      counterElement$n <- counterElement$n - 1
      prevcountElement$n <- counterElement$n + 1
    }

  })

  #On click: store values from element input boxes in a list within parameters list
  observeEvent(input$ele_save_btn, {
    if (counterElement$n > 0) {
      parameters$elementName <- NA
      parameters$elementType <- NA
      parameters$elementRole <- NA
     #parameters$elementUUID <- NA
      for(i in 1:counterElement$n) {
        parameters$elementName[i] <- input[[paste0("elementName",i)]]
        parameters$elementType[i] <- input[[paste0("elementType",i)]]
        parameters$elementRole[i] <- input[[paste0("elementRole",i)]]
        #parameters$elementUUID[i] <- UUIDgenerate(TRUE, 1)
      }
        output$element_list <- renderText(paste("<b>CREATE (:Element {elementName: '",parameters$elementName,"', elementType: '",parameters$elementType,"', elementRole: ",parameters$elementRole,"});<b/>","<br/><br/>"))
      }
    })

  #Store number of element input boxes
  output$counterElement <- renderPrint(print(counterElement$n))

  #code to generate element input boxes
  elementboxes <- reactive({

    n <- counterElement$n

    if (n > 0) {
      # If the no. of element boxes previously where more than zero, then
      #save the text inputs in those text boxes
      if(prevcountElement$n > 0){

        vals = c()
        typ = c()
        role = c()
        if(prevcountElement$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcountElement$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
          inpid = paste0("elementName",i)
          vals[i] = input[[inpid]]
          inptyp = paste0("elementType",i)
          typ[i] = input[[inptyp]]
          inprole = paste0("elementRole",i)
          role[i] = input[[inprole]]
        }
        if(isInc){
          vals <- c(vals, "New element")
          typ <- c(typ, "Construct")
          role <- c(role, "Mediate")
        }

        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("elementName", i),
                      label = paste0("Element ", i), value = vals[i]),
            selectInput(inputId = paste0("elementType",i), label = paste0("What type of element is element ",i, "?"),
                        c("Construct" = "Construct","Concept" = "Concept","Process"="Process"), selected = typ[i]),
            selectInput(inputId = paste0("elementRole",i), label = paste0("What role does element ",i, " have?"),
                        c("No Role" = "NA","Moderator" = "'Moderator'","Mediator" = "'Mediator'"), selected = role[i]))
        })

      }else{
        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("elementName", i),
                      label = paste0("Element ", i), value = "New element"),
            selectInput(inputId = paste0("elementType",i), label = paste0("What type of element is element ",i, "?"),
                        c("Construct" = "Construct","Concept" = "Concept","Process"="Process")),
            selectInput(inputId = paste0("elementRole",i), label = paste0("What role does element ",i, " have?"),
                        c("No Role" = "NULL","Moderator" = "'Moderator'","Mediator" = "'Mediator'")))
        })
      }

    }

  })



  #display element input boxes
  output$element_ui <- renderUI({ elementboxes() })

  ############################################################################

  # Track the number of input boxes to render
  counterDef <- reactiveValues(n = 0)

  #Track the number of input boxes previously
  prevcountDef <-reactiveValues(n = 0)

  #On click: add  input box
  observeEvent(input$def_add_btn, {
    counterDef$n <- counterDef$n + 1
    prevcountDef$n <- counterDef$n - 1})


  #On click: remove last input box
  observeEvent(input$def_rm_btn, {
    if (counterDef$n > 0) {
      counterDef$n <- counterDef$n - 1
      prevcountDef$n <- counterDef$n + 1
    }

  })

  #On click: store values from input boxes in a list within parameters list
  observeEvent(input$def_save_btn, {
    if (counterDef$n > 0) {
      parameters$defEle <- NA
      parameters$defFull <- NA
      #parameters$defUUID
      for(i in 1:counterDef$n) {
#       parameters$defName[i] <- input[[paste0("defName",i)]]
        parameters$defFull[i] <- input[[paste0("defFull",i)]]
        parameters$defEle[i] <- input[[paste0("defEle",i)]]
        #parameters$defUUID[i] <- UUIDgenerate(TRUE,1)
      }
      if(input$pubTyp == "Manual Pub"){
        output$def_list <- renderText(paste("<b>CREATE (:Definition {DefinitionName: '",parameters$defEle,"', definition: '",parameters$defFull,"'})","<br/><br/>",
                                            "MATCH (p:Publication {citation: '",parameters$manuCit,"'}), (a:Definition {DefinitionName: '",parameters$defEle,"'})
                                            <br>CREATE (p)-[r:DEFINES]->(a) <br>RETURN r;","<br/><br/>",
                                            "MATCH (a:Element {elementName: '",parameters$defEle,"'}), (b:Definition {DefinitionName: '",parameters$defFull,"'})
                                            <br>CREATE (a)-[r:DEFINED_AS]->(b) <br>RETURN r;<br/><br/>"))
      }
      else{
        output$def_list <- renderText(paste("<b>CREATE (:Definition {DefinitionName: '",parameters$defEle,"', definition: '",parameters$defFull,"'})","<br/><br/>",
                                            "MATCH (p:Publication {DOI: '",parameters$doi,"'}), (a:Definition {DefinitionName: '",parameters$defEle,"'})
                                            <br>CREATE (p)-[r:DEFINES]->(a) <br>RETURN r;","<br/><br/>",
                                            "MATCH (a:Element {elementName: '",parameters$defEle,"'}), (b:Definition {DefinitionName: '",parameters$defFull,"'})
                                            <br>CREATE (a)-[r:DEFINED_AS]->(b) <br>RETURN r;<br/><br/>"))
      }
    }
  })

  #Store number of input boxes
  output$counterDef <- renderPrint(print(counterDef$n))

  #code to generate input boxes
  defboxes <- reactive({

    n <- counterDef$n

    if (n > 0) {
      # If the no. of boxes previously where more than zero, then
      #save the inputs in those boxes
      if(prevcountDef$n > 0){

#       dName = c()
        dFull = c()
        dEle = c()
        if(prevcountDef$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcountDef$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
#         inpDefName = paste0("defName",i)
#         dName[i] = input[[inpDefName]]
          inpDefFull = paste0("defFull",i)
          dFull[i]=input[[inpDefFull]]
          inpDefEle = paste0("defEle",i)
          dEle[i] = input[[inpDefEle]]
        }
        if(isInc){
#         dName <- c(dName, "Definition Name")
          dFull <- c(dFull, "Definition")
#          dEle <- c(dEle, "Element Name")
        }

        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
#          textInput(inputId = paste0("defName", i),
#                    label = paste0("Definition ", i), value = dName[i]),
            textInput(inputId = paste0("defFull", i),
                      label = paste0("Definition ", i), value = dFull[i]),
            selectInput(inputId = paste0("defEle", i),
                      label = paste0("What element does Definition ", i," define?"),
                      parameters$elementName)

          )})

      }else{
        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
#           textInput(inputId = paste0("defName", i),
#                      label = paste0("Definition ", i), value = "Definition Name"),
            textInput(inputId = paste0("defFull", i),
                      label = paste0("Definition ", i), value = "Definition"),
            selectInput(inputId = paste0("defEle", i),
                      label = paste0("What element does Definition ", i," define?"),
                      parameters$elementName)
          )})
      }

    }

  })



  #display input boxes
  output$def_ui <- renderUI({ defboxes() })




  ############################################################################
  observeEvent(input$pub_save_btn, {
    parameters$pubUUID <- UUIDgenerate(TRUE, 1)
    if (input$pubTyp == "Manual Pub") {
      parameters$manuCit <- input$manuCit
      output$pub_out <- renderText(paste("<b>CREATE (:Publication {citation: '",parameters$manuCit,"', pubUUID: '",parameters$pubUUID,"'});</b>","<br/><br/>"))
    }
    else{
      parameters$doi <- input$doi
      output$pub_out <- renderText(paste("<b>CREATE (:Publication {DOI: '",parameters$doi,"', citation: '",cr_cn(parameters$doi, "text"),"', pubUUID: '",parameters$pubUUID,"'});</b>","<br/><br/>"))
    }
  })

  #code to generate element input boxes
  pubBox <- reactive({
    if (input$pubTyp == "Manual Pub") {
      # If the no. of element boxes previously where more than zero, then
      #save the text inputs in those text boxes
      manuPubInput<- tagList(
        textInput(inputId = "manuCit",
                  label = "Appropriately cite the publication"))
    }
    else{
      autoPubInput<- tagList(
        textInput(inputId = "doi",
                  label = "Enter the publication's DOI"))
    }
  })



  #display element input boxes
  output$pub_ui <- renderUI({ pubBox() })
  ###################################################################




  # Track the number of  input boxes to render
  counterModels <- reactiveValues(n = 0)

  #Track the number of input boxes previously
  prevcountModels <-reactiveValues(n = 0)

  #On click: add input box
  observeEvent(input$model_add_btn, {
    counterModels$n <- counterModels$n + 1
    prevcountModels$n <- counterModels$n - 1})

  #On click: remove last  input box
  observeEvent(input$model_rm_btn, {
    if (counterModels$n > 0) {
      counterModels$n <- counterModels$n - 1
      prevcountModels$n <- counterModels$n + 1
    }
  })

  #On click: store values from input boxes in a list within parameters list
  observeEvent(input$model_save_btn, {
    if (counterModels$n > 0) {
      parameters$modelTitle <- NA
      parameters$elementDepict <- NA
      parameters$modelUUID <- NA
      for(i in 1:counterModels$n) {
        parameters$modelTitle[i] <- input[[paste0("modelTitle",i)]]
        parameters$elementDepict[i] <- input[[paste0("elementDepict",i)]]
        parameters$modelUUID[i] <- UUIDgenerate(TRUE, 1)
      }
      if(input$pubTyp == "Manual Pub"){
        output$model_list <- renderText(paste("<b>CREATE (:Model {modelTitle: '",parameters$modelTitle,"', modelUUID: '",parameters$modelUUID,"'});</b>","<br/><br/>"
                                              ,"<b>MATCH (p:Publication {citation: '",parameters$manuCit,"'}),(m:Model)
     <br>WHERE g.modelTitle IN ['",parameters$modelTitle,"']
     <br>CREATE (p)-[r:CONTAINS]->(g)
     <br>RETURN r;</b>","<br/><br/>"
                                              ,"<b>MATCH (m:Model {modelTitle: '",parameters$modelTitle,"'}),(e:Element)
     <br>WHERE e.elementName IN [",parameters$elementDepict,"]
     <br>CREATE (g)-[r:DEPICTS]->(e)
     <br>RETURN r;</b>","<br/><br/>"))
      }
      else{
        output$model_list <- renderText(paste("<b>CREATE (:Model {modelTitle: '",parameters$modelTitle,"', modelUUID: '",parameters$modelUUID,"'});</b>","<br/><br/>"
                                              ,"<b>MATCH (p:Publication {DOI: '",parameters$doi,"'}),(m:Model)
     <br>WHERE g.modelTitle IN ['",parameters$modelTitle,"']
     <br>CREATE (p)-[r:CONTAINS]->(g)
     <br>RETURN r;</b>","<br/><br/>"
                                              ,"<b>MATCH (m:Model {modelTitle: '",parameters$modelTitle,"'}),(e:Element)
     <br>WHERE e.elementName IN [",parameters$elementDepict,"]
     <br>CREATE (g)-[r:DEPICTS]->(e)
     <br>RETURN r;</b>"))
      }
    }
  })

  #Store number of element input boxes
  output$counterModels <- renderPrint(print(counterModels$n))

  #code to generate element input boxes
  modelBoxes <- reactive({

    n <- counterModels$n

    if (n > 0) {
      # If the no. of element boxes previously where more than zero, then
      #save the text inputs in those text boxes
      if(prevcountModels$n > 0){

        grTitle = c()
        depict = c()
        if(prevcountModels$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcountModels$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
          inpTit = paste0("modelTitle",i)
          grTitle[i] = input[[inpTit]]
          inpDep = paste0("elementDepict",i)
          depict[i]=input[[inpDep]]
        }
        if(isInc){
          grTitle <- c(grTitle, "New Model")
          depict <- c(depict, "Elements")
        }

        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("modelTitle", i),
                      label = paste0("Model ", i), value = grTitle[i]),
            selectInput(inputId = paste0("elementDepict",i),
                      label = paste0("What elements does Model ",i, " depict?"), parameters$elementName, #(Please enter each element in single quotes and seprate the elements with a comma)"),
                      #value = depict[i],
                      multiple = TRUE))
        })

      }else{
        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("modelTitle", i),
                      label = paste0("Model ", i), value = "New Model"),
                      selectInput(inputId = paste0("elementDepict",i),
                                label = paste0("What elements does Model ",i, " depict?"), parameters$elementName, #(Please enter each element in single quotes and seprate the elements with a comma)"),
                                #value = depict[i],
                                multiple = TRUE))
        })
      }

    }

  })



  #display element input boxes
  output$model_ui <- renderUI({ modelBoxes() })





  ###########################################################################


  # Track the number of input boxes to render
  counterRelate <- reactiveValues(n = 0)

  #Track the number of input boxes previously
  prevcountRelate <-reactiveValues(n = 0)

  #On click: add  input box
  observeEvent(input$rel_add_btn, {
    counterRelate$n <- counterRelate$n + 1
    prevcountRelate$n <- counterRelate$n - 1})

  #On click: remove last input box
  observeEvent(input$rel_rm_btn, {
    if (counterRelate$n > 0) {
      counterRelate$n <- counterRelate$n - 1
      prevcountRelate$n <- counterRelate$n + 1
    }

  })

  #On click: store values from input boxes in a list within parameters list
  observeEvent(input$rel_save_btn, {
    if (counterRelate$n > 0) {
      parameters$ele1 <- NA
      parameters$ele2 <- NA
      parameters$relTyp <- NA
      parameters$grOrig <- NA
      #parameters$relUUID <- NA
      for(i in 1:counterRelate$n) {
        parameters$ele1[i] <- input[[paste0("ele1",i)]]
        parameters$ele2[i] <- input[[paste0("ele2",i)]]
        parameters$desc[i] <- input[[paste0("desc",i)]]
        parameters$relTyp[i] <- input[[paste0("relTyp",i)]]
        parameters$grOrig[i] <- input[[paste0("grOrig",i)]]
        #parameters$relUUID[i] <- UUIDgenerate(TRUE, 1)
      }
      output$rel_list <- renderText(paste("<b>MATCH (a:Element {elementName: '",parameters$ele1,"'}),(b:Element {elementName: '",parameters$ele2,"'})
    <br>CREATE (a)-[r:RELATES_TO {description:",to_null(parameters$desc),", type: '",parameters$relTyp,"', model: '",parameters$grOrig,"'}]->(b)
    <br>RETURN r;</b>","<br/><br/>"))
    }
  })

  #Store number of input boxes
  output$counterRelate <- renderPrint(print(counterRelate$n))

  #code to generate input boxes
  relBoxes <- reactive({

    n <- counterRelate$n

    if (n > 0) {
      # If the no. of boxes previously where more than zero, then
      #save the inputs in those boxes
      if(prevcountRelate$n > 0){

        el1 = c()
        el2 = c()
        descri = c()
        rel_Typ = c()
        gr_Orig = c()

        if(prevcountRelate$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcountRelate$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
          inpele1 = paste0("ele1",i)
          el1[i] = input[[inpele1]]
          inpele2 = paste0("ele2",i)
          el2[i]=input[[inpele2]]
          inpdesc = paste0("desc",i)
          descri[i] = input[[inpdesc]]
          inprelTyp = paste0("relTyp",i)
          rel_Typ[i] = input[[inprelTyp]]
          inpgrOrig = paste0("grOrig",i)
          gr_Orig[i] = input[[inpgrOrig]]

        }
        if(isInc){
          el1 <- c(el1, "Element 1")
          el2 <- c(el2, "Element 2")
          #descri <- c(descri, "Description")
          rel_Typ <- c(rel_Typ, "Relationship Type")
          gr_Orig <- c(gr_Orig, "Model Origin")
        }

        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            selectInput(inputId = paste0("ele1",i), label = "Create a relationship between two elements.", parameters$elementName,value = el1[i]),
            selectInput(inputId = paste0("ele2",i), label = "Relates to", parameters$elementName,value = el2[i]),
            textInput(inputId = paste0("desc", i),  label = "Description of the relationship (if applicable)", value = ""),
            selectInput(inputId = paste0("relTyp",i), label = paste0("What type of relationship is this?"),
                        c("Causal" = "Causal","Temporal" = "Temporal"), selected = rel_Typ[i]),
            selectInput(inputId = paste0("grOrig",i), label = "What model did this relationship come from?", parameters$modelTitle,
                      value = gr_Orig[i])
          )
        })


      }else{
        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            selectInput(inputId = paste0("ele1",i), label = "Create a relationship between two elements.", parameters$elementName),
            selectInput(inputId = paste0("ele2",i), label = "Relates to", parameters$elementName),
            textInput(inputId = paste0("desc", i), label = "Description of the relationship (if applicable)", value = ""),
            selectInput(inputId = paste0("relTyp",i), label = paste0("What type of relationship is this?"),
                        c("Causal" = "Causal","Temporal" = "Temporal")),
            selectInput(inputId = paste0("grOrig",i), label = "What model did this relationship come from?", parameters$modelTitle)
          )
        })

      }

    }
  })


  #display input boxes
  output$rel_ui <- renderUI({ relBoxes() })








  #############################################################

  # Track the number of input boxes to render
  counterAuthor <- reactiveValues(n = 0)

  #Track the number of input boxes previously
  prevcountAuthor <-reactiveValues(n = 0)

  #On click: add  input box
  observeEvent(input$auth_add_btn, {
    counterAuthor$n <- counterAuthor$n + 1
    prevcountAuthor$n <- counterAuthor$n - 1})

  #On click: remove last input box
  observeEvent(input$auth_rm_btn, {
    if (counterAuthor$n > 0) {
      counterAuthor$n <- counterAuthor$n - 1
      prevcountAuthor$n <- counterAuthor$n + 1
    }

  })

  #On click: store values from input boxes in a list within parameters list
  observeEvent(input$auth_save_btn, {
    if (counterAuthor$n > 0) {
      parameters$authFirst <- NA
      parameters$authMiddle <- NA
      parameters$authLast <- NA
      parameters$orcid <- NA
      parameters$authOrder <- NA
      parameters$authUUID <- NA
      for(i in 1:counterAuthor$n) {
        parameters$authFirst[i] <- input[[paste0("authFirst",i)]]
        parameters$authMiddle[i] <- input[[paste0("authMiddle",i)]]
        parameters$authLast[i] <- input[[paste0("authLast",i)]]
        parameters$orcid[i] <- input[[paste0("orcid",i)]]
        parameters$authOrder[i] <- i
        parameters$authUUID <- UUIDgenerate(TRUE, 1)
      }
      if(input$pubTyp == "Manual Pub"){
        output$auth_list <- renderText(paste("<b>CREATE (:Author {ORCID: ",to_null(parameters$orcid),", authorFirst: '",parameters$authFirst,"', authorMiddle: ",to_null(parameters$authMiddle),", authorLast: '",parameters$authLast,"', authUUID: '",parameters$authUUID,"'});","<br/><br/>"
                                             ,"MATCH (p:Publication {citation: '",parameters$manuCit,"'}), (a:Author {{ORCID: ",to_null(parameters$orcid),", authorFirst: '",parameters$authFirst,"', authorMiddle: ",to_null(parameters$authMiddle),", authorLast: '",parameters$authLast,"'})
                                             <br>CREATE (p)-[r:WRITTEN_BY {authorOrder: ",parameters$authOrder,"}]->(a)
                                             <br>RETURN r;</b>","<br/><br/>"))
      }
      else{
        output$auth_list <- renderText(paste("<b>CREATE (:Author {ORCID: ",to_null(parameters$orcid),", authorFirst: '",parameters$authFirst,"', authorMiddle: ",to_null(parameters$authMiddle),", authorLast: '",parameters$authLast,"', authUUID: '",parameters$authUUID,"'});","<br/><br/>"
                                             ,"MATCH (p:Publication {DOI: '",parameters$doi,"'}), (a:Author {{ORCID: ",to_null(parameters$orcid),", authorFirst: '",parameters$authFirst,"', authorMiddle: ",to_null(parameters$authMiddle),", authorLast: '",parameters$authLast,"'})
                                             <br>CREATE (p)-[r:WRITTEN_BY {authorOrder: ",parameters$authOrder,"}]->(a)
                                             <br>RETURN r;</b>","<br/><br/>"))
      }
    }
  })

  #Store number of input boxes
  output$counterAuthor <- renderPrint(print(counterAuthor$n))

  #code to generate input boxes
  authBoxes <- reactive({

    n <- counterAuthor$n

    if (n > 0) {
      # If the no. of boxes previously where more than zero, then
      #save the inputs in those boxes
      if(prevcountAuthor$n > 0){

        auth_First = c()
        auth_Middle = c()
        auth_Last = c()
        auth_Orcid = c()

        if(prevcountAuthor$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcountAuthor$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
          inpAuthFirst = paste0("authFirst",i)
          auth_First[i] = input[[inpAuthFirst]]
          inpAuthMiddle = paste0("authMiddle",i)
          auth_Middle[i]=input[[inpAuthMiddle]]
          inpAuthLast = paste0("authLast",i)
          auth_Last[i] = input[[inpAuthLast]]
          inpOrcid = paste0("orcid",i)
          auth_Orcid[i]=input[[inpOrcid]]
        }
        if(isInc){
          auth_First <- c(auth_First, "")
          auth_Middle <- c(auth_Middle, "")
          auth_Last <- c(auth_Last, "")
          auth_Orcid <- c(auth_Orcid, "")

        }

        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("authFirst", i),
                      label = paste0("Author ", i,"'s First Name"), value = auth_First[i]),
            textInput(inputId = paste0("authMiddle", i),
                      label = paste0("Author ", i,"'s Middle Name/Initial (if applicable)"), value = auth_Middle[i]),
            textInput(inputId = paste0("authLast", i),
                      label = paste0("Author ", i,"'s Last Name"), value = auth_Last[i]),
            textInput(inputId = paste0("orcid", i),
                      label = paste0("Author ", i,"'s ORCID (if applicable)"), value = auth_Orcid[i]),
          )
        })

      }else{
        lapply(seq_len(n), function(i) {
          elementInputs <- tagList(
            textInput(inputId = paste0("authFirst", i),
                      label = paste0("Author ", i,"'s First Name"), value = ""),
            textInput(inputId = paste0("authMiddle", i),
                      label = paste0("Author ", i,"'s Middle Name/Initial (if applicable)"), value = ""),
            textInput(inputId = paste0("authLast", i),
                      label = paste0("Author ", i,"'s Last Name"), value = ""),
            textInput(inputId = paste0("orcid", i),
                      label = paste0("Author ", i,"'s ORCID (if applicable)"), value = ""),
          )
        })
      }

    }

  })



  #display input boxes
  output$auth_ui <- renderUI({ authBoxes() })


  ##########################################################################


  # Track the number of  input boxes to render
  counterTheory <- reactiveValues(n = 0)

  #Track the number of input boxes previously
  prevcountTheory <-reactiveValues(n = 0)

  #On click: add input box
  observeEvent(input$theo_add_btn, {
    counterTheory$n <- counterTheory$n + 1
    prevcountTheory$n <- counterTheory$n - 1})

  #On click: remove last input box
  observeEvent(input$theo_rm_btn, {
    if (counterTheory$n > 0) {
      counterTheory$n <- counterTheory$n - 1
      prevcountTheory$n <- counterTheory$n + 1
    }

  })

  #On click: store values from input boxes in a list within parameters list
  observeEvent(input$theo_save_btn, {
    if (counterTheory$n > 0) {
      parameters$theoName <- NA
      #parameters$theoUUID <- NA
      for(i in 1:counterTheory$n) {
        parameters$theoName[i] <- input[[paste0("theoName",i)]]
        #parameters$theoUUID <- UUIDgenerate(TRUE,1 )
      }
      if(input$pubTyp == "Manual Pub"){
        output$theory_list <- renderText(paste("<b>CREATE (:Theory {theoryTitle: '",parameters$theoName,"'});
          <br>MATCH (t:Theory {theoryTitle: '",parameters$theoName,"'}), (p:Publication {citation: '",parameters$manuCit,"'})
          <br>CREATE (p)-[r:DRAWS_ON]->(t)
          <br>RETURN r;<b/>","<br/><br/>"))
      }
      else{
        output$theory_list <- renderText(paste("<b>CREATE (:Theory {theoryTitle: '",parameters$theoName,"'});
          <br>MATCH (t:Theory {theoryTitle: '",parameters$theoName,"'}), (p:Publication {DOI: '",parameters$doi,"'})
          <br>CREATE (p)-[r:DRAWS_ON]->(t)
          <br>RETURN r;<b/>","<br/><br/>"))
      }
    }
  })

  #Store number of input boxes
  output$counterTheory <- renderPrint(print(counterTheory$n))

  #code to generate input boxes
  theoryboxes <- reactive({

    n <- counterTheory$n

    if (n > 0) {
      # If the no. of boxes previously where more than zero, then
      #save the inputs in those boxes
      if(prevcountTheory$n > 0){

        thName = c()
        if(prevcountTheory$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcountTheory$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
          inpTheoryName = paste0("theoName",i)
          thName[i] = input[[inpTheoryName]]
        }
        if(isInc){
          thName <- c(thName, "Theory Name")
        }

        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("theoName", i),
                      label = paste0("Theory ", i), value = thName[i]),
          )})

      }else{
        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("theoName", i),
                      label = paste0("Theory ", i), value = "Theory Name"),
          )})
      }

    }

  })



  #display input boxes
  output$theory_ui <- renderUI({ theoryboxes() })


###################################################################################################


#output$downloadData <- downloadHandler("cypherScript.txt",
#content = function(file){
#write_file(c(output$pub_out,output$element_list,output$def_list,output$model_list,output$rel_list,output$auth_list,output$theory_list), file)
#}
#)

#output$downloadData <- downloadHandler("cypherScript.txt",
#content = function(file){
#write_file(c(paste("CREATE (:Publication {citation: '",parameters$manuCit,"'});
#  \nCREATE (:Element {elementName: '",parameters$elementName,"', elementType: '",parameters$elementType,"', elementRole: ",parameters$elementRole,"});
#  \nCREATE (:Definition {DefinitionName: '",parameters$defEle,"', definition: '",parameters$defFull,"'})",
#      "MATCH (p:Publication {citation: '",parameters$manuCit,"'}), (a:Definition {DefinitionName: '",parameters$defEle,"'})
#      CREATE (p)-[r:DEFINES]->(a) <br>RETURN r;",
#      "MATCH (a:Element {elementName: '",parameters$defEle,"'}), (b:Definition {DefinitionName: '",parameters$defFull,"'})
#      CREATE (a)-[r:DEFINED_AS]->(b) <br>RETURN r;
#  \n")), file)
#}
#)

#put$downloadData <- downloadHandler("cypherScript.txt",
#content = function(file){
#write_file(c(downloads$element_list[1]), file)
#}
#)
  
output$downloadData <- downloadHandler("cypherScript.txt",
content = str_view(output$element_list)
)
















}
