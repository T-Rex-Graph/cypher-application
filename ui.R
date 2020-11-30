# This script includes the user-interface definition of the app.

fluidPage(

  ui <- shinyUI(fluidPage(

    sidebarLayout(
      sidebarPanel(
        ######################.
        # Pub Initialization----
        ######################.
        h4("Publication"),
        selectInput(inputId = "pubTyp", label = "Will you be citing the publication inputted manually or with DOI assistance?",
                    c("DOI"="DOI Pub","Manual" = "Manual Pub")),#,"ISBN"="ISBN Input")),
        actionButton("pub_save_btn", "Save Publication Input"),
      ),
      # displays the input elements
      mainPanel(
        fluidRow(
          ####################.
          # Publication Input ----
          ####################.
          column(7,
                 h4("Publication Boxes"),
                 uiOutput("pub_ui")),
          ####################.
          # Pub Output----
          ####################.
          column(4,
                 h4("Publication Output"),
                 htmlOutput("pub_out")),
        )
      ),
    ),







    sidebarLayout(
    sidebarPanel(
      ######################.
      # Element Buttons----
      ######################.
      h4("Elements"),
      #adds a new element textbox & selection
      actionButton("ele_add_btn", "Add Element"),
      #removes last element textbox & selection
      actionButton("ele_rm_btn", "Remove Element"),
      #save all element textbox & selection inputs into a list
      actionButton("ele_save_btn", "Save Element List"),
      #prints the number of element input boxes in ui sidebar
      textOutput("counterElement")

 ),
 # displays the input elements
 mainPanel(
   fluidRow(
     ####################.
     # Element Input ----
     ####################.
     column(7,
            h4("Element Boxes"),
            uiOutput("element_ui")),
     ####################.
     # Element Output----
     ####################.
     column(4,
            h4("Element Output"),
            htmlOutput("element_list"))
   ))
 ),


 sidebarLayout(
   sidebarPanel(
     ######################.
     # Definition Buttons----
     ######################.
     h4("Definitions"),
     #adds a new textbox & selection
     actionButton("def_add_btn", "Add Defintion"),
     #removes last textbox & selection
     actionButton("def_rm_btn", "Remove Defnition"),
     #save all textbox & selection inputs into a list
     actionButton("def_save_btn", "Save Defnition List"),
     #prints the number of element input boxes in ui sidebar
     textOutput("counterDef")

   ),
   # displays the input elements
   mainPanel(
     fluidRow(
       ####################.
       # Definition Input ----
       ####################.
       column(7,
              h4("Definition Boxes"),
              uiOutput("def_ui")),
       ####################.
       # Definition Output----
       ####################.
       column(4,
              h4("Definition Output"),
              htmlOutput("def_list"))
     ))
 ),























 sidebarLayout(
   sidebarPanel(
     ######################.
     # Model Initialization----
     ######################.
     h4("Models"),
     #adds a new model textbox
     actionButton("model_add_btn", "Add Model"),
     #remove last model textbox
     actionButton("model_rm_btn", "Remove Model"),
     #save all model textbox inputs into a list
     actionButton("model_save_btn", "Save Model Input"),
     #prints the number of element input boxes in ui sidebar
     textOutput("counterModels")


   ),
   # displays the input elements
   mainPanel(
     fluidRow(
       ####################.
       # Publication Input ----
       ####################.
       column(7,
              h4("Model Boxes"),
              uiOutput("model_ui")),
       ####################.
       # Pub Output----
       ####################.
       column(4,
              h4("Model Output"),
              htmlOutput("model_list")),
     )
   )
 ),


 sidebarLayout(
    sidebarPanel(
       ######################.
       # Relationship Initialization----
       ######################.
       h4("Element Relationships"),
       #adds a new UI
       actionButton("rel_add_btn", "Add Relationship"),
       #remove last UI
       actionButton("rel_rm_btn", "Remove Relationship"),
       #save all UI inputs into a list
       actionButton("rel_save_btn", "Save Relationship Input"),
       #prints the number of UI Outputs in ui sidebar
       textOutput("counterRelate")


    ),
    # displays the input elements
    mainPanel(
       fluidRow(
          ####################.
          # Publication Input ----
          ####################.
          column(7,
                 h4("Element Relationship Boxes"),
                 uiOutput("rel_ui")),
          ####################.
          # Pub Output----
          ####################.
          column(4,
                 h4("Element Relationship"),
                 htmlOutput("rel_list")),
       )
    )
 ),

 sidebarLayout(
    sidebarPanel(
       ######################.
       # Relationship Initialization----
       ######################.
       h4("Authors"),
       h6("Please input authors in desired order"),
       #adds a new UI
       actionButton("auth_add_btn", "Add Author"),
       #remove last UI
       actionButton("auth_rm_btn", "Remove Author"),
       #save all UI inputs into a list
       actionButton("auth_save_btn", "Save Author Input"),
       #prints the number of UI Outputs in ui sidebar
       textOutput("counterAuthor")


    ),
    # displays the input elements
    mainPanel(
       fluidRow(
          ####################.
          # Publication Input ----
          ####################.
          column(7,
                 h4("Author Boxes"),
                 uiOutput("auth_ui")),
          ####################.
          # Pub Output----
          ####################.
          column(4,
                 h4("Author Output"),
                 htmlOutput("auth_list")),
       )
    )
 ),




 sidebarLayout(
   sidebarPanel(
     ######################.
     # Theory Buttons----
     ######################.
     h4("Theory Buttons"),
     #adds a new textbox & selection
     actionButton("theo_add_btn", "Add Defintion"),
     #removes last textbox & selection
     actionButton("theo_rm_btn", "Remove Defnition"),
     #save all textbox & selection inputs into a list
     actionButton("theo_save_btn", "Save Defnition List"),
     #prints the number of element input boxes in ui sidebar
     textOutput("counterTheory")

   ),
   # displays the input elements
   mainPanel(
     fluidRow(
       ####################.
       # Theory Input ----
       ####################.
       column(7,
              h4("Theory Boxes"),
              uiOutput("theory_ui")),
       ####################.
       # Definition Output----
       ####################.
       column(4,
              h4("Theory Output"),
              htmlOutput("theory_list"))
     ))
 ),

 sidebarLayout(
   sidebarPanel(
     ######################.
     # Download Buttons----
     ######################.
     h4("Save Code"),
     #adds a new textbox & selection
     downloadButton("downloadData","Download"),
   ),
   mainPanel(
     fluidRow(
     ))
 ),


  )#Close fluidpage()
  )#Close shinyUI()


)#CLOSES fluidPage
