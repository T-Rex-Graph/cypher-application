# This script includes the user-interface definition of the app.


fluidPage(

  
  ui <- shinyUI(fluidPage(
    
    sidebarPanel(
      ######################.
      # Element Buttons----
      ######################.
      h4("Element Buttons"),
      #adds a new element textbox
      actionButton("ele_add_btn", "Add Element"),
      #removes last element textbox
      actionButton("ele_rm_btn", "Remove Element"),
      #save all element textbox inputs into a list
      actionButton("save_btn", "Save Element List"),
      #prints the number of element input boxes in ui sidebar
      textOutput("counter")
      
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
               h4("Element Output List"),
               htmlOutput("element_list"))
      ))
    
  ))
  
  
  
)#CLOSES fluidPage