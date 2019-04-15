server<-function(input,output,session){
  #Updates all of the selected state options depending on which state options you had selected
  observe(
    if("Deselect All" %in% input$z){
      selected_choices<-z[-(1:54)]
      updateSelectInput(session,"z",selected = selected_choices)
    },priority = 1
  )
  observe(
  if("Select All" %in% input$z){
    selected_choices<-z[-c(1:4)]
    updateSelectInput(session,"z",selected = selected_choices)
  },priority = 2
  )
  observe(
    if("Continental" %in% input$z){
      selected_choices<-z[-c(1:4,6,15)]
      updateSelectInput(session,"z",selected = selected_choices)
    },priority = 3
  )
  observe(
    if("MAV" %in% input$z){
      selected_choices<-c(z[c(8,17,21,22,28,29,46)])
      updateSelectInput(session,"z",selected = selected_choices)
    },priority=4
  )
  #Creates an input value for the special inputs
  states<- reactive({
     if ("Deselect All" %in% input$z){
      NULL
    } else{
      if ("Select All" %in% input$z){
        z[-(1:4)]
      } else{
        if ("Continental" %in% input$z){
          z[-c(1:4,6,15)]
        } else{
          if ("MAV" %in% input$z){
            c(z[c(8,17,21,22,28,29,46)])
          } else{
            input$z
        }
        }
      }
    }
  })
  #Plots the map
  output$map<-renderPlot({
    #Checks to see if the year restrictions are met, if they aren't then an error message is printed
    shiny::validate(need(input$x <= input$y,"The Start Year cannot be after the End Year. Please adjust the time range."))
    Enrollment_Map(x=input$x,y=input$y,z=states(),countylevel=input$countylevel,borders=input$borders)})
}
