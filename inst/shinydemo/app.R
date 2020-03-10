
requireNamespace("shiny", quietly = TRUE)
requireNamespace("gtools", quietly = TRUE)

library("binovisualfields", quietly = TRUE)
library("gtools", quietly = TRUE)
fix_dist <- c(600,0)
step <- 10
near <- 150
far <- 1500
dist_planes  <- seq(near, far, step)
m_xs <- seq(-27, 27, length.out = 10)
m_ys <- seq(21, -21, -6)
c_xs <- seq(-57, 57, 6)  # x coordinates at which binocular visual field is computed (degrees)
###make fake data from the 12 archetypes
at_num <- 1:12 ## at stands for archetypes
# at_id <- sapply(1:at_num, function(i) 1:at_num + (i)*100)
# at_id <- as.vector(at_id)

at_gender <- factor(rep('female', length(at_num)), levels=c('female','male'))
# at_theta_left <- sapply(at_gender, fixdist = fix_dist, caltheta, gender="female", eye = "left")
# at_theta_rght <- sapply(at_gender, fixdist = fix_dist, caltheta, gender="female", eye = "right")


at_rght_1 <- matrix(c(
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  30, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  30, 30, 30, 30, 30, 30, 30,  0, 30, NA,
  NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA
), ncol=10, byrow=TRUE)

at_rght_2 <- matrix(c(
  NA, NA, NA,  0,  0, 30, 30, NA, NA, NA,
  NA, NA,  0,  0,  0,  0, 30, 30, NA, NA,
  NA,  0,  0,  0,  0,  0, 30, 30, 30, NA,
   0,  0,  0,  0,  0, 30, 30, 30, 30, NA,
  30, 30, 30, 30, 30, 30, 30,  0, 30, NA,
  NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA
), ncol=10, byrow=TRUE)

at_rght_3 <- matrix(c(
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA,  0, 30, 30,  0,  0,  0, 30, 30, NA,
  0,  0,  0,  0,  0,  0,  0,  0, 30, NA,
  30, 30, 30, 30, 30, 30, 30,  0, 30, NA,
  NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA
), ncol=10, byrow=TRUE)

at_rght_4 <- matrix(c(
  NA, NA, NA,  0,  0,  0,  0, NA, NA, NA,
  NA, NA,  0,  0,  0,  0,  0,  0, NA, NA,
  NA,  0,  0,  0,  0,  0,  0,  0,  0, NA,
  30, 30, 30, 30, 30,  0,  0,  0,  0, NA,
  30, 30, 30, 30, 30, 30, 30,  0, 30, NA,
  NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA
), ncol=10, byrow=TRUE)

at_rght_5 <- matrix(c(
  NA, NA, NA,  0,  0,  0,  0, NA, NA, NA,
  NA, NA,  0,  0,  0,  0,  0,  0, NA, NA,
  NA,  0,  0,  0,  0,  0,  0,  0,  0, NA,
   0,  0,  0,  0,  0,  0,  0,  0,  0, NA,
  30, 30, 30, 30, 30, 30, 30,  0,  0, NA,
  NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA
), ncol=10, byrow=TRUE)

at_rght_6 <- matrix(c(
  NA, NA, NA,  0,  0,  0,  0, NA, NA, NA,
  NA, NA,  0,  0,  0,  0,  0,  0, NA, NA,
  NA,  0,  0,  0,  0,  0,  0,  0,  0, NA,
   0,  0,  0,  0,  0,  0,  0,  0,  0, NA,
   0,  0,  0,  0,  0, 30, 30,  0,  0, NA,
  NA,  0,  0,  0,  0,  0,  0,  0,  0, NA,
  NA, NA,  0,  0,  0,  0,  0,  0, NA, NA,
  NA, NA, NA,  0,  0,  0,  0, NA, NA, NA
), ncol=10, byrow=TRUE)

# at_rght_7 <- matrix(c(
#   NA, NA, NA, 30, 30, 30, 30, NA, NA, NA,
#   NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
#   NA,  0, 30, 30, 30,  0, 30, 30, 30, NA,
#   0,  0, 30,  0,  0,  0,  0, 30, 30, NA,
#   0,  0,  0,  0,  0,  0,  0,  0, 30, NA,
#   NA,  0,  0,  0,  0,  0,  0, 30, 30, NA,
#   NA, NA,  0, 30,  0,  0, 30, 30, NA, NA,
#   NA, NA, NA, 30, 30, 30, 30, NA, NA, NA
# ), ncol=10, byrow=TRUE)



at_rght_7 <- matrix(c(
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  30, 30, 30, 30, 30, 30, 30, 30, 30, NA,
   0,  0,  0,  0,  0, 30,  0,  0,  0, NA,
  NA,  0,  0,  0,  0,  0,  0,  0,  0, NA,
  NA, NA,  0,  0,  0,  0,  0,  0, NA, NA,
  NA, NA, NA,  0,  0,  0,  0, NA, NA, NA
), ncol=10, byrow=TRUE)

at_rght_8 <- matrix(c(
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  30, 30, 30, 30, 30, 30, 30, 30, 30, NA,
   0,  0,  0,  0, 30, 30, 30,  0, 30, NA,
  NA,  0,  0,  0,  0,  0,  0,  0, 30, NA,
  NA, NA,  0,  0,  0,  0,  0,  0, NA, NA,
  NA, NA, NA,  0,  0,  0,  0, NA, NA, NA
), ncol=10, byrow=TRUE)

at_rght_9 <- matrix(c(
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  30, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  0,  0,  0,  0,  0, 30, 30,  0, 30, NA,
  NA,  0,  0,  0,  0,  0,  0, 30, 30, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA
), ncol=10, byrow=TRUE)



at_rght_10 <- matrix(c(
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  30, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  0,  0,  0,  0, 30, 30, 30,  0, 30, NA,
  NA,  0,  0,  0, 30, 30, 30, 30, 30, NA,
  NA, NA,  0, 30, 30, 30, 30, 30, NA, NA,
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA
), ncol=10, byrow=TRUE)


at_rght_11 <- matrix(c(
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  30, 30, 30, 30, 30, 30, 30, 30, 30, NA,
  30, 30, 30, 30, 30, 30, 30,  0,  0, NA,
  NA, 30, 30, 30, 30,  0,  0,  0,  0, NA,
  NA, NA, 30, 30,  0,  0,  0,  0, NA, NA,
  NA, NA, NA,  0,  0,  0,  0, NA, NA, NA
), ncol=10, byrow=TRUE)


at_rght_12 <- matrix(c(
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, 30, 30, 30, 30, 30, 30, 30,  0, NA,
  30, 30, 30, 30, 30, 30, 30, 30,  0, NA,
  30, 30, 30, 30, 30, 30, 30,  0,  0, NA,
  NA, 30, 30, 30, 30, 30, 30, 30,  0, NA,
  NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
  NA, NA, NA, 30, 30, 30, 30, NA, NA, NA
), ncol=10, byrow=TRUE)

# at_rght_13 <- matrix(c(
#   NA, NA, NA, 30, 30, 30,  0, NA, NA, NA,
#   NA, NA, 30, 30, 30, 30,  0,  0, NA, NA,
#   NA, 30, 30, 30, 30, 30,  0,  0,  0, NA,
#   30, 30, 30, 30, 30, 30,  0,  0,  0, NA,
#   30, 30, 30, 30, 30, 30,  0,  0,  0, NA,
#   NA, 30, 30, 30, 30, 30,  0,  0,  0, NA,
#   NA, NA, 30, 30, 30, 30,  0,  0, NA, NA,
#   NA, NA, NA, 30, 30, 30,  0, NA, NA, NA
# ), ncol=10, byrow=TRUE)

aa <- matrix(NA, nrow=8, ncol = 10)
at_rght_vf_array <- replicate(length(at_num), aa)

for (i in at_num){
  at_rght_vf_array[,,i] <- get(paste('at_rght_', i, sep=""))
}

at_left_vf_array <- at_rght_vf_array[, 10:1, ]

# conditions <- permutations(at_num, 2, v=1:at_num, repeats.allowed=T)
#
# at_rght_vf_array <- at_rght_array[, , conditions[,1]]
# at_left_vf_array <- at_left_array[, , conditions[,2]]


vf_height = 250
vf_width = "100%"
c_vf_height = 250
c_vf_width = "100%"
###start shiny
ui <- navbarPage(
      title ="Binocular vision simulated using glaucoma archetypes",

      tabPanel(title = "Binocular vision in 3D",
               fluidPage(

                   fluidRow(
                     column(6, h1("Binocular vision in 3D", align = "center")),
                     column(2, selectInput(inputId = "left_eye_t1", label="Left eye",  choices = at_num, size=3, selectize = FALSE,  selected=1)),
                     column(2, offset=1, selectInput(inputId = "rght_eye_t1", label="Right eye", choices = at_num, size=3, selectize = FALSE,  selected=at_num[1]))
                            ),

                   fluidRow(
                     column(6,
                          fluidRow(
                            column(12, plotOutput("combined_ray", height=500))
                                  ),
                          fluidRow(
                            column(12, sliderInput(inputId = "object_distance",
                                                   label = "Choose an object distance (mm)",
                                                   min = near, max = far, step = step, value = near+10*step, width = "100%"))
                                  ),
                          fluidRow(
                            column(12, sliderInput(inputId = "fixation_t1",
                                                   label = "Choose a fixation distance (mm)",
                                                   min = near, max = far, step = step, value = near+50*step, width = "100%"))
                                  )
                            ),
                   column(6,
                          fluidRow(
                            column(6, plotOutput("vf_left_t1",  height = vf_height, width = vf_width)),
                            column(6, plotOutput("vf_rght_t1",  height = vf_height, width = vf_width))
                                  ),
                          fluidRow(
                            column(12, offset=5, actionButton("update_t1", "Select", icon=icon("arrow-circle-right"),
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                    )
                                  ),
                          fluidRow(
                            column(12,
                                   p("Click the 'Select' button to update the integrated VF below to reflect the left and right VFs you select.")
                                    )
                                  ),
                          fluidRow(
                            column(12, plotOutput("c_vf_fixation_t1", height = 300, width = c_vf_width))
                                  ),
                          hr(),
                          fluidRow(
                            column(12, plotOutput("color_key", height = 100, width = c_vf_width))
                                  )

                   )

                 )

               )## fluidPage
               ),##tabPanel

      tabPanel(
        title = "Binocular vision simulated using 12 archetypes",

        fluidPage(

          fluidRow(
            column(12, h1("Binocular vision at close, fixation and far planes", align = "center"))
                  ),

          fluidRow(
            column(2, selectInput(inputId = "left_eye_t2", label="Left eye",  choices = at_num, size=3, selectize = FALSE,  selected=1), offset = 2),
            column(2, selectInput(inputId = "rght_eye_t2", label="Right eye", choices = at_num, size=3, selectize = FALSE,  selected=1), offset = 2),
            column(2, actionButton("update_t2", "Select", icon=icon("arrow-circle-right"),
                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      helpText("Click the 'Select' button to update the IVFs",
                             "to reflect the left and right VFs you select."), offset = 1)
                  ),

          fluidRow(
            column(4, plotOutput("vf_left_t2", height = vf_height, width = vf_width),  offset = 2),
            column(4, plotOutput("vf_rght_t2", height = vf_height, width = vf_width))
                  ),
          hr(),
          fluidRow(
            column(4, sliderInput(inputId = "object_close",
                                  label = "Choose a close object distance (mm)",
                                  min = near , max = 400, step = step, value = near+10*step, width = "100%")),
            column(4, sliderInput(inputId = "fixation_t2",
                                  label = "Choose a fixation distance (mm)",
                                  min = 400+step, max = max(fix_dist), step = step, value = 400+5*step, width = "100%")),
            column(4, sliderInput(inputId = "object_far",
                                  label = "Choose far object distance (mm)",
                                  min = max(fix_dist)+step, max = far, step = step, value = far-10*step, width = "100%"))

          ),

          fluidRow(
            column(4, plotOutput("c_vf_close", height = c_vf_height, width = c_vf_width)),
            column(4, plotOutput("c_vf_fixation_t2", height = c_vf_height, width = c_vf_width)),
            column(4, plotOutput("c_vf_far", height = c_vf_height, width = c_vf_width))
          ) ##fluidRow
        ) ## fluidPage
      ) ##tabPanel

) ##navbarPage



server <- function(input, output, session){

  ###reactive data for tab 1

  vfs_t1 <- reactive({
    input$update_t1
    isolate({
      withProgress({
        setProgress(message = "Updating simulated data...")})
      left_vf_t1 <- at_left_vf_array[, , as.numeric(input$left_eye_t1)]
      rght_vf_t1 <- at_rght_vf_array[, , as.numeric(input$rght_eye_t1)]
      list(left=left_vf_t1, rght=rght_vf_t1)
    })
  })

  fddray <- reactive({
    theta_left <- caltheta(c(input$fixation_t1, 0), gender='female', eye = "left")
    theta_rght <- caltheta(c(input$fixation_t1, 0), gender='female', eye = "right")
    c_vf       <- binovfcal(vfs_t1()$left, vfs_t1()$rght, theta_left, theta_rght, dist_planes, gender='female')
    list(theta_left = theta_left, theta_rght = theta_rght, c_vf = c_vf)
  })

  ###reactive data for tab 2

  vfs_t2 <- reactive({
    input$update_t2
    isolate({
      withProgress({
        setProgress(message = "Updating simulated data...")})
      left_vf_t2 <- at_left_vf_array[, , as.numeric(input$left_eye_t2)]
      rght_vf_t2 <- at_rght_vf_array[, , as.numeric(input$rght_eye_t2)]
      list(left=left_vf_t2, rght=rght_vf_t2)
    })
  })


  fdd_c_t2 <- reactive({

     theta_left <- caltheta(c(input$fixation_t2, 0), gender='female', eye = "left")
     theta_rght <- caltheta(c(input$fixation_t2, 0), gender='female', eye = "right")
     c_vf       <- binovfcal(vfs_t2()$left, vfs_t2()$rght, theta_left, theta_rght, dist_planes, gender='female')
     list(theta_left = theta_left, theta_rght = theta_rght, c_vf = c_vf)
  })


  ### tab 1
  output$combined_ray <- renderPlot({
    plotvfray (vfs_t1()$left, vfs_t1()$rght, fddray()$theta_left, fddray()$theta_rght, c(input$fixation_t1, 0), input$object_distance)
  }, height = 500)


  output$vf_left_t1 <- renderPlot({
    plotvf(m_xs,  at_left_vf_array[, , as.numeric(input$left_eye_t1)], "Left Monocular")
  }, height = 250)

  output$vf_rght_t1 <- renderPlot({
    plotvf(m_xs,  at_rght_vf_array[, , as.numeric(input$rght_eye_t1)], "Right Monocular")
  }, height = 250)

  output$c_vf_fixation_t1 <- renderPlot({
    plotvf(c_xs, fddray()$c_vf[, , as.character(input$object_distance)], paste0("Binocular fix = ", input$fixation_t1, "mm Object = ", input$object_distance, "mm"))
  }, height = 300)

  output$color_key <- renderPlot({colorkey()}, height = 100)

  ### tab 2
  output$vf_left_t2 <- renderPlot({
    plotvf(m_xs, at_left_vf_array[, , as.numeric(input$left_eye_t2)], "Left Monocular")
  }, height = vf_height)

  output$vf_rght_t2 <- renderPlot({
    plotvf(m_xs, at_rght_vf_array[, , as.numeric(input$rght_eye_t2)], "Right Monocular")
  }, height = vf_height)


  output$c_vf_close <- renderPlot({
    plotvf(c_xs, fdd_c_t2()$c_vf[, , as.character(input$object_close)], paste0("close distance = ", input$object_close, "mm"))
  }, height = c_vf_height)

  output$c_vf_fixation_t2 <- renderPlot({
    plotvf(c_xs, fdd_c_t2()$c_vf[, , as.character(input$fixation_t2)], paste0("fixation distance = ", input$fixation_t2, "mm"))
  }, height = c_vf_height)

  output$c_vf_far <- renderPlot({
    plotvf(c_xs, fdd_c_t2()$c_vf[, , as.character(input$object_far)], paste0("far distance = ", input$object_far, "mm"))
  }, height = c_vf_height)
}

shinyApp(ui=ui, server = server)

