
requireNamespace("shiny", quietly = TRUE)
requireNamespace("gtools", quietly = TRUE)

pd_gender <- c(62, 64)      # mm - inter pupiliary distance 62 for female 64 for male
fix_dist <- c(600,0)
m_xs <- seq(-27, 27, length.out = 10)
m_ys <- seq(21, -21, -6)
c_xs <- seq(-57, 57, 6)  # x coordinates at which binocular visual field is computed (degrees)
plen <- 10000 # line length for drawing rays (mm).
dist_planes  <- seq(140, 1200, 20)

###make fake data from the 12 archetypes

atbino.df <- read.csv(system.file("extdata", "atbinocular.csv", package = "binovisualfields"))
at_id <- unique(atbino.df$id)
at_gender <- atbino.df$gender[1:length(at_id)]
at_pd <- ifelse(at_gender=="Female", pd_gender[1],  pd_gender[2])

at_theta_left <- sapply(at_pd, caltheta, fixdist = fix_dist, eye = "left")
at_theta_rght <- sapply(at_pd, caltheta, fixdist = fix_dist, eye = "right")

at_rght_visual_fields <- atbino.df[atbino.df$seye=="OD", 4:ncol(atbino.df)]
at_left_visual_fields <- atbino.df[atbino.df$seye=="OS", 4:ncol(atbino.df)]

#initialize vf array for all patients
vf_matrix <- matrix(NA, ncol=10, nrow = 8)
at_rght_vf_array <- replicate(nrow(at_rght_visual_fields), vf_matrix)
for (i in 1:nrow(at_rght_visual_fields)){
  at_rght_vf_array[,,i] <- makevf(unlist(at_rght_visual_fields[i,], use.names = F), eye="right")
}

at_left_vf_array <- replicate(nrow(at_left_visual_fields), vf_matrix)
for (i in 1:nrow(at_rght_visual_fields)){
  at_left_vf_array[,,i] <- makevf(unlist(at_left_visual_fields[i,], use.names = F), eye="left")
}



vf_height = 250
vf_width = "80%"
c_vf_height = 250
c_vf_width = "100%"
###start shiny
ui <- navbarPage(
      title ="Binocular vision simulated using glaucoma archetypes",

      tabPanel(title = "Binocular vision in 3D",
               fluidPage(

                 fluidRow(
                   fluidRow(
                     column(6, h1("Binocular vision in 3D", align = "center")),
                     column(3, selectInput(inputId = "id_t1", label="Combination ID", choices = at_id, size=3, selectize = FALSE)),
                     column(3, actionButton("update_t1", "Select")))
                 ),

                 fluidRow(
                   column(6,
                          fluidRow(
                            column(12, plotOutput("combined_ray", height=500))
                          ),
                          fluidRow(
                            column(12, sliderInput(inputId = "object_distance",
                                                   label = "Choose an object distance (cm)",
                                                   min = min(dist_planes), max = max(dist_planes), step = 20, value = 600, width = "100%"))
                          ),
                          fluidRow(
                            column(12, sliderInput(inputId = "fixation_t1",
                                                   label = "Choose a fixation distance (mm)",
                                                   min = min(dist_planes), max = max(dist_planes), step = 20, value = 600, width = "100%"))
                          )
                   ),
                   column(6,
                          fluidRow(
                            column(6, plotOutput("vf_left_t1",  height = vf_height, width = vf_width)),
                            column(6, plotOutput("vf_rght_t1",  height = vf_height, width = vf_width))
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
            column(6, h1("Binocular vision at close, fixation and far planes", align = "center")),
            column(3, selectInput(inputId = "id_t2", label="Combination ID", choices = at_id, size = 3, selectize = FALSE)),
            column(3, actionButton("update_t2", "Select"))
          ),

          fluidRow(
            column(4, plotOutput("vf_left_t2", height = vf_height, width = vf_width),  offset = 2),
            column(4, plotOutput("vf_rght_t2", height = vf_height, width = vf_width))
          ),

          fluidRow(
            column(4, sliderInput(inputId = "object_close",
                                  label = "Choose a close object distance (mm)",
                                  min = min(dist_planes), max = 400, step = 20, value = 240, width = "100%")),
            column(4, sliderInput(inputId = "fixation_t2",
                                  label = "Choose a fixation distance (mm)",
                                  min = 420, max = 700, step = 20, value = 600, width = "100%")),
            column(4, sliderInput(inputId = "object_far",
                                  label = "Choose a far object distance (mm)",
                                  min = 720, max = max(dist_planes), step = 20, value = 1000, width = "100%"))

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
  fdd <- reactive({
    input$update_t1
    isolate({
      withProgress({
        setProgress(message = "Updating simulated data...")})
      index   <-  match(input$id_t1, at_id)
      rght_vf <-  at_rght_vf_array[, , index]
      left_vf <-  at_left_vf_array[, , index]
      pd      <-  at_pd[index]
    })
    list(index = index, left_vf = left_vf, rght_vf = rght_vf, pd = pd)

  })



  fddray <- reactive({
    theta_left <- caltheta(c(input$fixation_t1, 0), pd=fdd()$pd, eye = "left")
    theta_rght <- caltheta(c(input$fixation_t1, 0), pd=fdd()$pd, eye = "right")
    c_vf       <- binovfcal(fdd()$left_vf, fdd()$rght_vf, theta_left, theta_rght, dist_planes, pd=fdd()$pd)
    list(theta_left = theta_left, theta_rght = theta_rght, c_vf = c_vf)
  })

  fdd_t2 <- reactive({
    input$update_t2
    isolate({
      withProgress({
        setProgress(message = "Updating simulated data...")})
      index      <-  match(input$id_t2, at_id)
      rght_vf <- at_rght_vf_array[, , index]
      left_vf <- at_left_vf_array[, , index]
      pd      <- at_pd[index]
    })
    list(index = index, left_vf = left_vf, rght_vf = rght_vf, pd = pd)
  })

  fdd_c_t2 <- reactive({
     theta_left <- caltheta(c(input$fixation_t2, 0), pd=fdd_t2()$pd, eye = "left")
     theta_rght <- caltheta(c(input$fixation_t2, 0), pd=fdd_t2()$pd, eye = "right")
     c_vf       <- binovfcal(fdd_t2()$left_vf, fdd_t2()$rght_vf, theta_left, theta_rght, dist_planes, pd=fdd_t2()$pd)
     c_vf_norm  <- binovfcal(at_left_vf_array[, , 1], at_rght_vf_array[, , 1], theta_left, theta_rght, dist_planes, pd=fdd_t2()$pd)
     list(c_vf = c_vf, c_vf_norm=c_vf_norm)
  })


  ### tab 1
  output$combined_ray <- renderPlot({
    plotvfray (fdd()$left_vf, fdd()$rght_vf, fddray()$theta_left, fddray()$theta_rght, c(input$fixation_t1, 0), input$object_distance)
  }, height = 500)


  output$vf_left_t1 <- renderPlot({
    plotvf(m_xs, fdd()$left_vf, "Left Monocular")
  }, height = 250)

  output$vf_rght_t1 <- renderPlot({
    plotvf(m_xs, fdd()$rght_vf, "Right Monocular")
  }, height = 250)

  output$c_vf_fixation_t1 <- renderPlot({
    plotvf(c_xs, fddray()$c_vf[, , as.character(input$object_distance)], paste0("Binocular fix = ", input$fixation_t1, "mm Object = ", input$object_distance, "mm"))
  }, height = 300)

  output$color_key <- renderPlot({colorkey()}, height = 100)

  ### tab 2
  output$vf_left_t2 <- renderPlot({
    plotvf(m_xs, fdd_t2()$left_vf, "Left Monocular")
  }, height = vf_height)

  output$vf_rght_t2 <- renderPlot({
    plotvf(m_xs, fdd_t2()$rght_vf, "Right Monocular")
  }, height = vf_height)


  output$c_vf_close <- renderPlot({
    plotvf_2(c_xs, fdd_c_t2()$c_vf[, , as.character(input$object_close)], fdd_c_t2()$c_vf_norm[, , as.character(input$object_close)], paste0("close distance=", input$object_close, "mm"))
  }, height = c_vf_height)

  output$c_vf_fixation_t2 <- renderPlot({
    plotvf_2(c_xs, fdd_c_t2()$c_vf[, , as.character(input$fixation_t2)], fdd_c_t2()$c_vf_norm[, , as.character(input$fixation_t2)], paste0("fixation distance=", input$fixation_t2, "mm"))
  }, height = c_vf_height)

  output$c_vf_far <- renderPlot({
    plotvf_2(c_xs, fdd_c_t2()$c_vf[, , as.character(input$object_far)], fdd_c_t2()$c_vf_norm[, , as.character(input$object_far)], paste0("far distance=", input$object_far, "mm"))
  }, height = c_vf_height)
}

shinyApp(ui=ui, server = server)

