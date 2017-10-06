# setup
library(shiny)
library(photobiology)
library(ggspectra)
library(ooacquire)

w <- start_session()

instruments <- list_instruments(w)

# we use the descriptor stored in the package valid for "today"
## Run one of the two next statements
## old Maya
descriptor <- which_descriptor(descriptors = ooacquire::MAYP11278_descriptors)

descriptor[["w"]] <- w
descriptor[["sr.index"]] <- 0

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel(paste("Irradiance from ", descriptor[["spectrometer.name"]])),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "spctPlot")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Measure and plot a spectrum
  output$spctPlot <- renderPlot({

    settings <- acq_settings(descriptor,
                             HDR.mult = c(1,10),
                             tot.time.range = c(10,10))
    # automatic setting of integration time
    settings <- tune_acq_settings(descriptor, settings)
    settings

    # acquire a single spectrum from spectrometer
    spct_1 <- acq_raw_spct(descriptor, settings)
    plot(spct_1)

    # we use the high level function

    spct_1 <- s_irrad_corrected(spct_1, correction.method = MAYP11278_ylianttila.mthd)
    # spct_1 <- s_irrad_corrected(spct_1, correction.method = MAYP112785_ylianttila.mthd)
    plot(spct_1, unit.out = "photon")
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
