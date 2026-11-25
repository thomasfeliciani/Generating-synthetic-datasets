

# Toy simulation of flying birds. 
# Thomas Feliciani, 24 November 2026

# Runs in R 4.6.1. 


# Loading libraries
library("ggplot2")
library ("gganimate")
library("gifski")




# ______________________________________________________________________________
# Setting simulation parameters

set.seed(1234)
gridSize <- 200  # Resolution of sky features
numBirds <- 5    # Number of birds to be generated
timeSteps <- 100 # Number of animation frames




# ______________________________________________________________________________
# Generating the sky. 2D simplex noise populates it with clouds.
noiseMatrix <- ambient::noise_simplex(
  dim = c(gridSize, gridSize),
  frequency = 0.005,
  octaves = 4,
  gain = 0.5
)

# Converting the matrix into a data.frame
skyData <- transform(
  expand.grid(xCoord = 1:gridSize, yCoord = 1:gridSize),
  elevation = as.vector(noiseMatrix)
)

# Let's see a preview:
ggplot2::ggplot() +
  ggplot2::geom_tile(
    data = skyData,
    mapping = ggplot2::aes(x = xCoord, y = yCoord, fill = elevation),
    show.legend = FALSE
  ) +
  ggplot2::scale_fill_gradientn(
    colors = c("#21528c", "#4a85bc", "#a8c5dd", "#eeeeee", "#ffffff")
  )




# ______________________________________________________________________________
# Simulating bird flight path.

birdData <- lapply( # For each bird...
  X = 1:numBirds,
  
  FUN = \(birdId) { # ... we run a function ...
    
    # ... that, given some random starting coodinates in the sky...
    xStart = rnorm(n = 1, mean = gridSize / 2, sd = gridSize / 5)
    yStart = rnorm(n = 1, mean = gridSize / 2, sd = gridSize / 5)
    
    # ... simulates a random path using 1D symplex noise.
    data.frame(
      birdId = birdId,
      frame = seq_len(timeSteps),
      x = xStart + (100 * c(
        ambient::noise_simplex(dim = c(1, timeSteps), frequency = 0.005)
      )),
      y = xStart + (100 * c(
        ambient::noise_simplex(dim = c(1, timeSteps), frequency = 0.005)
      )),
      shape = sample(
        size = timeSteps, 
        x = c("hold", "flap!"), 
        prob = c(0.9, 0.1), 
        replace = TRUE
      )
    )
  }
)

# Merging all paths into a single data.frame:
birdData <- do.call(what = rbind, args = birdData)


# Removing any data point that may fall outside of the grid / sky:
birdData <- subset(
  x = birdData, 
  subset = ! (
    birdData$x < 1 | birdData$x > gridSize | birdData$y < 1 | birdData$y > gridSize
  )
)

# Let's get a preview of the flight paths:
ggplot2::ggplot(
  data = birdData,
  mapping = ggplot2::aes(x = x, y = y, color = as.factor(birdId))
) + 
  ggplot2::geom_path() +
  ggplot2::labs(color = "Bird")



# ______________________________________________________________________________
# Plotting the animation, one time step per frame:
animatedPlot <- ggplot2::ggplot() +
  
  # Sky
  ggplot2::geom_tile(
    data = skyData,
    mapping = ggplot2::aes(x = xCoord, y = yCoord, fill = elevation),
    show.legend = FALSE
  ) +
  ggplot2::scale_fill_gradientn(
    colors = c("#21528c", "#4a85bc", "#a8c5dd", "#eeeeee", "#ffffff")
  ) +
  
  # Birds
  ggplot2::geom_point(
    data = birdData,
    mapping = ggplot2::aes(x = x, y = y, group = birdId, shape = shape),
    color = "#404040",
    size = 2.5#,
    #shape = 18#17
  ) +
  # Using birds "shape" to give the intuition of flapping wings:
  ggplot2::scale_shape_manual(values = c("hold" = 18, "flap!" = 17)) +
   
  # Removing plot axes, panel background, legend
  ggplot2::coord_equal() +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "NA") +
  
  # Animation:
  gganimate::transition_time(frame) +
  gganimate::ease_aes("linear")



# Rendering and saving
animation <- gganimate::animate(
  plot = animatedPlot, 
  nframes = timeSteps, 
  fps = 15, 
  width = gridSize * 2, 
  height = gridSize * 2, 
  renderer = gganimate::gifski_renderer()
)
gganimate::anim_save("Simplex birds via ggplot2.gif", animation = animation)