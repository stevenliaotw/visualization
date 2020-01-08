######################################################
### Visualizing Global Migration Flows, 1960-2000
######################################################
library(igraph)
library(plyr)
library(gdata)

# loading Global Migration Flows data from 
# Abel, Guy J. 2013. “Estimating Global Migration Flow Tables Using Place of Birth Data.” Demographic Research 28(March): 505–46.
# Data can be downloaded here: http://www.demographic-research.org/volumes/vol28/18/files/flows.xlsx
# xlsx file includes 5 sheets: sheet 1 offers notes, sheet 2-5 offers bilateral flows data for 1960s, 1970s, 1980s, and 1990s respectively.
m.flows.raw <- NULL
for (i in 2:5) { 
  m.flows.raw[[i-1]] <- read.xls("http://www.demographic-research.org/volumes/vol28/18/files/flows.xlsx", sheet = i, method = "csv")
}

# define and apply function to clean up data and make list of matrices
makeMatrix <- function(x){
  x$X <- NULL
  x$TOTAL <- NULL
  x <- x[1:191,]   
  x <- as.matrix(x)
}
m.flows.matrix <- llply(m.flows.raw, makeMatrix)

# convert matrices to igraph objects
g.list <- llply(m.flows.matrix, function(x) graph.adjacency(x, weighted = TRUE, mode = "undirected"))

# check data for 1960s
summary(g.list[[1]])

# plot separate network graphs as pdfs for selected countries, 1960-2000
for (i in 1:4) {
  # select certain countries
  cty.list <- c("AUS", "CHN", "MAC", "HKG", "IND", "IDN", "USA", "MEX", "TWN", "THA", "VNM")
  # or draw 30 random countries from each dataset (may be too messy)
  #vs <- sample.int(vcount(m.flows.matrix[[1]]), 30) - 1
  
  # make list of years
  year.list <- c("1960s", "1970s", "1980s", "1990s")
  
  # find column numbers based on selected countries
  vs <- match(cty.list, colnames(m.flows.matrix[[i]]))
  
  # subset network to be graphed
  g <- induced.subgraph(g.list[[i]], vs)
  
  # style based on Gabriel J. Michael's visualization of Trans-Pacific Partnership (TPP) agreements. Thanks, Gabriel!
  # see http://topromotetheprogress.wordpress.com/2013/11/17/visualizing-negotiating-positions-in-the-tpp-ip-chapter/
  # set edge attributes
  E(g)$width <- rank(E(g)$weight)/7
  E(g)$curved <- 0.2
  # set color palette
  colors <- heat.colors(max(rank(E(g)$weight)/7) + 2)
  # reverse color order
  colors <- rev(colors)
  E(g)$color <- colors[round(E(g)$width) + 1]
  
  # set vertice attributes
  V(g)$label.cex <- 0.8
  V(g)$label.family <- "sans"
  V(g)$label.color <- "white"
  V(g)$color <- rgb(8, 104, 172, maxColorValue = 255)
  V(g)$frame.color <- rgb(8, 104, 172, maxColorValue = 255)
  
  # alternative styles
  #egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
  #E(g)$color <- rgb(.5, .5, 0, egam)
  #E(g)$width <- egam
  
  # plot
  pdf(paste("bilateral flows_", year.list[i], ".pdf", sep = ""), height = 7, width = 7)
  plot(g, layout = layout.circle)
  title(main = paste("Weighted Network Graph of Bilateral Migration Flows", year.list[i], sep = ", "))
  # alternative plots
  #plot(g, layout = layout.fruchterman.reingold(g, niter = 1000, 
  #                                             area = 50*vcount(g)^2.8, 
  #                                             repulserad = 50*vcount(g)^3))
  dev.off()
}


# plot network graphs as animation in GIF
# relies on the R package animation 
# make sure ImageMagick has been installed in your system
library(animation)

# set animation options
ani.options(interval = 2, nmax = 4)

# create GIF in working directory
saveGIF({
  for (i in 1:4) {
    # select certain countries
    cty.list <- c("AUS", "CHN", "MAC", "HKG", "IND", "IDN", "USA", "MEX", "TWN", "THA", "VNM")
    # or draw 30 random countries from each dataset 
    #vs <- sample.int(vcount(m.flows.matrix[[1]]), 30) - 1
    
    # make list of years
    year.list <- c("1960s", "1970s", "1980s", "1990s")
    
    # find column numbers based on selected countries
    vs <- match(cty.list, colnames(m.flows.matrix[[i]]))
    
    # subset network to be graphed
    g <- induced.subgraph(g.list[[i]], vs)
    
    # style based on Gabriel J. Michael's visualization of Trans-Pacific Partnership (TPP) agreements. Thanks, Gabriel!
    # see http://topromotetheprogress.wordpress.com/2013/11/17/visualizing-negotiating-positions-in-the-tpp-ip-chapter/
    # set edge attributes
    E(g)$width <- (rank(E(g)$weight)/7)
    E(g)$curved <- 0.2
    # set color palette
    colors <- heat.colors(max(rank(E(g)$weight)/7) + 2)
    # reverse color order
    colors <- rev(colors)
    E(g)$color <- colors[round(E(g)$width) + 1]
    
    # Set vertice attributes
    V(g)$label.cex <- 2
    V(g)$label.family <- "sans"
    V(g)$label.color <- "white"
    V(g)$color <- rgb(8, 104, 172, maxColorValue = 255)
    V(g)$frame.color <- rgb(8, 104, 172, maxColorValue = 255)
    
    # alternative styles
    #egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
    #E(g)$color <- rgb(.5, .5, 0, egam)
    #E(g)$width <- egam
    
    # plot
    plot(g, layout = layout.circle)
    title(main = paste("Weighted Network Graph of Bilateral Migration Flows", year.list[i], sep = ", "), cex.main = 2)}
    # alternative plots
    #plot(g, layout = layout.fruchterman.reingold(g, niter = 1000, 
    #                                              area = 50*vcount(g)^2.8, 
    #                                              repulserad = 50*vcount(g)^3))
  }, 
        movie.name = "bilateral flows, 1960-2000.gif", 
        ani.width = 1500, 
        ani.height = 1500,
        loop = TRUE,
        outdir = getwd())