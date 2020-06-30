####################################
#### taken from ggnetworkmap example
####################################


invisible(lapply(c("ggplot2", "maps", "network", "sna"), base::library, character.only = TRUE))

## Example showing great circles on a simple map of the USA
## http://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/
airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header = TRUE)
rownames(airports) <- airports$iata

# select some random flights
set.seed(1234)
flights <- data.frame(
  origin = sample(airports[200:400, ]$iata, 200, replace = TRUE),
  destination = sample(airports[200:400, ]$iata, 200, replace = TRUE)
)

# convert to network
flights <- network(flights, directed = TRUE)

# add geographic coordinates
flights %v% "lat" <- airports[ network.vertex.names(flights), "lat" ]
flights %v% "lon" <- airports[ network.vertex.names(flights), "long" ]

# drop isolated airports
delete.vertices(flights, which(degree(flights) < 2))

# compute degree centrality
flights %v% "degree" <- degree(flights, gmode = "digraph")

# add random groups
flights %v% "mygroup" <- sample(letters[1:4], network.size(flights), replace = TRUE)

# create a map of the USA
usa <- ggplot(map_data("usa"), aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2)







####################################################
##############        plot code      ###############
####################################################

# get points
plotcord = data.frame(
  lon = as.numeric(flights %v% "lon"),
  lat = as.numeric(flights %v% "lat")
)

# get edges
edges <- network::as.matrix.network.edgelist(flights)
edges_mat <- data.frame(
  x = plotcord$lon[edges[,1]],
  xend = plotcord$lon[edges[,2]],
  y = plotcord$lat[edges[,1]],
  yend = plotcord$lat[edges[,2]]
)

# make plot with usa in background
usa + 
  geom_point(data = plotcord, mapping = aes(x = lon, y = lat)) +
  geom_curve(
    data = edges_mat, 
    mapping = aes(x = x, xend = xend, y = y, yend = yend), 
    arrow = arrow(length = unit(0.03, "npc"))
  )







