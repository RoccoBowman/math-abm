library(tidyverse)
library(igraph)

# creating object list

object_list <- c(1:10)

# generating transition probability matrix

rand.sum <- function(n){
  x <- sort(runif(n-1))
  c(x,1) - c(0,x)
}

p_matrix <- t(replicate(10,rand.sum(10)))

# model run

n_t <- 100
created_objects <- vector()
all_objects <- tibble()
new_object <- 1

for (i in 1:n_t){
  last_object <- new_object
  new_object <- sample(object_list, 1, prob = p_matrix[last_object,])
  if (new_object %in% created_objects) {
    event <- "attention"
  } else {
    created_objects <- c(created_objects, new_object)
    event <- "intro"
  }
  add <- tibble(t = i, last = last_object, new = new_object, event = event)
  all_objects <- bind_rows(all_objects, add)
}


initial_graph <- graph_from_data_frame(all_objects[,2:3],
                                       directed = TRUE)

graph_summary <- all_objects %>% 
  filter(last > 0) %>% 
  group_by(last, new) %>% 
  summarize(weight = n()/2)

initial_graph <- graph_from_data_frame(graph_summary[,1:2],
                                       directed = TRUE)

weighted_graph <- set_edge_attr(initial_graph,
                                "weight",
                                value = graph_summary$weight)

plot(weighted_graph)

plot(weighted_graph,
     edge.width=E(weighted_graph)$weight,
     layout=layout.circle)

