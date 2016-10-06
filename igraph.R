library(igraph)
library(dplyr)
library(gtools)

g <- make_graph(edges = c('1','2', '2','3', '2','4', '4','1'), directed = TRUE)
plot(g)
vcount(g)
ecount(g)
neighbors(g, '1', mode = 'all')
adj_matrix <- get.adjacency(g)
#read_graph()
actors <- data.frame(name  = c("Alice", "Bob", "Cecil", "David","Esmeralda"),
                     age   = c(48,33,45,34,21),
                     gender= c("F","M","F","M","F"))

relations <- data.frame(from       = c("Bob", "Cecil", "Cecil", "David", "David", "Esmeralda"),
                        to         = c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept  = c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship = c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))

g2 <- graph_from_data_frame(relations, directed = FALSE, vertices = actors)
plot(g2)


graph_data <- read.csv('BK User ID - Content July 16.csv')
graph_data$Make_Model <- paste(graph_data$AT...Make, graph_data$AT...Model)
sample_data <- graph_data[graph_data$AT...User.ID.Cookie == "0f04a242-a912-bc7f-e3ad-c2673dfdc93c", ]
sample_data <- data.frame(lapply(sample_data, as.character), stringsAsFactors = FALSE)
sample_data[nrow(sample_data) + 1, ] <- c('20160725', 'bentley', 'bentayga', 'abc-123', 1, 'bentley bentayga')
sample_data[nrow(sample_data) + 1, ] <- c('20160725', 'bentley', 'mulsanne', 'abc-123', 1, 'bentley mulsanne')


sample <- data.frame(from = c('bentayga', 'bentayga', 'bentayga', 'bentayga', 'mulsanne', 'mulsanne', 'm2'),
                     to   = c('mulsanne', 'mulsanne', 'm2'      , 'megane'  , 'm2'      , 'megane'  , 'megane'))

# sample_g <- graph_from_data_frame(sample, directed = FALSE)
# E(sample_g)$weight <- 1
# simple_g <- simplify(sample_g, edge.attr.comb = list(weight = "sum"))
# E(simple_g)$width <- E(simple_g)$weight
# plot(simple_g)

df <- sample_data %>%
  group_by(AT...User.ID.Cookie) %>%
  do(expand.grid(from = .$Make_Model, to = .$Make_Model)) %>%
  apply(1, sort) %>%
  t() %>% 
  data.frame(stringsAsFactors = FALSE) %>%
  .[!duplicated(.), ] %>%
  filter(X2 != X3)

sample_g <- graph_from_data_frame(df[,-1], directed = FALSE)
E(sample_g)$weight <- 1
simple_g <- simplify(sample_g, edge.attr.comb = list(weight = "sum"))
E(simple_g)$width <- E(simple_g)$weight
plot(sample_g)
