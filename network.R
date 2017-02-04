library(statnet)
library(data.table)

# Import data
pp = read.csv("./data/person-person-connections.csv", header=T,
              stringsAsFactors=F, colClasses=rep("character", 4))
po = read.csv("./data/person-org-connections.csv", header=T,
              stringsAsFactors=F, colClasses=rep("character", 4))
oo = read.csv("./data/org-org-connections.csv", header=T,
           stringsAsFactors=F, colClasses=rep("character", 4))

# Convert datasets to individual edgelists
pp.n = network(pp[,1:2], matrix.type="edgelist", directed=F)
po.n = network(po[,1:2], matrix.type="edgelist", directed=F)
oo.n = network(oo[,1:2], matrix.type="edgelist", directed=F)

# Merged edgelist
all = rbindlist(list(pp, po, oo))
all = cbind(all, type=c(rep("person", nrow(pp) * 2), 
                         rep("org", nrow(po)), rep("person", nrow(po)),
                         rep("org", nrow(oo) * 2)))
all.n = network(all[,1:2], matrix.type="edgelist", directed=F)

# Add vertex attributes
info = data.frame(vertex.names = all.n %v% "vertex.names", stringsAsFactors=F)
for (i in 1:length(info$vertex.names)) {
    if (info$vertex.names[i] %in% pp[,1] |
        info$vertex.names[i] %in% pp[,2] |
        info$vertex.names[i] %in% po[,2]) {
        info$type[i] = "person"
        info$color[i] = "#4477AA"
    } else {
        if (info$vertex.names[i] %in% po[,1] |
            info$vertex.names[i] %in% oo[,1] |
            info$vertex.names[i] %in% oo[,2]) {
            info$type[i] = "org"
            info$color[i] = "#CC6677"
        } else {
        info$type[i] = NA
        info$color[i] = NA
        }
    }
}
all.n %v% "type" = info$type
all.n %v% "type" = info$color

windows()
gplot(all.n, gmode="graph", vertex.col=c("blue", "red"))

windows()
gplot(all.n, gmode="graph", vertex.col=c("blue", "red"), mode="kamadakawai")

windows()
gplot(all.n, gmode="graph", vertex.col=c("blue", "red"), mode="target")

pdf()
dev.off()
