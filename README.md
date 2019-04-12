# Chord Diagram Visualization
This tutorial is an expansion on how to create legends on the complex chord diagram, the manual of circlize package on R can be found [here](https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html). Again, this tutorial will be very similar to what chapter 13-15 cover in the manual, but it doesn't have a thorough detail on how to create legends of the diagram shown below, as I found very frustrated. I also notice people are going through the same stuggle as much as I do, mostly just don't know how to add the color to the color grid and add the corresponding number into the grid, so I thought I'd share the script. 
![alt text](https://jokergoo.github.io/circlize_book/book/15-a-complex-example-with-chord-diagram_files/figure-html/unnamed-chunk-13-1.png)
# Instructions
The R script is uploaded to the repository, but you are welcomed to go through it in readme.md. Still learning how to perfect the diagram so comeback for updates. And if you know how to do this kind of visualization in Shiny, please point me to the right direction, thanks in advance.

```R
#install.packages("randomcoloR")
library(randomcoloR)
#install.packages("circlize")
library(circlize)
# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap", version = "3.8")
library(ComplexHeatmap)
library(grid)
library(gridBase)
library(circlize)
library(chorddiag)
```
These are the packages that I used, ComplexHeatmap has to be download through the commented lines since you can't find it in R packages. The other ones just have to install if you haven't already. Last package chorddiag actually allows to do hoverover and show information on links, however, the color isn't as nice, or organized by group (ALSO IF YOU KNOW HOW TO EMBED THE HTMLWIDGET INTO THE MD PLEASE LET ME KNOW, THANKS)
```R
PI <- paste0(sample(LETTERS[1:10]),sample(LETTERS[1:10]),sample(LETTERS[1:10]))
PI_department <- sample(rep(c("EN-CS","EN-CE","EN-ME","AS-Stat","AS-DS","MD-Neuroradiology","MD-Oncology"),3))[1:length(PI)]
coPI_department <- sample(PI_department)[1:length(PI)]
coPI <- sample(c(paste0(sample(LETTERS[1:10]),sample(LETTERS[1:10]),sample(LETTERS[1:10])),PI))[1:length(PI)]
Total_Cost <- rnorm(10,100000,20000)
Status <- sample(rep(c("Accepted","Pending","Declined"),c(5,3,2)))
Status

df <- data.frame(PI, PI_department,coPI_department,coPI,Total_Cost,Status)

unique(df$PI)
unique(df$coPI)

unique(c(df$PI,df$coPI))
# IMPORTANT
# Make sure people from the same department are adjacent!
PI <- df[,c(1,2)]
colnames(PI) <- c("lastname","department")
coPI <- df[,c(4,3)]
colnames(coPI) <- c("lastname","department")
df.list <- rbind(PI,coPI)
df.list <- df.list[which(duplicated(df.list$lastname)=="FALSE"),]

sort(unique(df.list$department))

# create a grid.col.name to use as the matrix column and row names
# ORDER MATTERS
#randomly generate the colors for the sector
legend_col <- distinctColorPalette(k = length(unique(df.list$department)), altCol = FALSE, runTsne = FALSE)
legend_col

# automate school list and the PIs in the school
school <- unique(substr(df.list$department,1,2))
df.list$lastname[which(substr(df.list$department,1,2)==school[1])]
col = distinctColorPalette(k = length(school), altCol = FALSE, runTsne = FALSE)

leg_match <- data.frame(cbind(legend_col,levels(df.list$department)))
names(leg_match) <- c("color", "department")
#assign colors to PIs depending on their department
pi_match <- merge(df.list, leg_match, by.x = "department", by.y = "department")[-1]

#create a list of names to use as matrix rownames and colnames
grid.col.names <- as.character(pi_match[, 1])
grid.col <- as.character(pi_match[, 2])

#get a list of unique departments
dept <- sort(unique(df.list$department),decreasing = FALSE)
department = substring(dept,4)

#create a matrix to get the count
mat <- matrix(0,length(grid.col.names),length(grid.col.names))
rownames(mat) <- grid.col.names
colnames(mat) <- grid.col.names

mat_dollar <- data.frame(mat)
mat_count <- data.frame(mat)

# By total cost
for (i in 1:nrow(mat)){
  for (j in 1:ncol(mat)){
    for (n in 1:nrow(df)){
      if ((df[n,1]==rownames(mat)[i] & df[n,4]==colnames(mat)[j])==TRUE){
        mat_dollar[i,j] = mat_dollar[i,j]+df[n,5]
       }
    }
  }
}

# By proposal count
for (i in 1:nrow(mat)){ #aka in PI
  for (j in 1:ncol(mat)){ #aka in coPI
    for (n in 1:nrow(df)){
      if ((df[n,1]==rownames(mat)[i] & df[n,4]==colnames(mat)[j])==TRUE){
        mat[i,j] = mat[i,j]+1
      }
    }
  }
}

mat_dollar <- as.matrix(mat_dollar)
mat_count <- as.matrix(mat_count)

chorddiag(mat_dollar, type = "directional", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
chorddiag(mat_count, type = "directional", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
```

The last two lines is using chorddiag function to create a interactive visualization, and like I said, it's a trade-off of interaction and UI, but it's good to know it's out there. The chunk above is creating a matrix, which is the underlying dataframe for the plot (If you don't know how to do this, check the circlized manual, it's very helpful). Now we are about to plot the diagram.
```R
# This is the plot for Total Cost (Dollar)
circos.initializeWithIdeogram(plotType = NULL)
circlize_plot_dollar = function() {
  #circos.initialize(NULL)
  chordDiagram(mat_dollar, annotationTrack = c("grid", "axis"),
               grid.col = grid.col,
               preAllocateTracks = list(
                 track.height = uh(4, "mm"),
                 track.margin = c(uh(4, "mm"), 0)),
               link.sort = TRUE, 
               link.decreasing = TRUE,directional = 1, 
               direction.type = c("diffHeight", "arrows"),
               link.arr.type = "big.arrow")
  title("Proposed Total Cost", cex = 0.8)

  circos.track(track.index = 2, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, 
                CELL_META$ycenter, 
                CELL_META$sector.numeric.index, 
                cex = 0.7, 
                col="white", 
                niceFacing = TRUE)}, 
    bg.border = NA)
  

    for (i in 1:length(school)){
    highlight.sector(df.list$lastname[which(substr(df.list$department,1,2)==school[i])],
                   track.index = 1, col = col[i], 
                   text = school[i], 
                   cex = 0.8, 
                   text.col = "white", 
                   niceFacing = TRUE)
    }

  circos.clear()
}

circlize_plot_dollar()

lgd_list = Legend(labels = grid.col.names, 
                      title = "PI", 
                      type = "points", 
                      pch = paste0(c(1:nrow(mat_count)),""),
                      legend_gp = gpar(col = "white"), 
                      background = grid.col)


lgd_col = Legend(labels = department,
                 title = "Department",
                 type = "point",
                 background = unique(grid.col))

#unique(pi_match$color)
#legend_col
#grid.col

#pd = packLegend(lgd_list, lgd_col,direction = "horizontal")


plot.new()
circle_size = unit(1, "snpc") # snpc unit gives you a square region

pushViewport(viewport(x = 0, y = 0.5, width = circle_size, height = circle_size,
    just = c("left", "center")))
par(omi = gridOMI(), new = TRUE)
circlize_plot_dollar()
upViewport()

#"just" indicates the sides that align with x and y
pushViewport(viewport(x = 0.78, y = 0.9, width = grobWidth(lgd_list), 
    height = grobHeight(lgd_list), just = c("left", "top")))
grid.draw(lgd_list)
upViewport()

pushViewport(viewport(x = 0.78, y = 0.3, width = grobWidth(lgd_col), 
    height = grobHeight(lgd_col), just = c("left", "top")))
grid.draw(lgd_col)
upViewport()
```

```R circlize_plot_dollar()``` will give the plot with no legend

![alt text](https://github.com/timxymo/Chord-Diagram-Visualization/blob/master/no%20legend.png?raw=true)

and the chunk from ```R plot.new()``` and below will display the plot with legend. Note that the legend position will be funky, you might want to adjust that in your view pane.

![alt text](https://github.com/timxymo/Chord-Diagram-Visualization/blob/master/with%20legend.png?raw=true)

There you have it! You can also add another legend for the ring color (if you're reading this, it should be pretty easy for you now). If you need higher res plot, just run the code in Console, and download the plot as JPEG or pdf.
GOOD LUCK! 
