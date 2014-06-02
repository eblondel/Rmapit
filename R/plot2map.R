# Rmapit - plot2map.R		
# Copyright (C) 2014 - Emmanuel Blondel
#
# This program is free software; you can redistribute it and/or modify it under 
# the terms of the GNU General Public License as published by the Free Software 
# Foundation; either version 2 of the License, or (at your option) any later 
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with 
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
# Contact: emmanuel.blondel1 (at) gmail.com

plot2map <- function(sp, sp.ref, stat, stat.ref, stat.handler, ratio = 0.05,
                     pos = "c", offset = 0, pars = NULL, box.lty = "solid",
                     box.col = "transparent"){
  
  old.par <- par(no.readonly = TRUE)
  
  #graphical params used in the function
  plt <- par("plt")
  fig <- par("fig")
  
  #define the order of indexing (bycol vs. byrow) will be needed to be reset
  #multiplot pars after adding each graph inset (fig use deactivates multiplot)
  mfrow <- par("mfrow")
  mfcol <- par("mfcol")
  mfg <- par("mfg")
  
  #position
  pos <- tolower(pos)
  ppos <- c("tl", "t", "tr", "l", "c", "r", "bl", "b", "br")
  nb <- nrow(sp@data) / length(pos)
  if(is.integer(nb)){
    pos <- rep(pos, nb)
  }else{
    pos <- rep(pos, floor(nb + 1))
  }
  
  #calculate plot relative coordinates (with possible overlaps)
  plot.locations <- data.frame(xmin = numeric(0), xmax = numeric(0),
                               ymin = numeric(0), ymax = numeric(0))
  for(i in 1:nrow(sp@data)){
    subsp <- sp[sp@data[,sp.ref] == sp@data[i,sp.ref],]
    coords <- coordinates(subsp)
    x1 <- grconvertX(coords[1], from = "user", to = "nic")
    y1 <- grconvertY(coords[2], from = "user", to = "nic")
    
    graph.pos <- pos[i]
    if(graph.pos == "c"){
      x1 <- x1 - 0.5 * ratio
      y1 <- y1 - 0.5 * ratio
    }
    if(any(c("b","t") == graph.pos)) x1 <- x1 - (0.5 + offset) * ratio
    if(any(c("tl","l","bl") == graph.pos)) x1 <- x1 - (1 + offset) * ratio
    if(any(c("tr","r","br") == graph.pos)) x1 <- x1 + offset * ratio
    if(any(c("l","r") == graph.pos)) y1 <- y1 - (0.5 + offset) * ratio
    if(any(c("bl","b","br") == graph.pos)) y1 <- y1 - (1 + offset) * ratio
    if(any(c("tl","t","tr") == graph.pos)) y1 <- y1	+ offset * ratio
    
    #x2/y2 calculations
    x2 <- x1 + ratio
    if(graph.pos != "c") x2 <- x2 + offset * ratio
    y2 <- y1 + ratio
    if(graph.pos != "c") y2 <- y2 + offset * ratio
    
    #calculate relative internal plot margins
    if(i == 1){
      x2prim <- (x2 - x1) * fig[2] / (fig[2] - fig[1])
      x1prim <- x2prim - (x2 - x1)
      y2prim <- (y2 - y1) * fig[4] / (fig[4] - fig[3])
      y1prim <- y2prim - (y2 - y1)
      plot.margins <- data.frame(xmin = x1prim, xmax = x2prim,
                                 ymin = y1prim, ymax = y2prim)
    }
    
    #min fig adjustments
    graph.range.x <- x2 - x1 + plot.margins$xmin + plot.margins$xmax
    if(x2 + plot.margins$xmax - graph.range.x < fig[1]){
      x1 <- fig[1] + plot.margins$xmin
    }
    if(x1 <= fig[1]){
      x1 <- x1 + fig[1] + plot.margins$xmin
    }
    x2 <- x1 + ratio
    if(graph.pos != "c") x2 <- x2 + offset * ratio
    
    graph.range.y <- y2 - y1 + plot.margins$ymin + plot.margins$ymax
    if(y2 + plot.margins$ymax - graph.range.y < fig[3]){
      y1 <- fig[3] + plot.margins$ymin
    }
    if(y1 <= fig[3]){
      y1 <- y1 + fig[3] + plot.margins$ymin
    }
    y2 <- y1 + ratio
    if(graph.pos != "c") y2 <- y2 + offset * ratio
    
    #max fig adjustments
    if(x2 > fig[2]){
      x2 <- fig[2]
      x1 <- x2 - ratio
      if(graph.pos != "c") x1 <- x1 - offset * ratio
    }
    if(y2 > fig[4]){
      y2 <- fig[4]
      y1 <- y2 - ratio
      if(graph.pos != "c") y1 <- y1 - offset * ratio
    }
    
    plot.locations <- rbind(plot.locations, c(x1, x2, y1, y2))
  }
  plot.locations <- cbind(sp@data[,sp.ref], plot.locations)
  colnames(plot.locations) <- c(sp.ref, "xmin", "xmax", "ymin", "ymax")
  
  #proceed to the plot drawing
  if(!is.null(pars)) par(pars)
  for(i in 1:nrow(plot.locations)){	
    
    subsp <- sp[sp@data[,sp.ref] == sp@data[i,sp.ref],]
    
    #handle the stat subset
    substat <- switch(class(stat),
                      "data.frame" = stat[as.character(stat[,stat.ref]) ==
                                          as.character(subsp@data[1,sp.ref]),],
                      "list" = if(!is.null(names(stat)))
                                  t[[as.character(subsp@data[1,sp.ref])]]
                               else
                                  t[[1]]
    )
    
    if(nrow(substat) > 0){
      
      #approach using fig and layout allows to attach graphics to map
      graph.fig <- par("fig")
      print(graph.fig)
      graph.coords <- plot.locations[i,]
      op = par(plt = plt, fig = graph.fig, new=T)		
      
      graph.margins <- c(grconvertX(par("mai")[c(2,4)], "inches", "ndc"),
                         grconvertY(par("mai")[c(1,3)], "inches", "ndc"))
      graph.bounds <- c(graph.fig[1], 1 - graph.fig[2],
                        graph.fig[3], 1 - graph.fig[4])
      
      #calculate relative layouts
      layout.respect =TRUE
      layout.width <- c(graph.coords$xmin,
                        graph.coords$xmax - graph.coords$xmin,
                        1 - graph.coords$xmax)
      
      layout.height <- c(1 - graph.coords$ymax,
                         graph.coords$ymax - graph.coords$ymin,
                         graph.coords$ymin)
      print("===")
      print(layout.width)
      print(layout.height)
      
      layout(
        matrix(
          data = c(0,0,0,
                   0,1,0,
                   0,0,0),
          nrow = 3,
          ncol = 3,
          byrow = TRUE
        ),
        width = layout.width,
        height = layout.height,
        layout.respect
      )
      
      #add graph inset
      stat.handler(substat)
      box("figure", lty = box.lty, col = box.col)
      par(op)	
    }
  }
  
  #reset the initial pars (including eventual multiplot sequence)
  #TODO investigate how to deal with the order bycol vs. byrow
  #(seems byrow is always reset)
  par(old.par) 
  par(mfg = mfg, new = F)
  
  #return(invisible(match.call()))
  return(list(locations = plot.locations, margins = plot.margins))
}
