#	Rmapit - plot2map.R		
#	Copyright (C) 2014 - Emmanuel Blondel
#
#	This program is free software; you can redistribute it and/or modify
#	it under the terms of the GNU General Public License as published by
#	the Free Software Foundation; either version 2 of the License, or
#	(at your option) any later version.

#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.

#	You should have received a copy of the GNU General Public License along
#	with this program; if not, write to the Free Software Foundation, Inc.,
#	51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
#	Contact: emmanuel.blondel1 (at) gmail.com

plot2map <- function(sp, sp.ref, stat, stat.ref, stat.handler, ratio = 0.05,
    	pos = "c", offset = 0, pars = NULL){
	
	old.par <- par(no.readonly = TRUE)
	mfrow <- par("mfrow")
	mfcol <- par("mfcol")
	mfg <- par("mfg")
	#names.par <- names(par())[!names(par()) %in% c("mfrow","mfcol","mfg")]
	#on.exit(par(par(no.readonly = TRUE)[names.par]))
	#on.exit(par(old.par, new = T))
	#on.exit(par(mfrow = mfrow, mfcol = mfcol, mfg = mfg))
	plt <- par("plt")
	
	#switch to fig / layout approach
	fig <- par("fig")
	
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
	plot.locations <- data.frame(xmin = numeric(0), xmax = numeric(0), ymin = numeric(0), ymax = numeric(0))
	plot.margins <- data.frame()
	for(i in 1:nrow(sp@data)){
		subsp <- sp[sp@data[,sp.ref] == sp@data[i,sp.ref],]
		coords <- coordinates(subsp)
		x1 <- grconvertX(coords[1], from = "user", to = "ndc")
		y1 <- grconvertY(coords[2], from = "user", to = "ndc")
		print(c(x1,y1))
		
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
	
	#function to drawn polygon
	drawPolygon <- function(locations){
		bgcol <- "transparent"
		if(!is.null(pars$bg)) bgcol <- pars$bg
		polygon(
			grconvertX(c(locations$xmin, locations$xmax, locations$xmax, locations$xmin), from = "ndc", to = "nfc"),
			grconvertY(c(locations$ymin, locations$ymin, locations$ymax, locations$ymax), from = "ndc", to = "nfc"),
			border = "black",
			col = bgcol
		)
	}
	
	#proceed to the plot drawing
	if(!is.null(pars)) par(pars)
	for(i in 1:nrow(plot.locations)){	
		substat <- switch(class(stat),
				"data.frame" = stat[as.character(stat[,stat.ref]) == as.character(subsp@data[1,sp.ref]),],
				"list" = if(!is.null(names(stat))) stat[[as.character(subsp@data[1,sp.ref])]] else stat[[1]]
		)	
		if(nrow(substat) > 0){

			#approach using fig and layout allows to attach graphics to map
			graph.fig <- par("fig")
			graph.coords <- plot.locations[i,]
			graph.coords$xmin <- graph.fig[1] + graph.coords$xmin * (graph.fig[2] - graph.fig[1])
			graph.coords$xmax <- graph.fig[1] + graph.coords$xmax * (graph.fig[2] - graph.fig[1])
			graph.coords$ymin <- graph.fig[3] + graph.coords$ymin * (graph.fig[4] - graph.fig[3])
			graph.coords$ymax <- graph.fig[3] + graph.coords$ymax * (graph.fig[4] - graph.fig[3])
			
			op = par(plt = plt, fig = graph.fig, new=T)
			drawPolygon(graph.coords)
			
			op = par(plt = plt, fig = graph.fig, new=T)
			nf <- layout(
					matrix(
						data = c(0,0,0,
						  		 0,1,0,
						  		 0,0,0),
	  					nrow = 3,
						ncol = 3,
						byrow = TRUE
					),
					width = c(graph.coords$xmin, graph.coords$xmax - graph.coords$xmin, 1 - graph.coords$xmax),
					height = c(1 - graph.coords$ymax, graph.coords$ymax - graph.coords$ymin, graph.coords$ymin),
					TRUE
				 )
			stat.handler(substat)
			par(op)
			
		}
	}

	return(invisible(match.call()))
	#return(list(locations = plot.locations, margins = plot.margins))
}
