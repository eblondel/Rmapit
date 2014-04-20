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
    						pos = "c", offset = 0.1, pars = NULL){
	
	old.par <- par(no.readonly = TRUE)
	
	toRelativeCoords <- function(xy){
		rel.x <- (xy$x - usr[1]) / (usr[2] - usr[1]) * (plt[2] - plt[1])
		rel.y <- (xy$y - usr[3]) / (usr[4] - usr[3]) * (plt[4] - plt[3])
		rel.xy <- xy.coords(rel.x, rel.y)
		return(rel.xy)
	}
	
	#not used for now
	toUserCoords <- function(xy){
		usr.x <- xy$x / (plt[2] - plt[1]) * (usr[2] - usr[1]) + usr[1]
		usr.y <- xy$y / (plt[4] - plt[3]) * (usr[4] - usr[3]) + usr[3]
		user.xy <- xy.coords(usr.x, usr.y)
		return(user.xy)
	}
	
	#position
	pos <- tolower(pos)
	ppos <- c("tl", "t", "tr", "l", "c", "r", "bl", "b", "br")
	nb <- nrow(sp@data) / length(pos)
	if(is.integer(nb)){
		pos <- rep(pos, nb)
	}else{
		pos <- rep(pos, floor(nb + 1))
	}
	
	plt <- par("plt")	
	offset.x <- (plt[2] - plt[1]) * offset
	offset.y <- (plt[4] - plt[3]) * offset	
	usr <- par("usr")
	
	plot.locations <- data.frame(xmin = numeric(0), xmax = numeric(0),
								 ymin = numeric(0), ymax = numeric(0))
	
	#calculate plot relative coordinates (with possible overlaps)
	for(i in 1:nrow(sp@data)){
		subsp <- sp[sp@data[,sp.ref] == sp@data[i,sp.ref],]
		coords <- coordinates(subsp)
		xy <- xy.coords(coords[1], coords[2])
		rel <- toRelativeCoords(xy)
		x1 <- rel$x  + plt[1]
		y1 <- rel$y  + plt[3]
		
		graph.pos <- pos[i]
		if(any(c("b","c","t") == graph.pos)) x1 <- x1 - (0.5 + offset) * ratio
		if(any(c("tl","l","bl") == graph.pos)) x1 <- x1 - (1 + offset) * ratio
		if(any(c("tr","r","br") == graph.pos)) x1 <- x1 + offset * ratio
		if(any(c("l","c","r") == graph.pos)) y1 <- y1 - (0.5 + offset) * ratio
		if(any(c("bl","b","br") == graph.pos)) y1 <- y1 - (1 + offset) * ratio
		if(any(c("tl","t","tr") == graph.pos)) y1 <- y1	+ offset * ratio
		
		#x2/y2 calculations
		x2 <- x1 + (1 + offset) * ratio
		y2 <- y1 + (1 + offset) * ratio
		
		#calculate relative internal plot margins
		x2prim <- (x2 - x1) * plt[2] / (plt[2] - plt[1])
		x1prim <- x2prim - (x2 - x1)
		y2prim <- (y2 - y1) * plt[4] / (plt[4] - plt[3])
		y1prim <- y2prim - (y2 - y1)
		
		#min plt adjustments
		graph.range.x <- x2 - x1 + x1prim + x2prim
		if(x1 - graph.range.x < plt[1]){
			x1 <- x1 + (plt[1] - graph.range.x)
		}
		x2 <- x1 + (1 + offset) * ratio
		
		graph.range.y <- y2 - y1 + y1prim + y2prim
		if(y1 - graph.range.y < plt[3]){
			y1 <- y1 + (plt[3] - graph.range.y)
		}
		y2 <- y1 + (1 + offset) * ratio
		
		#max plt adjustments
		if(x2 > plt[2]){
			x2 <- plt[2]
			x1 <- x2 - ratio
		}
		if(y2 > plt[4]){
			y2 <- plt[4]
			y1 <- y2 - ratio
		}
		
		xmin <- x1 - x1prim
		xmax <- x2 + x2prim
		ymin <- y1 - y1prim
		ymax <- y2 + y2prim
		plot.locations <- rbind(plot.locations, c(xmin, xmax, ymin, ymax))
	}
	plot.locations <- cbind(sp@data[,sp.ref], plot.locations)
	colnames(plot.locations) <- c(sp.ref, "xmin", "xmax", "ymin", "ymax")
	
	#proceed to the plot drawing
	for(i in 1:nrow(plot.locations)){	
		substat <- switch(class(stat),
			"data.frame" = stat[as.character(stat[,stat.ref]) == as.character(subsp@data[1,sp.ref]),],
			"list" = if(!is.null(names(stat))) stat[[as.character(subsp@data[1,sp.ref])]] else stat[[1]]
		)	
		if(nrow(substat) > 0){
			par(pars)
			par(plt = c(plot.locations[i,"xmin"], plot.locations[i,"xmax"],
						plot.locations[i,"ymin"], plot.locations[i, "ymax"]),
				new = TRUE)
			plot.new()
			bgcol <- "transparent"
			if(!is.null(pars$bg)) bgcol <- pars$bg
			polygon(c(0, 1, 1, 0), c(0, 0, 1, 1),  border = NA, col = bgcol)
			par(plt = c(plot.locations[i,"xmin"], plot.locations[i,"xmax"],
						plot.locations[i,"ymin"], plot.locations[i, "ymax"]),
					new = TRUE)
			stat.handler(substat)
			par(plt = plt)
			par(old.par)
		}
	}
	return(invisible(match.call()))
}




