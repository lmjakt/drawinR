
## obtain the current user aspect:
usr.asp <- function(){
    with(par(), (pin[1]/pin[2]) / (diff(usr[1:2])/diff(usr[3:4])) )
}

## return the points of an arc centered on x, y, from a.beg to a.end and with a radious
## of r
arcPoints <- function( x, y, r, a.beg, a.end, a.sep=0.01, a.n=(a.end-a.beg)/a.sep, degrees=FALSE){
    if( degrees ){
        a.beg <- pi * a.beg / 180
        a.end <- pi * a.end / 180
    }
    if(a.end < a.beg)
        a.end <- a.end + 2 * pi
        
    angles <- seq(a.beg, a.end, a.sep)
    if( angles[ length(angles) ] != a.end )
        angles <- c(angles, a.end)
    
    a.x <- x + sin( angles ) * r
    a.y <- y + cos( angles ) * r
    pts <- matrix( c(a.x, a.y), ncol=2 )
    colnames(pts) <- c('x', 'y')
    pts
}

## draw an arc, as a line
## centered on x,y, and with radius of r+depth and r-depth..
## the angles should not be less than -pi though, more are possible.. 
lineArc <- function(x, y, r, a.beg, a.end, a.sep=NA, degrees=FALSE, label=NULL, label.col=1, ...){
    ## set the default here... 
    if(is.na(a.sep))
        a.sep = 0.01
    if( degrees ){
        a.beg <- pi * a.beg / 180
        a.end <- pi * a.end / 180
    }
    if(a.beg > a.end)
        a.end <- a.end + 2 * pi
        
    angles <- seq(a.beg, a.end, by=a.sep)
    if(angles[ length(angles) ] != a.end )
        angles <- c(angles, a.end)
    a.x <- x + sin( angles ) * r
    a.y <- y + cos( angles ) * r

    lines( a.x, a.y, ... )

    if(!is.null(label)){
        mid.a <- (a.beg + a.end)/2
        ## but the rotation angle we need for the text needs to be in degrees
        rot.a <- -180 * mid.a / pi
        text( x + sin( mid.a ) * r, y + cos( mid.a ) * r,
              label, srt=rot.a, col=label.col )
    }
}

rad2pi <- function(r){
    180 * r/pi
}

clamp <- function(v, min=-1, max=1){
    v <- ifelse( v < min, min, v )
    ifelse( v > max, max, v )
}

## this wont work with angles that are greater than 2pi..
## we could do that by making a while loop, but that wouldn't
## be compatible with vector arithmetic
internalAngle <- function(a1, a2){
    a1 <- ifelse( a1 > pi, a1 - 2*pi, a1 )
    a2 <- ifelse( a2 > pi, a2 - 2*pi, a2 )
    a <- sort(c(a1, a2))
    mid.angle <- mean(a)
    angle <- a[2] - a[1]
    if( angle > pi){
        angle <- 2*pi - (a[2]-a[1])
        a <- rev(a)
    }
    if(mid.angle < a[1])
        mid.angle <- mid.angle + pi
    return(c('angle'=angle, 'a1'=a[1], 'a2'=a[2], mid=mid.angle ))
}

## x, y, the center of a circle
## r radius of the circle
## the begining and end angles to connect
connectingArc <- function(x, y, r, a1, a2, draw=TRUE, ...){
    o1 <- c(x, y)
    a1.p <- o1 + c( r * sin(a1), r * cos(a1) )
    a2.p <- o1 + c( r * sin(a2), r * cos(a2) )

    ## the internal angle is needed to determine the location
    ## of the connecting circle from which the arc is drawn
    i.a <- internalAngle(a1, a2)  ## c(angle, a1, a2, mid.angle)

    ## the length of the hypotheneuse connecting two tangents
    ## from the two points specified by a1 and a2
    h2 <- r / abs(cos( i.a[1]/2 ))
    r2 <- sqrt( h2^2 - r^2 )
    o2 <- o1 + c(h2 * sin(i.a[4]), h2 * cos(i.a[4]) )

    b1.a = asin( clamp((a1.p - o2)[1] / r2) )
    b1.b = acos( clamp((a1.p - o2)[2] / r2) )
    b1 <- ifelse( b1.a > 0, b1.b, -b1.b )
    
    b2.a = asin( clamp((a2.p - o2)[1] / r2) )
    b2.b = acos( clamp((a2.p - o2)[2] / r2) )
    b2 <- ifelse( b2.a > 0, b2.b, -b2.b )

    b.i <- internalAngle(b1, b2)
    if(draw)
        lineArc( o2[1], o2[2], r2, b.i[2], b.i[3], ... )
    invisible( c('x'=o2[1], 'y'=o2[2], 'r'=r2, 'a.beg'=b.i[2], 'a.end'=b.i[3]) )
}

## draw an arc, as a polygon..
## centered on x,y, and with radius of r+depth and r-depth..
polygArc <- function(x, y, r, depth, a.beg, a.end, a.sep=NA, degrees=FALSE, label=NULL, label.col=1, label.cex=1, ...){
    ## set the default here... 
    if(is.na(a.sep))
        a.sep = 0.01
    if( degrees ){
        a.beg <- pi * a.beg / 180
        a.end <- pi * a.end / 180
    }
    if(a.beg > a.end)
        a.end <- a.end + 2 * pi
        
    angles <- seq(a.beg, a.end, by=a.sep)
    if(angles[length(angles)] != a.end)
        angles <- c(angles, a.end)
    
    a.x <- sin( angles )
    a.y <- cos( angles )
    outer.x <- x + (a.x * (r + depth/2))
    outer.y <- y + (a.y * (r + depth/2))

    inner.x <- x + (a.x * (r - depth/2))
    inner.y <- y + (a.y * (r - depth/2))

    polygon( c(outer.x, rev(inner.x)), c(outer.y, rev(inner.y)), ... )

    if(!is.null(label)){
        mid.a <- (a.beg + a.end)/2

        ## but the rotation angle we need for the text needs to be in degrees
        rot.a <- -180 * mid.a / pi
        text( x + sin( mid.a ) * r, y + cos( mid.a ) * r,
              label, srt=rot.a, col=label.col, cex=label.cex )
    }
}

## the first and last points are taken as anchored points
## the amount of distortion on any point is a function of its distance
## from either of these points along the curve specified by the points
## pts is a matrix containing two columns giving the x and y positions of the
## points
anchoredDistort <- function( pts, dist.v ){
    x.diff <- diff( pts[,1] )
    y.diff <- diff( pts[,2] )
    dists <- sqrt( x.diff^2 + y.diff^2 ) 
    fwd.dist <- c(0, cumsum( dists ))
    rev.dist <- rev(c(0, cumsum( rev(dists) )))
    a.dist <- sqrt( fwd.dist * rev.dist )
    ## and then simply take,,
    pts[,1] <- pts[,1] + dist.v[1] * (a.dist / max(a.dist))
    pts[,2] <- pts[,2] + dist.v[2] * (a.dist / max(a.dist))
    pts
}

## function taken from https://en.wikipedia.org/wiki/B%C3%A9zier_curve
## B(t) = sum{i=0..n}[ choose(n,i) * (1-t)^(n-i) * t^i * P_i
## where P_0 and P_n are the entry and exit points of the curve
##
## express the points as a matrix with two columns x and y
bezier.pts <- function(pts, t=seq(0,1, length.out=30)){
    if(!is.matrix(pts) || ncol(pts) != 2)
        stop("pts should be a matrix with two columns")
    n <- nrow(pts) - 1
    i <- 0:n
    bin.c <- choose(n, i)
    t(sapply(t, function(tt){
        colSums( bin.c * (1-tt)^(n-i) * tt^i * pts )
    }))
}

## if w is less than one, then it will be considered as a proprotion of the
## total width
tube <- function(pts, w, is.prop=w < 1){
    if(is.prop)
        w <- with(par(), (usr[2] - usr[1]) * w)
    w <- w/2
    asp <- with(par(), (pin[1]/pin[2]) / (diff(usr[1:2])/diff(usr[3:4])) )
    nr <- nrow(pts)
    get.delta <- function(p){
        dl <- nr - 1
        d <- diff(p)
        dd <- c(d[1], (d[-dl] + d[-1]) / 2, d[dl])
    }
    dx <- get.delta(pts[,1]) * asp
    dy <- get.delta(pts[,2])
    ## normalise dx and dy to unit distances:
    dss <- (dx^2 + dy^2)
    dx.sign <- ifelse(dx < 0, -1, 1)
    dy.sign <- ifelse(dy > 0, -1, 1)
    dx <- sqrt( (dx^2) / dss ) * dx.sign
    dy <- sqrt( (dy^2) / dss ) * dy.sign
    ## normals have opposing dx and dy
    rbind( pts + cbind(dy, dx*asp) * w,
    (pts - cbind(dy, dx*asp) * w)[nr:1,] )
}
    
## w = width in terms of proportion of width of plotting surface.
bezier.tube <- function(pts, w, t=seq(0,1, length.out=30)){
    w <- with(par(), w * (usr[2]-usr[1]))
    b.pts <- bezier.pts( pts, t )
    tube(b.pts, w)
}

    
## coordinates are in terms of the proportion of
## the width and height of the plotting area
## returns the points for an upward poiting arrow head. This can then be rotated to
## the appropriate angle
arrow.head <- function(height=0.05, width=NA, b.width=NA, b.height=height/3, i.h=1/3, i.w=0, t.n=30){
    x.sc <- with(par(), usr[2]-usr[1])
    y.sc <- with(par(), usr[4]-usr[3])
    asp <- with(par(), pin[1] / pin[2])
    if(is.na(width))
        width <- 0.66 * height / asp
    if(is.na(b.width))
        b.width <- width / 4
    x1 <- c(-width/2, -b.width/2, 0) * x.sc
    x2 <- -1 * rev(x1)
##    x2 <- c(0, b.width/2, width/2) * x.sc
    y1 <- (-height/2 + c(0, b.height, height)) * y.sc
    y2 <- rev(y1)
    y1.i <- (-height/2 + c(0, b.height, i.h*height)) * y.sc
    x1.i <- c(-width/2, i.w * -b.width/2, 0) * x.sc
    x2.i <- -1 * x1.i
    ##    y2 <- (height/2 - c(0, (height-b.height), height)) * y.sc
    t <- seq(0,1, length.out=t.n)
    rbind(bezier.pts( cbind(rev(x1.i), rev(y1.i)), t),
          bezier.pts( cbind(x1, y1), t),
          bezier.pts( cbind(x2, y2), t),
          bezier.pts( cbind(x2.i, y1.i), t)
          )
}

## returns an arrow head drawn in the direction given by the last two points of the pts
arrow.end <- function(pts, ...){
    h.pts <- arrow.head(...)
    ## translate the arrowhead so that the tip of the arrow is at 0,0
    ## around which it will be rotated.
    h.pts[,2] <- h.pts[,2] - max(h.pts[,2])
    nr <- nrow(pts)
    d <- pts[nr,] - pts[nr-1,]
    asp <- with(par(), (pin[1]/pin[2]) / (diff(usr[1:2])/diff(usr[3:4])) )
    l <- sqrt( (asp*d[1])^2 + d[2]^2 )
    a1 <- asin( asp * d[1] / l )
    a2 <- acos( d[2] / l )
    a <- ifelse(a1 > 0, a2, 2 * pi - a2)
    pts.r <- translate.pts(pts, -pts[nr,1], -pts[nr,2])
    pts.r <- rotate.pts(pts.r, -a, origin=c(0,0), preserve.aspect=TRUE)
    ## then select points external to the arrow head
    pts.r <- pts[ 1:max( which( pts.r[,2] < min(h.pts[,2]) )), ]
##    pts.r <- pts[ pts.r[,2] < h.pts[1,2], ]
    h.pts <- rotate.pts(h.pts, a, origin=c(0,0), preserve.aspect=TRUE)
    h.pts <- translate.pts(h.pts, pts[nr,1], pts[nr,2])
    list(l=rbind(pts.r, h.pts[1,]), ah=h.pts)
}

## t.w is in terms of proportion of the width of the plotting area
## ... gets passed to arrow.head()
bez.arrow <- function(pts, bt.n=30, bt=seq(0,1, length.out=bt.n), tube.w=0.01, a.h=tube.w * 6,
                      col='grey', border=NA, draw=TRUE, ...){
     b.pts <- bezier.pts(pts, t=bt)
     ae <- arrow.end(b.pts, height=a.h, ...)
     a.tube <- tube(ae$l, tube.w)
     if(draw){
         polygon(a.tube, col=col, border=border)
         polygon(ae$ah, col=col, border=border)
     }
     invisible(list(tube=a.tube, head=ae$ah, line=ae$l))
}
    
    
## x is taken as positions along an arc running from angle, a.beg to a.end
plotPolar <- function(o.x, o.y, x, y, r, y.depth, a.beg, a.end, degrees=FALSE, drawBorders=TRUE, drawCenter=TRUE, ...){
    if( degrees ){
        a.beg <- pi * a.beg / 180
        a.end <- pi * a.end / 180
    }
    x.r <- range(x)
    x.o <- (x - x.r[1]) / (x.r[2] - x.r[1])
    a <- a.beg + x.o * (a.end - a.beg)

    y.r <- range(y)
    y.o <- (y - mean(y.r)) / (y.r[2] - y.r[1])

    y.x <- o.x + sin( a ) * (r + y.o * y.depth)
    y.y <- o.y + cos( a ) * (r + y.o * y.depth)

    if(drawCenter)
        lineArc(o.x, o.y, r, a.beg, a.end, lty=2)
    if(drawBorders){
        lineArc(o.x, o.y, r + y.depth/2, a.beg, a.end )
        lineArc(o.x, o.y, r - y.depth/2, a.beg, a.end )
    }
    
    points(y.x, y.y, ...)
    
}

## if useNormals is true then the user may specify two additional points; one point
## extending from the first and last position for calculating the normals.
pathLetters <- function( x, y, word, cex=NA, cex.adj=0.75, useNormals=FALSE, ... ){
    chars <- unlist(strsplit(word, ''))
    ## to work out a reasonable cex we need to know the space between the letters
    ## that is the distances between the positions in x and y
    l <- length(chars)
    if(useNormals){
        ## we could consider using lowess / loess / smooth.spline / predict
        ## to get smoother transitions, but I think this is pretty OK to start with
        if(length(x) == length(chars) + 2){
            x.x <- x
            y.x <- y
            x <- x[2:(l+1)]
            y <- y[2:(l+1)]
        }else{
            ## add a point before and a point after that is extended in
            ## the same direction as the adjacent point
            x.x <- c( x[1] - (x[2] - x[1]), x, x[l] + (x[l] - x[l-1]) )
            y.x <- c( y[1] - (y[2] - y[1]), y, y[l] + (y[l] - y[l-1]) )
        }

        x.d <- x.x[3:(l+2)] - x.x[1:l]
        y.d <- y.x[3:(l+2)] - y.x[1:l]
        normals <- 1 * 180/pi * acos( x.d / sqrt(x.d^2 + y.d^2) ) ## we need
        normals <- ifelse( y.d < 0, -1 * normals, normals )
    }
    d <-  sqrt( diff(x)^2 + diff(y)^2 )
    def.cex <- par("cex")
    if(is.na(cex)){
        w.ratios <- c(d[1], d) / strwidth( chars )
        cex <- cex.adj * def.cex * min( w.ratios )
    }
    ## Then if we have not defined any normals, then we simply do
    if(!useNormals){
        text( x, y, chars, cex=cex, ... )
    }else{
        ## srt can only be a single value.. 
        for(i in 1:l){
            text( x[i], y[i], chars[i], cex=cex, srt=normals[i], ... )
        }
    }
}

## wraps text using usr coordinates roughly translated to columns
## does not use strwrap in order to work better with non-monospaced
## fonts
## returns the string with newlines added and width and height
## required
usrStrWrap <- function(txt, w, ...){
    txt <- as.character(txt)
    words <- unlist(strsplit(txt, ' |\t'))
    current.line = words[1]
    txt.f <- current.line
    i <- 2
    while(i <= length(words)){
        if( strwidth( paste( current.line, words[i] ), ... ) <= w ){
            current.line <- paste(current.line, words[i])
            txt.f <- paste(txt.f, words[i])
        }else{
            current.line <- words[i]
            txt.f <- paste(txt.f, words[i], sep='\n')
        }
        i <- i + 1
    }
    list('w'=strwidth(txt.f, ...), 'h'=strheight(txt.f, ...), 's'=txt.f)
}

## r is the locations of a bounding box
## left, right, bottom, top (the same as you get from par("usr")
## this function seems to work OK, but is really rather messy.. 
## note that I could probably use strwrap here; but that wraps on
## columns.. 
boxText <- function(r, txt, max.cex=NA, min.cex=NA, margin=0.1, ...){
    r <- as.numeric(r)
    w = r[2] - r[1]
    h = r[4] - r[3]
    ## split the txt into words
    ## we can only handle one set of words to start with.. 
    words <- unlist(strsplit(txt, ' '))
    words.nchar <- nchar(words)
    ## word.widths <- strwidth( words )
    txt.nchar <- nchar(txt)
    char.width <- strwidth( txt ) / txt.nchar ## an approximation
    ## we need to consider line spacing as well.
    char.height <- strheight(paste("A", "A", sep="\n")) - strheight("A")
    ## work out the ideal number of chars per line on the basis of the dimensions of the box
    ## we intentionally skew this towards allowing a longe text per line than necessary,
    ## since no line is likely to give the exact number.
    ##
    ## r = ncol / nrow = (char.height / char.width) * (w / h)
    ## 
    ## We also know that
    ## ncol * nrow = txt.nchar (the total number of characters (without spaces))
    ## hence we can obtain the number of columns by substitution
    ## nrow = ncol / r
    ## ncol * ncol/r = txt.nchar
    ## ncol^2 / r = txt.nchar
    ## ncol = sart( r * txt.nchar )
    ## since r = (char.height / char.width) * (w / h)
    line.nchar <- sqrt( (char.height / char.width) * (w / h) * txt.nchar )
    line.n <- floor(sqrt( (h/w) * (char.width/char.height) * txt.nchar ))
    ### UGLY KLUDGE BODGE
    ### to avoid doing something re-iterative; as that is the only way that I can
    ### thing of getting things to work. 
    line.nchar <- ceiling(txt.nchar / line.n) + mean(words.nchar) * line.n / 3

    ## an approximation of the space under the current cex
    line.width <- line.nchar * char.width 
    
    ## for filling the box
    txt.prt <- words[1]
    i <- 2
    while(i <= length(words)){
        current.line <- words[i-1]
        line.length <- nchar( words[i-1] )
        while(i <= length(words) && strwidth(paste(current.line, words[i])) <= line.width ){
#        while( line.length + nchar(words[i]) <= line.nchar && i <= length(words)  ){
            txt.prt <- paste(txt.prt, words[i]) ## sep is a space..
            current.line <- paste(current.line, words[i])
            line.length <- line.length + nchar(words[i]) + 1
            i <- i + 1
        }
        if(i <= length(words)){
            txt.prt <- paste(txt.prt, words[i], sep='\n')
            i <- i + 1
        }
    }
    ## now I should have something reasonable..
    txt.width.r <- (1 + margin) * strwidth(txt.prt) / w
    txt.height.r <- (1 + margin) * strheight(txt.prt) / h
    ## simply scale the cex appropriately
    max.r <- max( c(txt.width.r, txt.height.r) )
    cex <- par("cex") / max.r
    if(!is.na(max.cex)) cex <- min(cex, max.cex)
    if(!is.na(min.cex)) cex <- max(cex, min.cex)
    text( r[1] + w * margin/2, mean(r[3:4]), txt.prt, cex=cex, adj=c(0,0.5), ... )
    invisible(cex)
}

make.prefix <- function(r, style='d', sep=". "){
    if(!(style %in% c('d', 'l', 'L', 'R', 'r')))
        style = 'd'
    sapply(r, function(i){
        prefix <- switch(style,
                         'd'=as.character(i),
                         'l'=letters[1 + (i-1) %% length(letters)],
                         'L'=LETTERS[1 + (i-1) %% length(LETTERS)],
                         'R'=as.roman(i),
                         'r'=tolower(as.roman(i)))
        paste(prefix, sep, sep="")
    })
}

## splits text at backticks; sets the font value
## of the substrings
text.format <- function(text, cex=1, ...){
    txt <- strsplit( text, "`" )
    for(i in 1:length(txt)){
        fonts <- suppressWarnings( as.numeric( sub("^([1-5]).+", "\\1", txt[[i]] )))
        fonts[ is.na(fonts) ] <- 1
        words <- sub("^[1-5](.*)", "\\1", txt[[i]])
        w <- mapply( function(wd, cx, fnt){ strwidth(wd, cex=cx, font=fnt) }, words, cex, fonts )
        h <- mapply( function(wd, cx, fnt){ strheight(wd, cex=cx, font=fnt) }, words, cex, fonts )
        txt[[i]] <- list( 'txt'=words, 'font'=fonts,
                         h=h, w=w, n=length(words))
    }
    ## txt can then be drawn by simply calling:
    ## text( x, y, txt$txt, cex=cex, adj=c(0,0), font=txt$font)
    ## but note that this only works for single lines of text.
    ## also note that adj=c(0,0) is absolutely necessary.
    txt
    ## To make this useful, we have to adapt the text wrapping functions to
    ## use this format. But for now we will leave that a something to be done later.
}

## scales text on the x-axis
## Make sure that it fits. If y or x is not given
## center text on the dimensions
## multiple cex values can be specified, but only a single adj
## and only a single family
sc.text <- function(txt, x=NA, y=NA, max.w=diff(par('usr')[1:2]), cex=1, adj=c(0.5,0), max.iter=20, npc=FALSE, ...){
    usr <- par('usr')
    if(is.na(x))
        x <- ifelse(npc, 0.5, mean(usr[1:2]))
    if(is.na(y))
        y <- ifelse(npc, 0.5, mean(usr[3:4]))
    if(npc){
        x <- grconvertX(x, "npc", "user")
        y <- grconvertY(y, "npc", "user")
    }
    iter <- 1
    repeat({
        w <- strwidth(txt, cex=cex, ...)
        left <- x - (w * adj[1])
        right <- x + (w * (1-adj[1]))
        if( (max( right - left ) <= max.w && min(left) >= usr[1] && max(right) <= usr[2]) || iter >= max.iter )
            break
        cex <- cex * 0.75
        iter <- iter + 1
    })
    text( x, y, txt, cex=cex, adj=adj, ... )
    h <- strheight(txt, cex=cex, ...)
    npw <- with(par(), w / (usr[2]-usr[1]))
    nph <- with(par(), h / (usr[4]-usr[3]))
    npx <- with(par(), (x-usr[1]) / (usr[2]-usr[1]))
    npy <- with(par(), (y-usr[3]) / (usr[4]-usr[3]))
    invisible(list(cex=cex, x=x, y=y, w=w, h=h, npx=npx, npw=npw, npy=npy, nph=nph))
}

## x and y give the top left position of the bullet list.
## the text should be a list of character vectors;
## t.cex gives the cex given to the each line. The text will wrapped to
## fit.
## if bullet = TRUE; prefix with pch
## otherwise prefix with: 'd'=number, 'l'=lower case letter, 'L'=upper case letter
##                        'r'=roman numeral (i, ii, iii)
bullet.list <- function(text, x, y, max.w=NA, max.h=NA, indent.str='MM', t.cex=c(4,3,2,1), pch=19, line.spc=2,
                        prefix.style=c('d', 'l', 'r'), prefix.sep=c(". ", ") ", " "),  bullet=TRUE, bullet.cex=1,
                        ...){
    ## txt.fmt must be a data frame, that contains:
    ## 
    format.txt <- function(text, max.w, indent, indent.str, t.cex, level, j, ...){
        text.fmt <- data.frame()
        j.sub <- 0
        level.n <- sum( sapply( text, is.character ))
        cex <- t.cex[ min(level+1, length(t.cex)) ]
        if(!bullet)
            prefix.space <- strwidth( make.prefix(level.n,
                                                  prefix.style[min(level+1, length(prefix.style))],
                                                  prefix.sep[min(level+1, length(prefix.sep))]),
                                     cex=cex, ...)
        else
            prefix.space <- strwidth('x', cex=cex, ...)
        ## then go through and make all the members.. 
        for(i in 1:length(text)){
            l1 <- indent + ifelse(level > 0, strwidth(indent.str, cex=cex, ...), 0)
            if(is.list(text[[i]])){
                text.fmt = rbind(text.fmt, format.txt(text[[i]], max.w, indent + l1, indent.str, t.cex, level+1, j.sub))
                j.sub <- j.sub + 1
                next
            }
            ## or we have to do the actual formatting.
            j <- j + 1
            prefix <- ifelse(bullet, '', make.prefix(j + 1:length(text[[i]]) - 1,
                                                      prefix.style[min(level+1, length(prefix.style))],
                                                      prefix.sep[min(level+1, length(prefix.sep))]))
##            bullet.space <- ifelse(bullet, strwidth('x', cex=cex), 0)
            ## if(!bullet)
            ##     text[[i]] <- paste(prefix, text[[i]], sep="")
            w <- max.w - (l1 + prefix.space)
            txt <- do.call( rbind, Map(data.frame, lapply( text[[i]], usrStrWrap, w=w, cex=cex, ... )) )
            text.fmt <- rbind(text.fmt, cbind('x'=l1, 'cex'=cex, 'ls'=strheight('A', cex=cex, ...), 'prefix'=prefix,
                                              'p.space'=prefix.space, txt))
        }
        text.fmt
    }
    usr <- par('usr')
    if(is.na(max.w))
        max.w <- usr[2] - x
    if(is.na(max.h))
        max.h <- y - usr[3]
    
    repeat({
        txt.r <- format.txt( text, max.w, 0, indent.str, t.cex, 0, 0, ...)
        h <- txt.r$h + (line.spc * txt.r$ls)
        if( sum(h) < max.h )
            break
        t.cex <- t.cex * 0.9
    })
    ls <- txt.r$ls * line.spc
    h <- txt.r$h
    y <- ls[1] + y - cumsum(h + ls)
    b.y <- y + h - txt.r$ls / 2
    p.y <- y + h - txt.r$ls 
##    b.w <- ifelse(rep(bullet, nrow(txt.r)), strwidth('x', cex=txt.r$cex), 0)
    b.w <- txt.r$p.space
    text( x + txt.r$x + b.w, y, txt.r$s, cex=txt.r$cex, adj=c(0,0), ... )
    text( x + txt.r$x, p.y, txt.r$prefix, cex=txt.r$cex, adj=c(0,0), ... )    
    if(bullet)
        points( x + txt.r$x, b.y, pch=pch, cex=txt.r$cex * bullet.cex )
    invisible(list( y=y, r=txt.r ))
}

## some ideas as to how to output tables in R..
## there are a whole load of things, but these generally markdown / latex / html
## output, rather than simply drawing a tabe using the plot functions..
## say we would like to plot a table,,
## this is still a bit messy, but it works OK.
## if we wish to have seperate justification for individual columns, then
## we need to draw each column seperately as we cannot specify adj as a list of vectors?
plotTable <- function(x, y, df, c.widths=NULL, num.format=NA,
                      row.margin=1, col.margin=0.1, doPlot=TRUE,
                      row.bg=NA, column.bg=NA, cell.bg=NA, text.col=1,
                      text.adj=c(0,0.5), font=1, header.font=2,
                      ...){
    df.m <- as.matrix(df)
    if(length(num.format) > 1 || !is.na(num.format)){
#        num.count <- 0
        for(i in 1:ncol(df)){
            if(is.numeric(df[,i])){
                df.m[,i] <- sprintf(num.format[ 1 + (i-1) %% length(num.format) ], df[,i])
                ##num.count <- num.count + 1
            }
        }
    }
    ## add column names if they are present:
    if(!is.null(colnames(df)))
        df.m <- rbind(colnames(df), df.m)

    ## This should probably be an option, but seems reasonable 
    df.m[is.na(df.m) | is.null(df.m)] <- ""
    
    if(is.null(c.widths)){
        c.widths <- (apply( df.m, 2, function(x){ max( strwidth(x, ...) ) } ))
    }
    ## make a matrix of colours..
    ## assume that colours are given per column
    text.col <- matrix(text.col, nrow=nrow(df.m), ncol=ncol(df.m), byrow=TRUE)
    font <- matrix(font, nrow=nrow(df.m), ncol=ncol(df.m), byrow=TRUE)
    if(!is.null(colnames(df)))
        font[1,] <- header.font
    if(!is.matrix(text.adj))
        text.adj <- matrix(text.adj, ncol=2, nrow=ncol(df.m), byrow=TRUE)
    ## determine r.heights using the usrStrWrap function
    ## but this cannot wrap the actual text as it uses apply, which cannot
    ## modify the df.m..
    r.heights <- vector(mode='numeric', length=nrow(df.m ))
    for(i in 1:length(r.heights)){
        x.f <- vector(mode='list', length=ncol(df.m))
        for(j in 1:length(x.f))
            x.f[[j]] <- usrStrWrap( df.m[i,j], c.widths[j], font=font[i,j], ... )
        r.heights[i] <- max( sapply(x.f, function(y){ y$h } ) )
        df.m[i,] <- sapply( x.f, function(y){ y$s })
    }
        
    h.margin <- min( c.widths ) * col.margin
    c.widths <- c.widths + h.margin
    ## r.heights <- apply( df.m, 1,
    ##                    function(x){
    ##                        x.f <- vector(mode='list', length=length(x))
    ##                        for(i in 1:length(x))
    ##                            x.f[[i]] <- usrStrWrap( c.widths[i], x[i], ... )
    ##                        max( sapply(x.f, function(y){ y$h } ) )
    ##                    })
    
    v.margin <- min( r.heights[r.heights > 0] ) * row.margin
    r.heights <- r.heights + v.margin
    ## v.margin <- min( r.heights ) * row.margin/2
    ## r.heights <- r.heights + (v.margin)  * min(r.heights)
#    c.widths <- c.widths + mean( c.widths ) * col.margin
### then we know where to place things, starting at the top and using adj=c(0,1)
### 
    y.bot <- y - cumsum(r.heights)
    y.top <- c(y, y.bot[ -length(y.bot) ])
    x.right <- x + cumsum(c.widths)
    x.left <- c(x, x.right[ -length(x.right) ]) 
    
    ## redo these so that we can have a matrix of each positions.
    y.top.m <- matrix( rep(y.top, ncol(df.m)), nrow=length(y.top) )
    y.bot.m <- matrix( rep(y.bot, ncol(df.m)), nrow=length(y.bot) )
    x.left.m <- matrix( rep(x.left, nrow(df.m)), ncol=length(x.left), byrow=TRUE )
##    x.left.m <- matrix( rep(x.left + text.adj[1] * (c.widths - h.margin*1.5), nrow(df.m)), ncol=length(x.left), byrow=TRUE )
    c.widths.m <- matrix( rep(c.widths, nrow(df.m)), ncol=length(c.widths), byrow=TRUE )
    
    ## then simply,,
    if(doPlot){
        if(length(row.bg) > 1 || !is.na(row.bg))
            rect( x.left[1], y.bot, rev(x.right)[1], y.top, col=row.bg, border=NA )
        if(length(column.bg) > 1 || !is.na(column.bg))
            rect( x.left, rev(y.bot)[1], x.right, y.top[1], col=column.bg, border=NA )
        if(is.matrix(cell.bg) && nrow(cell.bg) == nrow(df.m) && ncol(cell.bg) == ncol(df.m))
            rect( x.left.m, y.bot.m, x.left.m + c.widths.m, y.bot.m + r.heights, col=cell.bg, border=NA )
        ##        text( x.left.m + h.margin/2, (y.top.m + y.bot.m)/2, df.m, adj=text.adj, col=text.col, ... )
        for(i in 1:ncol(df.m)){
            ## text( x.left.m[,i] + h.margin/2, y.bot.m[,2] + text.adj[i,2] * r.heights + (0.5-text.adj[i,2]) * v.margin,
            ##      df.m[,i], adj=text.adj[ 1 + (i-1) %% nrow(text.adj), ], col=text.col[,i], font=font[,i], ... )
            text.w <- strwidth(df.m[,i], font=font[,i], ...)
            text.h <- strheight(df.m[,i], font=font[,i], ...)
            text( x.left.m[,i] + h.margin/2 + (c.widths[i] - (text.w+h.margin)) * text.adj[i,1],
                 y.bot.m[,2] + v.margin/2 + (r.heights - (text.h+v.margin)) * text.adj[i,2],
##                 y.bot.m[,2] + text.adj[i,2] * r.heights + (0.5-text.adj[i,2]) * v.margin,
                 df.m[,i], adj=c(0,0), col=text.col[,i], font=font[,i], ... )
        }
        ## text( x.left.m + h.margin/2, y.bot.m + text.adj[2] * r.heights + (0.5-text.adj[2]) * v.margin,
        ##      df.m, adj=text.adj, col=text.col, font=font, ... )
    }
    invisible( list('r'=x.right, 'l'=x.left, 't'=y.top, 'b'=y.bot, 'h.m'=h.margin, 'v.m'=v.margin) )
}


## A rotate function
rotate.pts <- function(pts, a=pi/2, origin=apply(pts, 2, function(x){mean(range(x))}), preserve.aspect=FALSE){
    rot=matrix(c(cos(a), -sin(a), sin(a), cos(a)), nrow=2, byrow=TRUE)
    tfd <-t( t(pts) - origin)
    if(!preserve.aspect){
        tfd <- tfd %*% rot
    }else{
        ## the amount of device space used per x..
        asp <- with(par(), (pin[1]/pin[2]) / (diff(usr[1:2])/diff(usr[3:4])) )
        tfd[,1] <- tfd[,1] * asp
        tfd <- tfd %*% rot
        tfd[,1] <- tfd[,1] / asp
    }
    tfd <- t( t(tfd) + origin )
    colnames(tfd) <- colnames(pts)
    tfd
}

scale.pts <- function(pts, x.scale, y.scale=x.scale, origin=colMeans(pts)){
    tfd <- t( t(pts) - origin)
    colnames(tfd) <- colnames(pts)
    tfd[,1] <- tfd[,1] * x.scale
    tfd[,2] <- tfd[,2] * y.scale
    t( t(tfd) + origin )
}

translate.pts <- function(pts, x, y){
    t( t(pts) + c(x,y) )
}

### A somewhat prettier arrow than the usual one
### makes an upward pointing arrow
### scale by aspect ?
### by pointing upwards, the arrow width is defined by
### x coordinates, whereas the length is by y-coordinates
arrow.x2.pts <- function(a.l, a.w, ah.l=0.2*a.l, ah.w=3*a.w, p.n=30){
    x1 <- seq(-3, 0, length.out=p.n)
    y1 <- (4 + x1)^2
    y1 <- y1 - min(y1)
    x2 <- seq(-3, min(x1) * a.w/ah.w, length.out=p.n)
    y2 <- ((4+x2)*0.5)^2
    y2 <- y2 - min(y2)

    c.w <- max(abs(x1)) * 2 ## current width
    x1 <- x1 * ah.w / c.w
    x2 <- x2 * ah.w / c.w

    c.h <- max(y1)
    y1 <- y1 * ah.l / c.h
    y2 <- y2 * ah.l / c.h
    
    x <- c(rev(x1), x2, -a.w/2, 0)
    y.min <- max(y1) - a.l
    y <- c(rev(y1), y2, y.min, y.min)
    x <- c(x, -rev(x))
    y <- c(y, rev(y))
    y <- y - min(y)
    cbind('x'=x, 'y'=y)
}

## 
arrow.x2 <- function(x1, y1, x2, y2, a.w, ah.w, ah.l, ah.l.prop=(ah.l < 1), p.n=30,
                     preserve.aspect=TRUE){
    asp <- ifelse( preserve.aspect,
                  with(par(), (pin[1]/pin[2]) / (diff(usr[1:2])/diff(usr[3:4])) ),
                  1)
    l <- sqrt( (asp * (x2-x1))^2 + (y2-y1)^2 )
    ## I suspect that there should be a better way of getting the angle,
    ## but I don't know it... (maybe using complex numbers would do it?)
    a1 <- asin( asp*(x2-x1) / l )
    a2 <- acos( (y2-y1) / l )
    a <- ifelse( a1 > 0, a2, 2 * pi - a2 )
    if(ah.l.prop)
        ah.l <- l * ah.l
    pts <- arrow.x2.pts(l, a.w, ah.l, ah.w, p.n)
    pts[,2] <- pts[,2] - max(pts[,2])
    pts <- rotate.pts(pts, a, origin=c(0, 0), preserve.aspect=preserve.aspect)
    pts <- translate.pts(pts, x2, y2)
    pts
}


## places an image using rasterImage
## the image should be an array of three dimensions
## w, h, 3|4
## where the last dimension specifies the colors
## values should be between 0 and 1
## w and h are determined in some reasonable manner
## x and y give left and top coordinats of the image
##
## Note that images can be read in using "readPNG" from the png
## library, readJPEG from the jpeg library, readTIFF from tiff
## and probably others.
placeImage <- function(img, x, y, w=NA, h=NA, invert.y=FALSE, display=TRUE, ...){
    usr <- par('usr')
    pin <- par('pin')
    asp <- (pin[1]/pin[2]) / (diff(usr[1:2])/diff(usr[3:4]))
    sp.asp <- asp * (usr[2] - x) / (y - usr[3])
    img.asp <- ncol(img) / nrow(img)
    if(is.na(w) && is.na(h)){
        if(img.asp > sp.asp){  ## image has a wider aspect than the remaining space
            w <- usr[2] - x
            h <- w * (asp / img.asp)
        }else{
            h <- y - usr[3]
            w <- h / (asp / img.asp)
        }
    }
    if(is.na(w) ){ ## then h must be defined
        w <- h / (asp / img.asp)
    }
    if(is.na(h) ){
        h <- w / (img.asp / asp)
    }
    ## note that ytop and ybottom are inverted
    if(display){
        if(invert.y)
            rasterImage(img, xleft=x, xright=x+w, ybottom=y, ytop=y-h, ...)
        else
            rasterImage(img, xleft=x, xright=x+w, ybottom=y-h, ytop=y, ...)
    }
    invisible(c(x=x, y=h, w=w, h=h))
}
