
dirGraf <- "TEST/"

nc <- 4

tit <- "My title"
righttext <- "right text"
lefttext <- "left text"

# To ensure there are no graphics devices on:
graphics.off()

# I want to create 7 plotting windows with the following arrangemente
# I'm supposing the NDC coordinates are such that the lower-left corner
# is at (0,0) and the upper-right corner is at (1,1). 
#
#       +-----------------------------------+ (1,1)
#       |                 5    h=0.5/5.5    |
#       +--+-----------------------------+--+
#       |  |              1    h=1.0/5.5 |  |
#       |  +-----------------------------+  |
#       |  |              2    h=1.0/5.5 |  |
#       |  +-----------------------------+  |
#       |  |              3    h=1.0/5.5 |  |
#       |6 +-----------------------------+ 7|
#       |  |                             |  |
#       |  |              4    h=2.0/5.5 |  |
#       |  |                             |  |
#       +--+-----------------------------+--+
#    (0,0)
#
# I want 4 information (central) windows, from 1 to 4. However these windows
# must vary in their heights, because window 4 is twice the height the others.
# I want an upper window to put a title to the whole plot, and two 
# lateral windows to add some texts.
#
# So I will build a four-column matrix, as instructed by the split.screen 
# documentation. Each column correspond in order to the following 
# characteristics, of each window (represented by each row), namely:
# 1. left, 2. right, 3. bottom, 4. top.
#

# First I will create a vector with the first four heights
Yinc <- c(2, rep(1,3))/5.5 
# To have the NDC device coordinates (?) I will accumulate:
Yinc <- Reduce('+', Yinc, accumulate=T)
# To put this information in the right window order I invert the vector
Yinc <- Yinc[4:1] 
# Then I will build the matrix for the first four windows:
Mm <- cbind(left=1/6, right=5/6, bottom=c(Yinc[2:4],0), top=Yinc)
# Now, let's add upper and lateral windows (screens 5 to 7)
Mm <- rbind(Mm, 
            # +--------+--------+--------+--------+
            # | left   | right  | bottom |   top  |
            # +--------+--------+--------+--------+
            c(   0    ,   1    , Yinc[1],    1   ),
            c(   0    ,  1/6   ,   0    , Yinc[1]),
            c(  5/6   ,   1    ,   0    , Yinc[1])
)

# margins for most of the cases
gpar <- list(mar=c(0,2.1,0,0))

# the device name (a pdf file)
gname <- paste0(dirGraf, "Test.pdf") 

# Test table to plot, just a line from (0,0) to (1,1)
tt <- data.frame(x=c(0,1), y=c(0,1))
# Let's open the device:
pdf(gname, width=7, height=9.11) 
# Let's split the device space (I think this is the default, isn't it?)
#YANO>> split.screen(Mm)


# Let's put a title in the upper window
#YANO>> screen(5)
# WINDOW 5
opar <- par(fig=Mm[5,], new=T)
par(gpar)
plot(c(0,1), c(0,1), ylab="", axes=F, type="n")
text(0.5, 0.5, tit, cex=1.5)

# For each "data" window
for (ii in 1:nc) { 
    #YANO>> screen(ii) # select screen 1 to 4
    par(fig=Mm[ii,], new=T)
    par(gpar)
 
    plot(tt, ylab=LETTERS[ii], xlab="", type="l", xaxt="n")   
}
# A text in the left window
#YANO>> screen(6)
par(fig=Mm[6,], new=T)
par(mar=c(0.1,0.1,0.1,0.1))
plot(c(0,1), c(0,1), axes=F, type="n")
text(0.5, 0.5, lefttext, srt=90, cex=1.2)
# A text in the right window
#YANO>> screen(7)
par(fig=Mm[7,], new=T)
par(mar=c(0.1,0.1,0.1,0.1))
plot(c(0,1), c(0,1), axes=F, type="n")
text(0.5, 0.5, righttext, srt=90, cex=1.2)
# Restore to original
par(opar)
# close graphics
graphics.off()
