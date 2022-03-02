#####################
# beautifying figures
#####################

######################### 
# real examples in base R
#########################

################################################
# amphibian threat statuses by reproductive mode

#------------------
# data for plotting

# IUCN stuff
categories = c("EX", "CR", "EN", "VU", "NT", "LC", "DD") # IUCN categories
iucnpal <-c("#000000", "#D81E05", "#FC7F3F", "#F9E814", "#CCE226", "#60C659", "#D1D1C6") # official colors of IUCN categories

# number of species in each catergory
dd = c(5, 171, 273, 195, 110, 433, 281) # direct developers
prop.dd = dd/sum(dd)
lar = c(9, 226, 381, 278, 165, 1645, 517) # aquatic larvae
prop.lar = lar/sum(lar)
peds = c(0, 2, 2, 1, 3, 13, 0) # paedomorphic species
prop.peds = peds/sum(peds)
viv = c(1, 6, 7, 2, 0, 10, 19) # live-bearing species
prop.viv = viv/sum(viv)

#---------
# plotting
par(mfrow = c(2, 2), mar = c(5.9, 6, 4, 2), las = 1) # adjust number of pltos to display, margins, and text orientation


barplot(height = prop.dd, names.arg = categories, col = iucnpal, 
        ylim = c(0, 0.6), axes = FALSE, width = 1, cex.names = 1.4, 
        main = "Direct Developers (n = 1468)", cex.main = 1.5)

axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), lwd = 2, cex.axis = 1.5)


barplot(height = prop.lar, names.arg = categories, col = iucnpal, 
        ylim = c(0, 0.6), axes = FALSE, width = 1, cex.names = 1.4, 
        main = "Aquatic Larvae (n = 3221)", cex.main = 1.5)

axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), lwd = 2, cex.axis = 1.5)


barplot(height = prop.peds, names.arg = categories, col = iucnpal, 
        ylim = c(0, 0.6), axes = FALSE, width = 1, cex.names = 1.4, 
        main = "Paedomorphic (n = 21)", cex.main = 1.5)

axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), lwd = 2, cex.axis = 1.5)


barplot(height = prop.viv, names.arg = categories, col = iucnpal, 
        ylim = c(0, 0.6), axes = FALSE, width = 1, cex.names = 1.4, 
        main = "Live-bearing (n = 45)", cex.main = 1.5)

axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), lwd = 2, cex.axis = 1.5)





#######################################################
# predicted vs expected counts of flatwoods salamanders

#-----
# data


# simulated observed counts
counts = c(rnbinom(1000, size = 1, prob = 0.1)) 

len = length(unique(counts)) # get length of unique counts for plotting limits 
xmax = round_any(len, 10, f = ceiling) # round max value of len to have sensible x limit 
ymax = round_any(max(table(counts)), 20, f = ceiling) # round max frequency for sensible y limit

# predicted counts
preds = dnbinom(0:len, size = 1, prob = 0.1) * 1000 


#---------
# plotting

library(plyr) # need this for the round_any() function

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

h <- hist(counts, freq = TRUE, right=F, breaks = len+10, 
          main = "", xlab = "", ylab = "", 
          ylim = c(0, ymax), xlim = c(0, xmax), axes = FALSE, 
          col = "grey")

lines(0:len + 0.5, preds, lwd = 2, lty = "dashed") # add predicted line

axis(1, seq(0, xmax, by = 10)) # add x-axis
axis(2, seq(0, ymax, by = 20)) # add y-axis

mtext("Nightly Captures", side = 1, line = 3, cex = 1.5, font = 2) # x-axis label
mtext("Frequency", side = 2, line = 3.5, cex = 1.5, font = 2, las = 0) # y-axis label




#########################
# real examples in ggplot
#########################
library(ggplot2)

#-------------------------------------
# best customizable theme I have found

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}


#################################################
# migration times of flatwoods salamanders by sex
library(ggpubr)

df1 = data.frame(date = c(rnorm(400, 50, 20), 
                    rnorm(100, 70, 20), rnorm(100, 30, 10), rnorm(100, 90, 20), rnorm(100, 50, 20)),
           sex = sort(rep(c("M", "F"), 400)),
           year = as.factor(c(sort(rep(2010:2013, 100)), sort(rep(2010:2013, 100)))))


p1 <- ggboxplot(df1, x = "sex", y = "date") + 
  facet_wrap(~year) +
  stat_compare_means(label.x = 0.6, label.y = 90) +
  theme_Publication() + 
  coord_flip()



######################################
# body size of major vertebrate clades
library(ggforce)

# mock dataset
df2 = data.frame(class = as.factor(c(rep("Actinopterygii", 100), rep("Amphibia", 100), rep("Reptilia", 100), rep("Aves", 100), rep("Mammalia", 100), rep("Chondrichthyes", 100))),
                 size = c(rnorm(100, 50, 20), rnorm(100, 70, 20), rnorm(100, 100, 30), rnorm(100, 130, 20), rnorm(100, 150, 30), rnorm(100, 200, 30)))

#---------------------------------------------
# geom_sina is a nice alternative to box plots

p2 <- ggplot(df2, aes(class, log(size), col = class)) +
  geom_sina() +
  xlab("") + ylab("Body Size") +
  theme_Publication() + theme(    
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2") +
  scale_size(guide = 'none')


#-----------------------------------------------------------
# ggdist also has plenty of options for boxplot-esque things

p3 = ggplot(df2, aes(class, log(size), col = class)) + 
  ggdist::stat_halfeye(
    adjust = 1,
    width = 0.8, 
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95),
    justification = -.1
  ) +
  coord_cartesian(xlim = c(1.2, NA)) +
  theme_Publication() +
  scale_color_brewer(palette = "Dark2") +
  ylab("Body Size")
