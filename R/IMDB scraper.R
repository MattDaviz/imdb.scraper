# set working directory
setwd("C:\\Users\\mattd\\Dropbox\\IMDB")

# scraping function --------

'scrape_ratings' <- function(url)
{
  require(XML)
  # get HTML of url
  doc <- htmlParse(url)
  
  # find all tables in webpage
  tables <- readHTMLTable(doc)
  
  # find largest table and return as dataframe
  nrows <- unlist(lapply(tables, function(t) dim(t)[1]))
  df <- tables[[which.max(nrows)]]
  
  return(df)
}

# IMDB id of tv show
url <- 'http://www.imdb.com/title/tt0944947/epdate' # Game of Thrones
url <- 'http://www.imdb.com/title/tt0306414/epdate' # The Wire
url <- 'http://www.imdb.com/title/tt0903747/epdate' # Breaking Bad
url <- 'http://www.imdb.com/title/tt0141842/epdate' # The Sopranos
url <- 'http://www.imdb.com/title/tt0475784/epdate' # Westworld
url <- 'http://www.imdb.com/title/tt0367279/epdate' # Arrested Development
url <- 'http://www.imdb.com/title/tt0795176/epdate' # Planet Earth
url <- 'http://www.imdb.com/title/tt5491994/epdate' # Planet Earth II
url <- 'http://www.imdb.com/title/tt0098904/epdate' # Seinfeld
url <- 'http://www.imdb.com/title/tt0121955/epdate' # South Park
url <- 'http://www.imdb.com/title/tt1520211/epdate' # The Walking Dead
url <- 'http://www.imdb.com/title/tt0096697/epdate' # The Simpsons
url <- 'http://www.imdb.com/title/tt0386676/epdate' # The Office
url <- 'http://www.imdb.com/title/tt0353049/epdate' # Chappelle's Show
url <- 'http://www.imdb.com/title/tt0264235/epdate' # Curb Your Enthusiasm
url <- 'http://www.imdb.com/title/tt0182576/epdate' # Family Guy
url <- 'http://www.imdb.com/title/tt0149460/epdate' # Futurama
url <- 'http://www.imdb.com/title/tt1439629/epdate' # Community
url <- 'http://www.imdb.com/title/tt1442437/epdate' # Modern Family
url <- 'http://www.imdb.com/title/tt0387199/epdate' # Entourage
url <- 'http://www.imdb.com/title/tt0285403/epdate' # Scrubs
url <- 'http://www.imdb.com/title/tt0472954/epdate' # It's Always Sunny in Philadelphia
url <- 'http://www.imdb.com/title/tt0072562/epdate' # Saturday Night Live
url <- 'http://www.imdb.com/title/tt0247082/epdate' # CSI: Crime Scene Ivestigation


###Data Cleaning ----------
series.ratingsA <- scrape_ratings(url)
series.ratings=series.ratingsA
colnames(series.ratings)[1]="Ep_Number"
series.ratings$EpisodeNumber <- as.integer(rownames(series.ratings))
series.ratings$UserVotes <- as.numeric(series.ratings$UserVotes)
series.ratings$UserRatings=as.numeric(levels(series.ratings$UserRating))[series.ratings$UserRating]
series.ratings$s <- lapply(strsplit(as.character(series.ratings$Ep_Number), ''), function(x) which(x == "."))
series.ratings$s=as.numeric(series.ratings$s)-1
series.ratings$season <- as.factor(as.numeric(substr(series.ratings$Ep_Number,1,series.ratings$s)))
series.ratings$Ep_Number = gsub('[[:alpha:]]', '', series.ratings$Ep_Number)

#libraries --------
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(extrafont)

# The Wire ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "The Wire was one of the most consistently great shows of all time",
       subtitle = "IMDB user ratings for every episode of The Wire",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas"))

p

# Export plot with fonts
ggsave("The Wire.pdf", plot = p, width = 10.5, height = 8, device = cairo_pdf)

# Game of Thrones ---------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "The quality of Game of Thrones is beginning to fluctuate",
       subtitle = "IMDB user ratings for every episode of Game of Thrones",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("Game of Thrones.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)

# Breaking Bad ---------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Breaking Bad had the best final season for a TV show ever",
       subtitle = "IMDB user ratings for every episode of Breaking Bad",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),-
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("Breaking Bad.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)

# The Sopranos -----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "The Sopranos was consistently good, not great",
       subtitle = "IMDB user ratings for every episode of The Sopranos",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Sopranos.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)

# Westworld ---------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Westworld",
       subtitle = "IMDB user ratings for every episode of The Sopranos",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Sopranos.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)

# Arrested Development -----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Arrested Development",
       subtitle = "IMDB user ratings for every episode of The Sopranos",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Sopranos.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)

# Planet Earth ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Planet Earth",
       subtitle = "IMDB user ratings for every episode of The Sopranos",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Sopranos.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)

# Planet Earth II ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Planet Earth",
       subtitle = "IMDB user ratings for every episode of The Sopranos",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Sopranos.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# Seinfeld ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Seinfeld",
       subtitle = "IMDB user ratings for every episode of The Sopranos",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Sopranos.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# South Park ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  #geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "South Park",
       subtitle = "IMDB user ratings for every episode of South Park",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_fill_discrete("Season") +
  #scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(fill=guide_legend(nrow=5))

p

#Export Plot with Fonts
ggsave("South Park.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# The Walking Dead ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "The Walking Dead is losing its way",
       subtitle = "IMDB user ratings for every episode of The Walking Dead",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Walking Dead.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)

# The Simpsons ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "D'oh! The Simpsons is mostly mediocre now",
       subtitle = "IMDB user ratings for every episode of The Simpsons",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=4))

p

#Export Plot with Fonts
ggsave("The Simpsons.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)

# The Office ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "The Office",
       subtitle = "IMDB user ratings for every episode of The Office",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Simpsons.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# Chappelle's Show ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Chappelle's Show",
       subtitle = "IMDB user ratings for every episode of Chapelle's Show",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Simpsons.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# Curb Your Enthusiasm ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Chappelle's Show",
       subtitle = "IMDB user ratings for every episode of Chapelle's Show",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Simpsons.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# Family Guy ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Chappelle's Show",
       subtitle = "IMDB user ratings for every episode of Chapelle's Show",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Simpsons.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# Futurama ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Chappelle's Show",
       subtitle = "IMDB user ratings for every episode of Chapelle's Show",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Simpsons.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# Community ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Chappelle's Show",
       subtitle = "IMDB user ratings for every episode of Chapelle's Show",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Simpsons.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# Modern Family ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Chappelle's Show",
       subtitle = "IMDB user ratings for every episode of Chapelle's Show",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Simpsons.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# Entourage ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Entourage",
       subtitle = "IMDB user ratings for every episode of Entourage",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("The Simpsons.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# Scrubs ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "The final season of Scrubs was an outlier",
       subtitle = "IMDB user ratings for every episode of Scrubs",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("Scrubs.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)

# It's Always Sunny in Philadelphia ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  geom_smooth(method = "loess",se = FALSE, size = 2, aes(color = season)) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "It's Always Sunny in Philadelphia",
       subtitle = "IMDB user ratings for every episode of It's Always Sunny in Philadelphia",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_color_discrete("Season") +
  scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(colour=guide_legend(nrow=1))

p

#Export Plot with Fonts
ggsave("Scrubs.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# Saturday Night Live ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  #geom_smooth(method = "loess",se = FALSE, size = 2) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "Saturday Night Live",
       subtitle = "IMDB user ratings for every episode of Saturday Night Live",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_fill_discrete("Season") +
  #scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(fill=guide_legend(nrow=7))

p

#Export Plot with Fonts
ggsave("Scrubs.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)
# CSI: Crime Scene Investigation ----------
p<-ggplot(series.ratings, aes(x=EpisodeNumber, y=UserRatings, fill = season), colour = "black") +
  #geom_smooth(method = "loess",se = FALSE, size = 2) +
  geom_point(pch = 21, size = 4) +
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10,1))) +
  labs(title = "CSI: Crime Scene Investigation",
       subtitle = "IMDB user ratings for every episode of CSI: Crime Scene Investigation",
       caption = "@MattDaviz                                                                                                                                                                                                                                   Source: IMDB") +
  scale_fill_discrete("Season") +
  #scale_fill_discrete(guide = FALSE) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium", size = 20),
        plot.subtitle = element_text(family = "Franklin Gothic Medium", size = 16),
        plot.caption = element_text(family = "Franklin Gothic Medium"),
        legend.text = element_text(family = "Franklin Gothic Book", size = 14),
        legend.title = element_text(family = "Franklin Gothic Medium", size = 14),
        axis.text = element_text(family = "Franklin Gothic Book"),
        text = element_text(family = "Consolas")) +
  guides(fill=guide_legend(nrow=5))

p

#Export Plot with Fonts
ggsave("Scrubs.pdf",plot=p,width=10.5,height=8,device=cairo_pdf)