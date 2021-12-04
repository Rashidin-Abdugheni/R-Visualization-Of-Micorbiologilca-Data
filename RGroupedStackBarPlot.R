```R

set.seed(1995)
# generate random data
data=matrix(abs(round(rnorm(200, mean=1000, sd=500))), 20, 10)
# random data for col and row 20*20
rownames(data)=paste("Species", 1:10, sep=".")
# rownames

rownames(data)=paste("Sample", 1:20, sep=".")
# rownames
# group

group=c("Blautia", "Blautia", "Blautia", "Roseburia", "Roseburia", 
        "Roseburia", "Roseburia", "Coprococcus", "Coprococcus", 
        "Coprococcus", "Coprococcus", "Lachnospira", "Lachnospira", 
        "Lachnospira", "Dorea", "Lachnospira", "Lachnospira", "Dorea", 
        "Dorea", "Dorea")
sample_id=rownames(data)
data_group=data.frame(sample_id, group)
# get the grouped data 
data_norm=data
for(i in 1:20){
  sample_sum=apply(data, 1, sum)
  # sum of the total num 
  for(j in 1:10){
    data_norm[i,j]=data[i,j]/sample_sum[i]
    # set the total as 1
  }
}

library(reshape2)
#for reshape2 
Taxonomy=colnames(data)
# get the Taxonomy data
data_frame=data.frame(t(data_norm), Taxonomy)
# generate data_frame
data_frame=melt(data_frame, id='Taxonomy')
# Taxonomy and Sample based setting
names(data_frame)[2]='sample_id'
# should note :variable is sample_id，data_group names should be the same
data_frame=merge(data_frame, data_group, by='sample_id')


library(ggplot2)# get the figure 
stack_plot=ggplot(data_frame, aes(x=sample_id, fill=Taxonomy, y=value*100))+
 
  geom_col(position='stack') +
  # stack
  labs(x='Samples', y='Relative Abundance (%)')+
  # axis names
  scale_y_continuous(expand=c(0, 0))+
  # axis styling 
  theme(axis.text.x=element_text(angle=45, hjust=1))
# angle：the angle 
# hjust：adjust the distance between x axis

ggsave(stack_plot, filename="stack_plot.pdf")


#install.packages("ggalluvial")
library("ggalluvial")

#install.packages("rlang", version="0.4.7")
#packageVersion("rlang")
stack_plot=ggplot(data_frame,
                  aes(x=sample_id,
                      y=value*100,
                      fill=Taxonomy,
                      stratum = Taxonomy,
                      alluvium = Taxonomy)) +
  geom_bar(stat='identity', width=0.45,position = "fill") +
  geom_alluvium() +
  geom_stratum(width=0.45, size=0.1) +
  labs(x='Samples', y='Relative Abundance (%)')+
  scale_y_continuous(expand=c(0, 0))+
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave(stack_plot, filename="stack_plot.pdf")

stack_plot=ggplot(data_frame, aes(x=sample_id, 
                                  fill=Taxonomy, 
                                  y=value*100,
                                  stratum = Taxonomy,
                                  alluvium = Taxonomy))+
  geom_col(position='stack') +
  geom_alluvium() +
  geom_stratum(width=0.45, size=0.1) +
  labs(x='Samples', y='Relative Abundance (%)')+
  scale_y_continuous(expand=c(0, 0))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  facet_wrap(~group, scales = 'free_x', ncol = 5)+
  theme( axis.text = element_text( size = 12 ),
         axis.text.x = element_text( size = 12 ),
         axis.title = element_text( size = 12, face = "bold" ),
         legend.position="bottom",
         legend.background = element_rect(fill="lightblue",
          size=0.5, linetype="solid", colour ="darkblue"),
         #legend setting colour、size
         legend.title=element_text(size=20), 
         #legend font setting
         legend.text=element_text(size=18),
         #legend.position = ' none' 
         #legend.position="bottom" 
         #or “left”,“top”, “right”, “bottom”.none
         # The new stuff
         strip.text = element_text(size = 14))

stack_plot
#ggsave(stack_plot, filename="stack_plot.pdf") # to save
  

```

