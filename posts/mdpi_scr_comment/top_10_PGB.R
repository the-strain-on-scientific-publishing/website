library(tidyverse)
library(ggtext)
library(gt)
library(gtExtras)
library(ggrepel)

## PART I: Python to R translation

# Load the data
data <- read_csv("2024_data.csv")

# Drop NA values in "Name" and "Web of Science Documents"
data <- data %>%
  filter(Name!="n/a")%>%
  filter(`Web of Science Documents`!="n/a")%>%
  filter(`WoS Categories`!="n/a")

# Create a new column "Publisher"
data <- data %>%
  mutate(Publisher = coalesce(`Publisher (unified)`, `Publisher (all)`))

# Create a new column "Self Citation Rate"
data <- data %>%
  mutate(`Self Citation Rate` = 1 - `Times Cited without Self-Citations` / `Times Cited`)

# Create a new dataset with the top 10 publishers based on the Web of Science document count
top_10_publishers <- data %>%
  group_by(Publisher) %>%
  summarise(`Total Documents` = sum(`Web of Science Documents`, na.rm = TRUE)) %>%
  arrange(desc(`Total Documents`)) %>%
  slice_head(n = 10)


# Filter the data to include only the top 10 publishers
top_publishers_data <- data %>%
  filter(Publisher %in% top_10_publishers$Publisher)



# Calculate the average self-citation rate for the top 10 publishers
avg_self_citation_rate <- top_publishers_data %>%
  group_by(Publisher) %>%
  summarise(`Avg Self Citation Rate` = mean(`Self Citation Rate`, na.rm = TRUE))

# Merge the average self-citation rate with the top 10 publishers dataset
top_10_publishers <- top_10_publishers %>%
  left_join(avg_self_citation_rate, by = "Publisher")

orig_tp<-top_10_publishers

scale_factor <- max(top_10_publishers$`Total Documents`, na.rm = TRUE) / 0.3

ggplot(top_10_publishers, aes(x = fct_reorder(Publisher, desc(`Total Documents`)))) +
  geom_bar(aes(y = `Total Documents`), stat = "identity", fill = "skyblue", alpha = 0.8) +
  geom_point(aes(y = `Avg Self Citation Rate` * scale_factor), color = "orange", size = 3) +
  geom_line(aes(y = `Avg Self Citation Rate` * scale_factor, group = 1), color = "orange", linewidth = 1.2) +
  scale_y_continuous(
    name = "Total Documents",
    labels = scales::comma, 
    sec.axis = sec_axis(~ . / scale_factor, name = "Avg Self-Citation Rate")
  ) +
  labs(
    title = "2024 Top 10 Publishers: <span style='color:skyblue'><b>Total Documents </b></span> and <span style='color:orange;'><b>Avg Self-Citation Rate</b></span> (R coded)",
    x = "Publisher"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_markdown(),
        plot.background = element_rect(fill="white"))

ggsave("MDPI_graph_with_R.png",dpi="retina")

## PART 2. Editing graph

ggplot(top_10_publishers, aes(y= `Avg Self Citation Rate`,x=  fct_reorder(Publisher, `Avg Self Citation Rate`))) +
  geom_segment(aes(x=Publisher,xend=Publisher,y=0,yend=`Avg Self Citation Rate`),linetype="dashed",linewidth=1.1)+ 
  geom_point(colour="black",fill = "darkred", size = 8, shape=21) +
  
  labs(
    title = "2024 Top 10 Publishers: <span style='color:darkred;'><b>Avg Self-Citation Rate</b></span>",
    x = "Publisher"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_markdown(),
        text = element_text(size=16),
        plot.background = element_rect(fill="white"))+
  scale_y_continuous(limits = c(0,0.3))+
  coord_flip()

ggsave("MDPI_redo.png",dpi="retina")

## Part 3: Applying weighted means based on total number of publictions for each journal

# Load the data
data <- read_csv("2024_data.csv")

# Drop NA values in "Name" and "Web of Science Documents"
data <- data %>%
  filter(Name!="n/a")%>%
  filter(`Web of Science Documents`!="n/a")%>%
  filter(`WoS Categories`!="n/a")

# Create a new column "Publisher"
data <- data %>%
  mutate(Publisher = coalesce(`Publisher (unified)`, `Publisher (all)`))

# Create a new column "Self Citation Rate"
data <- data %>%
  mutate(`Self Citation Rate` = 1 - `Times Cited without Self-Citations` / `Times Cited`)

# Create a new dataset with the top 10 publishers based on the Web of Science document count
top_10_publishers <- data %>%
  group_by(Publisher) %>%
  summarise(`Total Documents` = sum(`Web of Science Documents`, na.rm = TRUE)) %>%
  arrange(desc(`Total Documents`)) %>%
  slice_head(n = 10)

# Filter the data to include only the top 10 publishers
top_publishers_data <- data %>%
  filter(Publisher %in% top_10_publishers$Publisher)



# Calculate the average self-citation rate for the top 10 publishers
avg_self_citation_rate <- top_publishers_data %>%
  group_by(Publisher) %>%
  summarise(`Avg Self Citation Rate` = weighted.mean(`Self Citation Rate`, `Web of Science Documents`, na.rm = TRUE))


# Merge the average self-citation rate with the top 10 publishers dataset
top_10_publishers <- top_10_publishers %>%
  left_join(avg_self_citation_rate, by = "Publisher")

wm_tp<-top_10_publishers

## Part 4. New graph after weighted means

ggplot(top_10_publishers, aes(y= `Avg Self Citation Rate`,x=  fct_reorder(Publisher,`Avg Self Citation Rate`))) +
  geom_segment(aes(x=Publisher,xend=Publisher,y=0,yend=`Avg Self Citation Rate`),linetype="dashed",linewidth=1)+ 
  geom_point(colour="black",fill = "darkred", size = 8, shape=21) +
  
  labs(
    title = "2024 Top 10 Publishers: <span style='color:darkred;'><b>Avg Self-Citation Rate</b></span>",
    subtitle = "after calculating weighted mean",
    x = "Publisher"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_markdown(),
        text = element_text(size=16),
        plot.background = element_rect(fill="white"))+
  scale_y_continuous(limits = c(0,0.3))+
  coord_flip()

ggsave("MDPI_redo_mw.png",dpi="retina")


## Part 5. Original vs weighted TOP 10 table

table<-left_join(orig_tp,wm_tp,by=c("Publisher", "Total Documents"))%>%

  rename(Original=3,Weighted=4) %>%
  
  mutate(Original=(round(Original,4)*100),
         Weighted=(round(Weighted,4))*100)%>%
  
  mutate(Change=Weighted-Original)%>%
  select(Publisher, Original, Weighted,Change)%>%
  gt() %>%
  tab_style(
    style = cell_text(color = "darkgreen", weight = "bold"),
    locations = cells_body(columns = Change, rows = Change > 0)
  )%>%
  
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(columns = Change, rows = Change < 0)
  )  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

table

## MDPI jourlans self-citation rates 2024

highlight_journals <- c("RELIGIONS","SEXES","IJMS",
                        
                        "APPLIED SCIENCES","SUSTAINABILITY","SENSORS","GENEALOGY","ARTS",
                        "LANGUAGES","JCM", "ENERGIES","MATERIALS") 

data%>%
  filter(Publisher=="MDPI")%>%
  
  ggplot(.,aes(x=`Web of Science Documents`,y=`Self Citation Rate` ,size=`Web of Science Documents`,fill = `Self Citation Rate`))+
  geom_point(shape=21,colour="black",alpha=.9)+
  geom_text_repel(data = data %>%
                    filter(Publisher=="MDPI")%>%
                    mutate(Name=ifelse(grepl("INTERNATIONAL JOURNAL OF MOLECULAR SCIENCES",Name),"IJMS",Name),
                           Name=ifelse(grepl("APPLIED SCIENCES-BASEL",Name),"APPLIED SCIENCES",Name),
                           Name=ifelse(grepl("JOURNAL OF CLINICAL MEDICINE",Name),"JCM",Name))%>%
                    filter(Name %in% highlight_journals),
                    
            aes(label = Name), size = 3,point.padding = 5) + 
  labs(x="Articles in 2024",
       title = "MDPI self-citation rates (2024)",
       size="Total articles",
       fill="SCR")+
  scale_y_continuous(labels=scales::label_percent())+
  scale_fill_viridis_c()+
  theme_minimal() +
  theme(text = element_text(size=16),
        plot.background = element_rect(fill="white"),
        legend.position = c(.7,.8),
        legend.direction = "horizontal",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))

ggsave("MDPI_scr.png",dpi="retina")  
