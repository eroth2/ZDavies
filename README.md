# ZDavies
Zach Davies Project
#Eric Roth Zach Davies 2020 follow up project 
############################################

#scrape Davies' 2020 pitch data
scrape_statcast_savant_pitcher(start_date = "2020-07-23",
                               end_date = "2020-09-26",
                               pitcherid = 605200) -> zdavies20

#Edit pitch names for easier clarification
ifelse(zdavies20$pitch_type == "SI", "SNK", zdavies20$pitch_type) -> zdavies20$pitch_type
ifelse(zdavies20$pitch_type == "FC", "CUT", zdavies20$pitch_type) -> zdavies20$pitch_type

#rename plate_x and z
zdavies20 %>% 
  rename(px = "plate_x",
         pz = "plate_z") -> zdavies20
         
#create count column
zdavies20 %>% 
  mutate(Count = paste(zdavies20$balls, "-", zdavies20$strikes)) -> zdavies20
  
#find % of pitches thrown ny Davies in 2020
zdavies20 %>% 
  group_by(pitch_type) %>% 
  summarize(n = n(),
            pct = n/nrow(zdavies20)*100) %>% select(-n) -> zdavies20_pitch_pct

#arrange in descending order of pitch pct and round to one digit
zdavies20_pitch_pct %>% arrange(desc(pct)) -> zdavies20_pitch_pct
zdavies20_pitch_pct %>% 
  mutate(pct = round(pct, digits = 1)) -> zdavies20_pitch_pct
  
#Separate vs righties vs lefties % (some code from previous project)
zdavies20 %>% filter(stand == "R") -> zdavies20vsR
zdavies20 %>% filter(stand == "L") -> zdavies20vsL
zdavies20 %>% filter(stand == "R") %>% 
  group_by(pitch_type, year) %>% 
  summarize(n = n(),
            pct = n/nrow(zdavies20 %>% filter(stand == "R"))*100) %>% select(-n) %>% 
  mutate(pct = round(pct, digits = 1)) -> zdavies20_pitch_pct_R
  
zdavies20 %>% mutate(year = "2020") -> zdavies20
rbind(ZDavies_vsR, zdavies20_pitch_pct_R) -> zdaviesRpct_combined
as.character(ZDavies$year) -> ZDavies$year
as.character(zdavies20$year) -> zdavies20$year

zdavies20 %>% filter(stand == "L") %>% 
  group_by(pitch_type, year) %>% 
  summarize(n = n(),
            pct = n/nrow(zdavies20 %>% filter(stand == "L"))*100) %>% select(-n) %>% 
  mutate(pct = round(pct, digits = 1)) -> zdavies20_pitch_pct_L
            
#Visualize pitch distributions
#VsR  ggplot(zdaviesRpct_combined, aes(reorder(pitch_type, -pct), pct, fill = year)) + 
        geom_bar(stat = "identity", position = "dodge2") + 
        scale_fill_manual(values = c("#8B4513", "yellow")) +
        xlab("Pitch Type") + ylab("% Thrown")+
        geom_text(aes(label = pct), position = position_dodge(width = .9), vjust = -.25) +
        labs(title = "Davies Pitch Distribution Vs Righties") + 
        theme(plot.title = element_text(face = "bold", hjust = .5)) -> zdavies20_pitch_pct_R_chart
#VsL  ggplot(zdaviesLpct_combined, aes(reorder(pitch_type, -pct), pct, fill = year)) + 
        geom_bar(stat = "identity", position = "dodge2") + 
        scale_fill_manual(values = c("#8B4513", "yellow")) +
        xlab("Pitch Type") + ylab("% Thrown")+
        geom_text(aes(label = pct), position = position_dodge(width = .9), vjust = -.25) +
        labs(title = "Davies Pitch Distribution Vs Lefties") + 
        theme(plot.title = element_text(face = "bold", hjust = .5)) -> zdavies20_pitch_pct_L_chart
  
#combine two graphs: grid.arrange(zdavies20_pitch_pct_R_chart, zdavies20_pitch_pct_L_chart, nrow = 1)


#heat map of Davies' pitch location, changeup location, & and whiffs on changeup

#kzone function
k_zone_plot <- function(...) {
  ggplot(...) +
    geom_rect(xmin = -.94706583,
              xmax = .94706583,
              ymin = 1.5,
              ymax = 3.6, color = "black", alpha = 0) +
    coord_equal() + 
    scale_x_continuous("Horizontal location (ft.)",
                       limits = c(-2,2)) +
    scale_y_continuous("Vertical location (ft.) RHB POV",
                       limits = c(0,5))
}

k_zone_plot(zdavies20, aes(px, pz)) + 
  geom_density_2d_filled(alpha = .8, show.legend = F) +
  xlab("Vertical Location (ft.) RHB POV") +
  ylab("Horizontal Location (ft.)") + labs(title = "Davies Pitch Location Heat Map")+
  theme(plot.title = element_text(face = "bold", hjust = .5)) -> zdavies20_heatmap

k_zone_plot(zdavies20 %>% filter(pitch_type == "CH"), aes(px, pz)) + 
  geom_density_2d_filled(alpha = .8, show.legend = F) +
  geom_point(alpha = .2, color = "white") +
  xlab("Vertical Location (ft.) RHB POV") +
  ylab("Horizontal Location (ft.)") + labs(title = "Davies Changeup Location Heat Map 2020")+
  theme(plot.title = element_text(face = "bold", hjust = .5)) -> zdavies20_heatmap_ch
  
 k_zone_plot(zdavies20 %>% filter(description == "swinging_strike" | description == "swinging_strike_blocked", pitch_type == "CH"), aes(px, pz)) +
  geom_density2d_filled(alpha = .8, show.legend = F) + geom_point(alpha = .3, col = "white") +
  xlab("Vertical Location (ft.) RHB POV") + ylab("Horizontal Location (ft.)") +
  labs(title = "Davies Changeup Whiff Location Heat Map 2020")+
  theme(plot.title = element_text(face = "bold", hjust = .5)) -> zdavies20_heatmap_ch_whiff
  
 #side by side graph
 grid.arrange(zdavies20_heatmap_ch, zdavies20_heatmap_ch_whiff,nrow = 1)

#find distribution of pitches based on count & handedness
round(prop.table(table(zdavies20vsL$pitch_type, zdavies20vsL$Count), margin = 2),2) -> zdavies20_countL
round(prop.table(table(zdavies20vsR$pitch_type, zdavies20vsR$Count), margin = 2),2) -> zdavies20_countR
