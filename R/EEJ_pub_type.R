library(synthesisr)
library(tidyverse)
library(openalexR)

library(showtext)
library(ggtext)
library(glue)
# 
# # columns: year, review_type, n
# 
# # ---- fonts ----
font_add_google("Oswald", "Oswald")
font_add_google("Nunito", "Nunito")
showtext_auto()

title_font <- "Oswald"
body_font  <- "Nunito"
# 
# # ---- colours ----
bg_col        <- "#F2F4F8"
text_col      <- "#151C28"
protocol_col1 <- "#797FCD"
protocol_col2 <- "#A0A5E8"
review_col1   <- "#FA9161"
review_col2   <- "#F2B38F"
text_size <- 14
text_legend<-14
text_axis<-14

#----
EE<-oa_fetch(
    primary_location.source.issn="2047-2382",
    entity = "works",
    verbose = TRUE
  )

#EE$title
names(EE)



EE_classified <- EE %>%
  mutate(
    title_lc = str_to_lower(title),
    
    is_protocol = str_detect(title_lc, "\\bprotocol\\b"),
    has_review  = str_detect(title_lc, "\\bsystematic\\s+review\\b") |
      str_detect(title_lc, "\\bmixed method systematic review\\b"),
    has_map     = str_detect(title_lc, "\\bsystematic\\s+map\\b") |
      str_detect(title_lc, "\\bsystematic\\s+mapping\\b"),
    
    # Assign one primary category (protocols kept separate)
    review_type = case_when(
      is_protocol & has_map   ~ "protocol_map",
      is_protocol & has_review~ "protocol_review",
      !is_protocol & has_map  ~ "systematic_map",
      !is_protocol & has_review ~ "systematic_review",
      TRUE ~ "other_or_unclear"
    )
  )


# Manually check the classification

# A methodology for systematic mapping in environmental sciences - is a methods paper

EE_classified$review_type[2]<-"other_or_unclear"

#Human well-being impacts of terrestrial protected areas - systematic map
EE_classified$review_type[9]<-"systematic_review"
#Evaluating the biological effectiveness of fully and partially protected marine areas
EE_classified$review_type[19]<-"systematic_review"
#Are alternative livelihood projects effective at reducing local threats to specified elements of biodiversity and/or improving or maintaining the conservation status of those elements?
EE_classified$review_type[25]<-"systematic_review"

#The multifunctional roles of vegetated strips around and within agricultural fields
EE_classified$review_type[29]<-"systematic_map"

#Does delaying the first mowing date benefit biodiversity in meadowland?
EE_classified$review_type[37]<-"systematic_review"

#Bridging Indigenous and science-based knowledge in coastal and marine research, monitoring, and management in Canada
EE_classified$review_type[39]<-"systematic_map"


#What are the effects of wooded riparian zones on stream temperature?

EE_classified$review_type[47]<-"systematic_review"

#What are the impacts of urban agriculture programs on food security in low and middle-income countries?

EE_classified$review_type[49]<-"systematic_review"

#What evidence exists on the impact of governance type on the conservation effectiveness of forest protected areas? Knowledge base and evidence gaps

EE_classified$review_type[55]<-"systematic_map"

#What are the effects of agricultural management on soil organic carbon in boreo-temperate systems?
EE_classified$review_type[57]<-"systematic_map"

#Is local best? Examining the evidence for local adaptation in trees and its scale
EE_classified$review_type[65]<-"protocol_review"

#Review of the evidence base for ecosystem-based approaches for adaptation to climate change
EE_classified$review_type[99]<-"protocol_review"

#Evaluating effects of land management on greenhouse gas fluxes and carbon balances in boreo-temperate lowland peatland systems
EE_classified$review_type[102]<-"systematic_review"


#What are the effects of agricultural management on soil organic carbon (SOC) stocks?
EE_classified$review_type[103]<-"protocol_review"


#Which components or attributes of biodiversity influence which dimensions of poverty?
EE_classified$review_type[117]<-"systematic_review"


#Do mangrove forest restoration or rehabilitation activities return biodiversity to pre-impact levels?
EE_classified$review_type[122]<-"protocol_review"

#What are the environmental impacts of property rights regimes in forests, fisheries and rangelands?

EE_classified$review_type[127]<-"systematic_review"


#The evidence base for community forest management as a mechanism for supplying gloval environmental benefits and improving local welfare
EE_classified$review_type[129]<-"protocol_review"

#Realising the potential of environmental data: a call for systematic review and evidence synthesis in environmental management
EE_classified$review_type[136]<-"other_or_unclear"

#Existing evidence on the effect of urban forest management in carbon solutions and avian conservation: a systematic literature map
EE_classified$review_type[224]<-"systematic_map"

#Floodplain management in temperate regions: is multifunctionality enhancing biodiversity?
EE_classified$review_type[244]<-"protocol_review"

EE_classified |> 
  saveRDS("data/EE_class_fixed.RDS")
# Count over time
counts_over_time <- EE_classified |>
  rename(year=publication_year) |> 
  group_by(year, review_type) |>
  summarise(n = n(), .groups = "drop") |>
  complete(year, review_type, fill = list(n = 0)) |>
  arrange(year, review_type)

counts_over_time
plot_data3 <- counts_over_time |>
  filter(review_type %in% c(
    "protocol_map",
    "protocol_review",
    "systematic_map",
    "systematic_review"
  )) |>
  filter(year != 2026) |>
  mutate(
    year = as.integer(year),
    synthesis_type = case_when(
      review_type %in% c("protocol_map", "systematic_map") ~ "Map",
      TRUE ~ "Review"
    ),
    stage_type = case_when(
      str_detect(review_type, "^protocol") ~ "Protocol",
      TRUE ~ "Final report"
    ),
    label = recode(
      review_type,
      protocol_map = "Protocol map",
      protocol_review = "Protocol review",
      systematic_map = "Systematic map",
      systematic_review = "Systematic review"
    )
  )

p <- ggplot(
  plot_data3,
  aes(
    x = year, y = n,
    group = review_type,
    colour = synthesis_type,
    linetype = stage_type
  )
) +
  geom_line(linewidth = 1.4) +
  geom_point(
    aes(fill = synthesis_type),
    shape = 21, size = 3, stroke = 0.8, colour = text_col
  ) +
  scale_colour_manual(values = c("Map" = protocol_col1, "Review" = review_col1)) +
  scale_fill_manual(values = c("Map" = protocol_col1, "Review" = review_col1)) +
  scale_linetype_manual(values = c("Protocol" = "22", "Final report" = "solid")) +
  scale_x_continuous(
    breaks = sort(unique(plot_data3$year)),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
   labs(
     y = "Number of outputs",
     x = "Year"
   ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = body_font, base_size = 14) +
  theme(
    legend.position = "top",
    legend.title=element_blank(),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.grid.minor = element_blank(),
    plot.title = ggtext::element_textbox_simple(
      family = title_font,
      face = "bold",
      size = rel(2.2),
      colour = text_col
    ),
    plot.subtitle = ggtext::element_markdown(colour = text_col),
    axis.text = element_text(colour = text_col, size=text_size),
    axis.title.y = element_text(colour = text_col, size=text_axis),
    axis.title.x = element_text(colour = text_col, size=text_axis),
    legend.text = element_text(size=text_legend),
    plot.margin = margin(15, 80, 15, 15)
  )

p

ggsave(
  "outputs/plots/evidence_synthesis_outputs.png",
  plot = p,
  dpi = 300,
  bg = "white"
)
