allCovidDATAandPEMPH <- rbind(allCovidDATA, ALLPEMPH, ALLPEMPHIGOID)
allCovidVAXandPEMPH <- rbind(allCovidVAX, ALLPEMPHIGOIDVAX, ALLPEMPHVAX)
Vitiligo <- allCovidDATAandPEMPH %>%
  filter(str_detect(SYMPTOM_TEXT, "Vitiligo") | str_detect(SYMPTOM_TEXT, "vitiligo") | str_detect(SYMPTOM_TEXT, "VITILIGO") | str_detect(SYMPTOM_TEXT, "itiligo"))
#save.image("VitiligoStudy.RData")
#load("VitiligoStudy.RData")
vitTESTthing <- allCovidDATAandPEMPH %>%
  filter(str_detect(SYMPTOM_TEXT, "Vitiligo") | str_detect(SYMPTOM_TEXT, "vitiligo") | str_detect(SYMPTOM_TEXT, "VITILIGO") | str_detect(SYMPTOM_TEXT, "itiligo"))
vitTESTthing$SYMPTOM_TEXT[100:200]
vitTESTthing$SYMPTOM_TEXT[200:216]

VitiligoUnique <- unique(Vitiligo$VAERS_ID)

allCovidDataNoVitiligo <- allCovidDATAandPEMPH %>%
  filter(!(VAERS_ID %in% VitiligoUnique))
allCovidDataNoVitiligoUnique <- unique(allCovidDataNoVitiligo$VAERS_ID)

VitiligoVax <- allCovidVAX %>%
  filter(VAERS_ID %in% VitiligoUnique)

allCovidVaxNoVitiligo <- allCovidVAXandPEMPH %>%
  filter(VAERS_ID %in% allCovidDataNoVitiligoUnique) %>%
  arrange(VAERS_ID, desc(VAX_DOSE_SERIES)) %>%
  group_by(VAERS_ID) %>%
  slice_head(n = 1) %>%
  ungroup()

VitiligoVax <- VitiligoVax %>%
  filter(VAERS_ID %in% VitiligoUnique) %>%
  arrange(VAERS_ID, desc(VAX_DOSE_SERIES)) %>%
  group_by(VAERS_ID) %>%
  slice_head(n = 1) %>%
  ungroup()


