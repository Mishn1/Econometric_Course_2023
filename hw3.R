#Home-work 3 Mishal Nawaz ; Group members: Akash and Zakaria 

# from orig_data, pick a subset
restrict_vax <- (Household_Pulse_data$RECVDVACC == "yes got vaxx" ) | (Household_Pulse_data$RECVDVACC == "no did not get vaxx")
sample_data_vaxx <- subset(Household_Pulse_data,restrict_vax)

# Count the number of males who got vaccinated
male_vaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE== "male" & sample_data_vaxx$RECVDVACC == "yes got vaxx")
# Males vaccinated: 21405
# Count the number of males who didnt vaccinated
male_unvaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE == "male" & sample_data_vaxx$RECVDVACC == "no did not get vaxx")
# Males unvaccinated: 3114

# Count the number of females who got vaccinated.
female_vaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE == "female" & sample_data_vaxx$RECVDVACC == "yes got vaxx")
# Females vaccinated :2848

# Count the number of females who didnt get vaccinated.
female_unvaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE == "female" & sample_data_vaxx$RECVDVACC == "no did not get vaxx")
# Females unvaccinated : 4336 

trans_vaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE == "transgender" & sample_data_vaxx$RECVDVACC == "yes got vaxx")
trans_unvaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE == "transgender" & sample_data_vaxx$RECVDVACC == "no did not get vaxx")
# What fraction of men got vaccinated and what fraction of women got vaccinated?
# Fraction of men that got vaccinated?
percent_MALE_vaxxed <- male_vaccinated_count/sum(male_unvaccinated_count,male_vaccinated_count)
# The percentage of Males vaccinated: 87.29965 % 

percent_FEMALE_vaxxed <- female_vaccinated_count/sum(female_vaccinated_count,female_unvaccinated_count)
# The percentage of females vaccinated: 86.78854 %

# The ratios of males vaccinated and females vaccinated are very close. 
# The fact that the percentage of males and females who were vaccinated is 
# close suggests that, proportionally, a similar fraction of males and females in the population have 
# been vaccinated. This could indicate that vaccination efforts have been somewhat evenly distributed 
# across the two genders.

percent_TRANS_vaxxed <- trans_vaccinated_count/sum(trans_vaccinated_count,trans_unvaccinated_count)

# Additionally the percentage of people who identify as trans and received the 
# vaccine is 92.27642 %

# Now we are specifically honing in on the New York region for Males, Females, and Transgenders.  
NYmale_vaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE== "male" & sample_data_vaxx$RECVDVACC == "yes got vaxx"
                               & sample_data_vaxx$EST_ST =="New York")
NYmale_unvaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE== "male" & sample_data_vaxx$RECVDVACC == "no did not get vaxx"
                                 & sample_data_vaxx$EST_ST =="New York")
Percent_NY_Males_Vaxxed <- NYmale_vaccinated_count/sum(NYmale_unvaccinated_count,NYmale_vaccinated_count)
# 94.29224 % of New York Males Got Vaccinated

NYfemale_vaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE== "female" & sample_data_vaxx$RECVDVACC == "yes got vaxx"
                                 & sample_data_vaxx$EST_ST =="New York")
NYfemale_unvaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE== "female" & sample_data_vaxx$RECVDVACC == "no did not get vaxx"
                                   & sample_data_vaxx$EST_ST =="New York")
Percent_NY_females_Vaxxed <- NYfemale_vaccinated_count/sum(NYfemale_unvaccinated_count,NYfemale_vaccinated_count)
# 92.52669 % of New York females Got Vaccinated

NY_Trans_vaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE== "transgender" & sample_data_vaxx$RECVDVACC == "yes got vaxx"
                                 & sample_data_vaxx$EST_ST =="New York")
NY_Trans_unvaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE== "transgender" & sample_data_vaxx$RECVDVACC == "no did not get vaxx"
                                   & sample_data_vaxx$EST_ST =="New York")
Percent_NY_Trans_Vaxxed <- NY_Trans_vaccinated_count/sum(NY_Trans_unvaccinated_count,NY_Trans_vaccinated_count)
# 60 % of New York transgenders Got Vaccinated


# what are the age ranges of men who got vaccinated?
#Filter data for men who got vaccinated
filteredmen_vax <- (Household_Pulse_data$RECVDVACC == "yes got vaxx" ) | (Household_Pulse_data$GENID_DESCRIBE == "male")
filtmensample_data_vaxx <- subset(Household_Pulse_data,filteredmen_vax)
# Find the age range
# Calculate age based on birth year (assuming the current year is 2023)
current_year <- 2023
filtmensample_data_vaxx$AGE <- current_year - filtmensample_data_vaxx$TBIRTH_YEAR

# Calculate the minimum and maximum ages
male_min_age <- min(filtmensample_data_vaxx$AGE) 
#minimum age: 18
male_max_age <- max(filtmensample_data_vaxx$AGE) 
#maximum age: 88

# and what are the age ranges women who got vaccinated?
filteredFEMALE_vax <- (Household_Pulse_data$RECVDVACC == "yes got vaxx" ) | (Household_Pulse_data$GENID_DESCRIBE == "female")
filtFEMALEsample_data_vaxx <- subset(Household_Pulse_data,filteredFEMALE_vax)
# Find the age range
# Calculate age based on birth year (assuming the current year is 2023)
current_year <- 2023
filtFEMALEsample_data_vaxx$AGE <- current_year - filtFEMALEsample_data_vaxx$TBIRTH_YEAR

# Calculate the minimum and maximum ages
fem_min_age <- min(filtFEMALEsample_data_vaxx$AGE)
#minimum age:18
fem_max_age <- max(filtFEMALEsample_data_vaxx$AGE)
#maximum age: 88



#What other factors could explain the difference in outcome?
#Among your list of differences in vaxx status, are there some potential confounders 
#such as age or education? What else?
#We could potentially broaden our location or classification of the location
# thereof,  to be Western states, central states, and eastern states.
#Education level certainly plays a factor depending on if you are smart 
#enough to identify a psyop(psychological operation). Income is also a ver good one.
# The level of an individuals income combined with education level.  b