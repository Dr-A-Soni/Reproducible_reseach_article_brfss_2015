
# Loading required packages for this analysis:

pacman::p_load(rio,          # File import
               here,         # File locator
               tidyverse,    # data management + ggplot2 graphics
               survey,       # for survey functions
               srvyr,      	 # dplyr wrapper for survey package
			   broom,        # Tidy model outputs
			   knitr,		# Generate Tables
			   kableExtra)  # Add advanced formatting options for tables.


# Loading the dataset:

data <- import("D:/Downloads/Data/brfss2015.csv")

# Selecting the only the pertinent variables required in this analysis:

stroke <- data %>% select(X_PSU, X_STSTR,X_LLCPWT,
				X_AGE_G, X_RACEGR3,X_EDUCAG,
				SEX,CVDSTRK3,CVDINFR4,DIABETE3,
				X_RFSMOK3,X_RFDRHV5,X_BMI5CAT,
				HLTHPLN1, BPHIGH4)

# Data tidying and wrangling for further analysis.

stroke <- stroke %>% filter(CVDSTRK3 <= 2)

stroke <- stroke %>% filter(X_RACEGR3 <=5)

# 1) Recoding all the variables to the required format and levels:

	# a) X_AGE_G : Age variable:

stroke$X_AGE_G <- as.factor(recode(stroke$X_AGE_G,
			"1" = "18-44",
			"2" = "18-44",
			"3" = "18-44",
			"4" = "45-54",
			"5" = "55-64",
			"6" = ">=65"))
stroke$X_AGE_G <- factor(stroke$X_AGE_G, levels = c("18-44", "45-54","55-64",">=65"))

	# b)  X_RACEGR3: Race/ Ethnicity variable:

stroke$X_RACEGR3 <- as.factor(recode(stroke$X_RACEGR3,
			"1" = "White only, Non-Hispanic",
			"2" = "Black only, Non-Hispanic",
			"3" = "Other race only, Non-Hispanic",
			"4" = "Multiracial, Non-Hispanic",
			"5" = "Hispanic"))

stroke$X_RACEGR3 <- factor(stroke$X_RACEGR3,levels = c(
		"White only, Non-Hispanic",
		"Black only, Non-Hispanic",
		"Other race only, Non-Hispanic",
		"Multiracial, Non-Hispanic",
		"Hispanic"))



	# c) SEX: Sex variable:

stroke$SEX <- as.factor(recode(stroke$SEX, "1" = "Male", "2" = "Female"))

stroke$SEX <- factor(stroke$SEX, levels = c("Male", "Female"))

	# Make a separate subset for the proportion counting from data till here:
	# Before filtering the data set lets make a subset data which will be used for calculating proportion

st <- stroke

	# d) X_EDUCAG: Education levels variable:

stroke <- stroke %>% filter(X_EDUCAG < 9)

stroke$X_EDUCAG <- as.factor(recode(stroke$X_EDUCAG,
	"1" = "Under High School",
	"2" = "High School",
	"3" = "College Candidate",
	"4" = "College"))


	# e) Filtering remaining variable whose response was Yes and No only and excluding other responses:

#stroke <- stroke %>% filter(CVDSTRK3 <= 2)

stroke <- stroke %>% filter(CVDINFR4 <= 2)
stroke <- stroke %>% filter(DIABETE3 <=4)
stroke <- stroke %>% filter(X_RFSMOK3 <9)
stroke <- stroke %>% filter(X_RFDRHV5 <9)
stroke <- stroke %>% filter(HLTHPLN1 < 7)
stroke <- stroke %>% filter(BPHIGH4 < 7)


	# All the binary variables are converted in to binary output of "0" for No  and "1" for Yes responses:

stroke$CVDSTRK3 <- as.factor(recode(stroke$CVDSTRK3, "1" = "1", "2" = "0", .default = "0", .missing = "0"))
stroke$CVDINFR4 <- as.factor(recode(stroke$CVDINFR4, "1" = "1", "2" = "0",.default = "0", .missing = "0"))
stroke$DIABETE3 <- as.factor(recode(stroke$DIABETE3, "1" = "1", "2" = "0", "3" = "0", "4" = "0",.default = "0", .missing = "0"))
stroke$X_RFSMOK3 <- as.factor(recode(stroke$X_RFSMOK3, "1" = "0", "2" = "1",.default = "0", .missing = "0"))
stroke$X_RFDRHV5 <- as.factor(recode(stroke$X_RFDRHV5, "1" = "0", "2" = "1",.default = "0", .missing = "0"))
stroke$HLTHPLN1 <- as.factor(recode(stroke$HLTHPLN1, "1" = "1", "2" = "0",.default = "0", .missing = "0"))
stroke$X_BMI5CAT <- as.factor(recode(stroke$X_BMI5CAT, "1" = "0", "2" = "0", "3" = "0", "4" = "1",.default = "0", .missing = "0"))
stroke$BPHIGH4 <- as.factor(recode(stroke$BPHIGH4, "1" = "1", "2" = "0", "3" = "0", "4" = "0" ))




	# Data Analysis:

	# 1) Part 1: Descriptive statistics on the characteristic of the populations and Chi-square analysis between Race/Ethnicity and other binary variables apart from stroke:

	# Writing a function to pass all the variable which would give use the summary count for each variable.

stroke_count <- function(data,var){
	res = vector("list", length(var))
	names(res)  = var
 for(i in var) {
	res[[i]] = data %>% group_by(X_RACEGR3, !!sym(i))%>%
			summarise(count = n(), .groups = "drop") %>%
			pivot_wider(names_from = X_RACEGR3, values_from = count) %>%
			mutate(Health_variable = i)
}
	rest = bind_rows(res)
	rest <- rest[-c(9,10,15,20,21,26,29,32,37,38,39,42,43),]
	rest = rest %>% select(7, 2:6)

	return(rest)
}

# st dataset will be used on this function to calculate counts.

Hv <- c("X_AGE_G","SEX","HLTHPLN1","X_EDUCAG","BPHIGH4","X_BMI5CAT", "X_RFDRHV5", "X_RFSMOK3", "DIABETE3","CVDINFR4")

stroke_N <- stroke_count(st,Hv)

b <- stroke_N %>% slice(14:16)%>% select(-1) %>% summarise(across(everything(), sum))
c <- stroke_N %>% slice(17:19) %>% select(-1)%>% summarise(across(everything(), sum))
d <- stroke_N %>% slice(26:28) %>% select(-1)%>% summarise(across(everything(), sum))
ab <- bind_rows(b,c,d)
ab$Health_variable <- c("Hypertension_No","Not_Obese", "Diabetes_No")
stroke_N <- bind_rows(stroke_N,ab)
stroke_N <- stroke_N[-c(14:16,17:19,26:28),]

stroke_N <- stroke_N[c(1:12,22,13,23,14,15:18,24,19:21),]
stroke_N$Health_variable <- c("'18-44'", "'45-54'", "'55-64'", "'>=65'",
							 "SEX_Male","SEX_Female",
							 "HealthPlan_Yes","HealthPlan_No",
							 "Under High School", "High School", "College Candidate", "College",
							"Hypertension_No", "Hypertension_Yes",
							"Not_Obese", "Obese", "Alcoholism_No", "Alcoholism_Yes",
							"Smoking_No", "Smoking_Yes", "Diabetes_No", "Diabetes_Yes", "MI_Yes", "MI_No")

# half part of one table is Proportion_count

Proportion_count_Stroke <- stroke_N


# Now to calculate second part of table1 , it is proportion calculation from the population after strtification and weighting.


# Survey Analysis: Survey Designing for Stratification and weighting of the data for further analysis:

stroke_design <- svydesign(ids = ~1, strata = ~X_STSTR, weights = ~X_LLCPWT, data = stroke)
# Since survey packages produces results that are not compatible with some of other packages used previously  for data manipulation, it is required to be converted in to more suitable format using srvyr package:

st_dsgn <- as_survey(stroke_design)


# Calculating the proportion of the population participated, through the following function:

st_prop_count <- function(design, var){

	res = vector("list", length(var))
	names(res) = var

	for (i in var){

		res[[i]] = design %>% group_by(X_RACEGR3, !!sym(i)) %>%
					summarise(proportion = survey_mean(vartype = "se")) %>%
					mutate(Health_variable = i) %>% select(1:3,5) %>% mutate(proportion = proportion*100)%>%
					pivot_wider(names_from = X_RACEGR3,
						values_from = proportion)

	}
		res = bind_rows(res)
		res = res %>% select(2:7)
	return(res)
}

Proportion <- st_prop_count(st_dsgn,Hv)

# Final table :

# Making sure Health variables are aligned in both tables:

Proportion <- Proportion[c(1:6,8,7,12,11,10,9,13:22,24,23), ]

Proportion$Health_variable <- Proportion_count_Stroke$Health_variable


colnames(Proportion_count_Stroke)[2:6] <- c("White_count", "Black_count","Other_count", "Multi_count","Hisp_count")

colnames(Proportion)[2:6] <- c("White_Per", "Black_Per","Other_per", "Multi_per","Hisp_per")

#cbind both the tables showing count and percentage:
Stroke_Prop <- left_join(Proportion_count_Stroke, Proportion, by = "Health_variable")


Table_1 <- Stroke_Prop %>% mutate("white" = paste0(White_count," (",round(White_Per,1),"%)"),
                        "Black" = paste0(Black_count," (",round(Black_Per,1),"%)"),
                        "Other" = paste0(Other_count, " (",round(Other_per,1),"%)"),
                        "Multi" = paste0(Multi_count, " (",round(Multi_per,1),"%)"),
                        "Hisp" = paste0(Hisp_count," (", round(Hisp_per,1),"%)")) %>% select(1,12:16)




#  chi-square test :

chi_fun <- function(var, design){

    model <- vector("list", length(var))
    names(model) = var
    for (i in var){

		formula = as.formula(paste0("~X_RACEGR3 +", i))


        model[[i]] = tidy(svychisq(formula,design)) %>% mutate(variable = i)
        }
model = bind_rows(model)
model$p_value <- sprintf("%.2e",model$p.value)
model <- model %>% select(variable,p_value)
return(model)
}

stroke_Chi <- chi_fun(Hv,st_dsgn)stroke

Variable <- c("X_AGE_G" ,  "X_AGE_G"  , "X_AGE_G"  , "X_AGE_G" ,  "SEX","SEX" ,"HLTHPLN1","HLTHPLN1" , "X_EDUCAG","X_EDUCAG","X_EDUCAG","X_EDUCAG", "BPHIGH4","BPHIGH4" ,  "X_BMI5CAT","X_BMI5CAT" ,"X_RFDRHV5" ,"X_RFDRHV5" ,"X_RFSMOK3", "X_RFSMOK3", "DIABETE3", "DIABETE3","CVDINFR4","CVDINFR4")

# combine chi square result with th stroke Population Table:

Table_1$HV <- Variable

Table_1 <- left_join(Table_1,stroke_Chi, by = c("HV" = "variable"))
Table_1 <-  Table_1 %>% select(-HV)

colnames(Table_1) <- c("Variables", "White only,Non-Hispanic", "Black only, Non-Hispanic", "Other race only, Non-Hispanic", "Multiracial, Non-Hispanic", "Hispanic", "P-value")



# Kable styling:


TAB_1 <- 	Table_1 %>% kable(caption = "Table 1: The Association between the Race/Ethnicity and sme characteristics among the US adult population, BRFSS 2015", align = "c") %>%
    		    kable_styling(bootstrap_options = c("striped","hover","condensed","bordered")) %>%
        		pack_rows("Age", 1,4)%>%
        		pack_rows("Sex",5,6)%>%
        		pack_rows("Health Coverage", 7,8)%>%
        		pack_rows("Education", 9,12)%>%
        		pack_rows("Hypertension",13,14) %>%
        		pack_rows("Obesity",15,16)%>%
        		pack_rows("ALcoholism",17,18 )%>%
        		pack_rows("Smoking", 19,20)%>%
       			pack_rows("Diabetes", 21,22) %>%
        		pack_rows("Myocardial Infarction",23,24) %>%
        		add_header_above(c("Characteristics" = 1, "Race/Ethnicity" = 6))

print(TAB_1)

# Part: 2 of Odds Ratio:
# 2i : unadjusted odds ratio:

# Required variables :

HV <- c("X_RACEGR3","X_AGE_G","SEX","HLTHPLN1","X_EDUCAG","BPHIGH4","X_BMI5CAT", "X_RFDRHV5", "X_RFSMOK3", "DIABETE3","CVDINFR4")


var <- c("X_RACEGR3","X_RACEGR3","X_RACEGR3","X_RACEGR3","X_RACEGR3",
				"X_AGE_G", "X_AGE_G","X_AGE_G","X_AGE_G",
				"SEX","SEX","HLTHPLN1","HLTHPLN1", "X_EDUCAG", "X_EDUCAG","X_EDUCAG","X_EDUCAG",
				"BPHIGH4","BPHIGH4", "X_BMI5CAT","X_BMI5CAT", "X_RFDRHV5","X_RFDRHV5",
				"X_RFSMOK3","X_RFSMOK3", "DIABETE3","DIABETE3","CVDINFR4","CVDINFR4")

# Glm function to apply for variables all together and extract the Coefficients and Confidence Interval:


glm_stroke <- function(data ,design, var,HV){

	# Fit Models:

	model = vector("list",length(HV))
	names(model) = HV
	for(i in HV){
		formula = as.formula(paste0("CVDSTRK3 ~ ", i))
		model[[i]] = svyglm(formula, design = design,data = data, family = quasibinomial)
	}

	#Extract Coefficients and Confidence Intervals:

	Coef = vector("list", length(var))
	CIs = vector("list",length(var))
	names(Coef) = names(model)
	names(CIs)= names(model)

	for (i in names(model)){
		Coef[[i]] = coef(model[[i]])
		CIs[[i]] = confint(model[[i]])
	}
	#Combine coefficients and confidence interval into data frames:

	Coef_df <- bind_rows(lapply(Coef, function(x) data.frame(Unadjusted_OR = x)))
	CI_df <- bind_rows(lapply(CIs, function(x) data.frame(Lower_ci = x[,1], Upper_ci = x[,2])))

	#Combine into one data frame:
	REST <- bind_cols(Coef_df,CI_df)
return(REST)
}

# New variable to store all result:

st_glm_OR <- glm_stroke(stroke,st_dsgn,var,HV)

st_glm_OR$Health_variable <- row.names(st_glm_OR)

stroke_glm_OR <- st_glm_OR %>%  select(4,everything())


stroke_glm_OR[,c(2:4)] <- lapply(stroke_glm_OR[,c(2:4)], exp)

stroke_OR <- stroke_glm_OR %>% mutate(Unadjusted_OR = paste0(round(Unadjusted_OR,2),"(",round(Lower_ci,2),"-",round(Upper_ci,2),")")) %>%
select(1,2)


## Part 2: analysis

			# 2ii : Adjusted odds ratio:

glm_adj <- svyglm(CVDSTRK3 ~ X_RACEGR3+ X_AGE_G+SEX+HLTHPLN1+
								X_EDUCAG+BPHIGH4+X_BMI5CAT+
								X_RFDRHV5+X_RFSMOK3+DIABETE3+
								CVDINFR4,family = quasibinomial,data = stroke,design= st_dsgn)


stroke_adj <- as.data.frame(cbind(glm_adj$coef, confint(glm_adj)))

colnames(stroke_adj) <- c("Adjusted_OR", "Lower_ci", "Upper_ci")

stroke_adj$Health_variable <- row.names(stroke_adj)
stroke_adj <- stroke_adj %>% select(4,everything())

stroke_adj[,c(2:4)] <- lapply(stroke_adj[,c(2:4)], exp)


st_adj <- stroke_adj %>% mutate(Adjusted_OR = paste0(round(Adjusted_OR,2),"(", round(Lower_ci,2),"-", round(Upper_ci,2),")")) %>% select(1,2)


## Combine adjusted and Unadjusted.

TABLE_2 <- left_join(stroke_OR, st_adj, by = "Health_variable")


TABLE_2$Unadjusted_OR[is.na(TABLE_2$Adjusted_OR)] <- "Ref"
TABLE_2$Adjusted_OR[is.na(TABLE_2$Adjusted_OR)]  <- "Ref"

TABLE_2$Health_variable <- c("White only, Non-Hispanic","Black only, Non-Hispanic","Other race only, Non-Hispanic","Multiracial, Non-Hispanic","Hispanic","18-44","45-54","55-64",">=65","Sex_Male","SEX_Female","Health_coverage_No","Health_coverage_Yes","College","College Candidate","High School","Under High School","Hypertension_No","Hypertension_Yes","Not_Obese","Obese","Alcoholism_No","Alcoholism_Yes","Smoking_No","Smoking_Yes","Diabetes_No","Diabetes_Yes","Myocardial_Infarction_No","Myocardial_Infarction_Yes")


# Kable styling for the final Table 2:


TAB_2 <- TABLE_2 %>% kable(caption = "Table_2: The Unadjusted and Adjusted (Odds Ratio) association between the prevalence of Stroke and some characteristics such as Race/Ethnicity, sex, and other confounding variables among the US adults population, BRFSS 2015.", align = "c") %>%
    				kable_styling(bootstrap_options = c("striped","hover","condensed","bordered"))%>%
					pack_rows("Race/Ethnicity", 1,5) %>%
					pack_rows("Age", 6,9)%>%
					pack_rows("Sex",10,11)%>%
					pack_rows("Health Coverage", 12,13)%>%
					pack_rows("Education", 14,17)%>%
					pack_rows("Hypertension",18,19) %>%
					pack_rows("Obesity",20,21)%>%
					pack_rows("ALcoholism",22,23 )%>%
					pack_rows("Smoking", 24,25)%>%
					pack_rows("Diabetes", 26,27) %>%
					pack_rows("Myocardial Infarction",28,29) %>%
					row_spec(1:nrow(TABLE_2)), color = "black")

print(TAB_2)






# Note:

# Below code is for better visual (same but more aesthetic).


# Table 1:

a11 %>% kable(caption = "The Association between the Race/Ethnicity and Age,Sex, and some more characteristics among the US adults population, BRFSS_2015", align = "c",row.names = FALSE) %>%
    kable_styling(full_width = FALSE,bootstrap_options = c("striped","hover","condensed","bordered")) %>%
    add_header_above(c("Characteristics" = 1, "Race/Ethnicity" =6), bold = TRUE) %>%
    pack_rows("Age",1,4,bold =TRUE,color = "black") %>%
    pack_rows("Sex",5,6,bold = TRUE,color = "black")  %>%
	pack_rows("Health Coverage",7,8,bold = TRUE,color = "black")   %>%
    pack_rows("Education",9,12,bold = TRUE,color = "black")  %>%
	pack_rows("Hypertension",13,14,bold = TRUE,color = "black") %>%
    pack_rows("Obesity",15,16,bold = TRUE,color = "black")  %>%
	pack_rows("Alcoholism",17,18, bold = TRUE,color = "black")  %>%
    pack_rows("Smoking",19,20,bold = TRUE,color = "black")  %>%
	pack_rows("Diabetes",21,22, bold = TRUE,color = "black")  %>%
    pack_rows("Myocardial Infarction",23,24, bold =TRUE)  %>%
	row_spec(0,bold = TRUE,color = "black") %>%
	row_spec(1:4, bold = TRUE,color = "black") %>%
	row_spec(5:6 ,bold = TRUE,color = "black")%>%
	row_spec(7:8 ,bold = TRUE,color = "black") %>%
	row_spec(9:12,bold = TRUE,color = "black")%>%
	row_spec(13:14 ,bold = TRUE,color = "black")%>%
	row_spec(15:16 ,bold =TRUE,color = "black") %>%
	row_spec(17:18 ,bold = TRUE,color = "black") %>%
	row_spec(19:20,bold = TRUE,color = "black") %>%
	row_spec(21:22 ,bold = TRUE,color = "black")%>%
	row_spec(23:24 ,bold =TRUE,color = "black") %>%
    footnote(general = "The p-value threshold is set at 0.05, Chi-square test is performed ")

	# Table: 2:


a22 %>% kable(caption = "The unadjusted and Adjusted Odd Ratio showing the association between the prevalence of stroke and some characteristics among the US adult population, BRFSS_2015 ", align = "c") %>%
    kable_styling(full_width = FALSE,bootstrap_options = c("striped","bordered","condensed","hover"),position = "center") %>%
    add_header_above(c("Characteristics " = 1, "Odds Ratio" = 2), align = "c", color = "white", background = "purple") %>%
    pack_rows("Race/Ethnicity", 1,5,color = "white", background = "purple")%>%
    row_spec(0, background = "gray", color = "black") %>%
    pack_rows("Age", 6,9,background = "purple", color = "white") %>%
    row_spec(1:5,background = "grey", color = "black", bold = TRUE)%>%
    pack_rows("Sex", 10,11,background = "purple", color = "white") %>%
    pack_rows("Health Coverage", 12,13,background = "purple", color = "white") %>%
    pack_rows("Education",14,17,background = "purple", color = "white")%>%
    pack_rows("Hypertension", 18,19,background = "purple", color = "white") %>%
    pack_rows("Obesity",20,21,background = "purple", color = "white") %>%
    pack_rows("Alcoholism",22,23,background = "purple", color = "white") %>%
    pack_rows("Smoking",24,25,background = "purple", color = "white") %>%
    pack_rows("Diabetes",26,27,background = "purple", color = "white") %>%
    pack_rows("Myocardial Infarction",28,29,background = "purple", color = "white") %>%
    row_spec(6:9, background = "grey", color = "black", bold = TRUE) %>%
    row_spec(10:11, background = "grey", color = "black", bold = TRUE) %>%
    row_spec(12:13, background = "grey", color = "black",bold = TRUE) %>%
    row_spec(14:17, background = "grey", color = "black",bold = TRUE) %>%
    row_spec(18:19, background = "grey", color = "back", bold = TRUE) %>%
    row_spec(20:21, background = "grey", color = "black",bold = TRUE) %>%
    row_spec(22:23, background = "grey", color = "black", bold = TRUE) %>%
    row_spec(24:25, background = "grey", color = "black",bold = TRUE) %>%
    row_spec(16:27, background = "grey", color ="black",bold = TRUE) %>%
    row_spec(28:29, background = "grey", color = "black", bold = TRUE) %>%
    footnote(general = "The 95% confidence interval is displayed in the bracket")

# GrVIZ Diagram code:

grViz("
        digraph Exclusion_criteria {

           graph [layout = dot,
                 rankdir = LR,
                 overlap = false,
                 fontsize = 12];

            node [shape = rectangle,
                  fixedsize = false,
                  width = 1];

            one [label = '441,456 have\ncompleted the\nBRFSS survey'];
            two [label = '440,166 had\nno missing\ninformation about\nstroke'];
            three [label = '432,814 included in the study\nwith no missing information\nabout stroke\nand Race/Ethnicity'];
            four [label = 'Population with\nmissing information\nabout stroke (n = 1290)'];
            five [label = 'Population with\nmissing information\nabout Race/Ethnicity (n = 7352)'];

            // Define edges
            one -> four [style = dashed];
            two -> five [style = dashed];
            four -> two[style = dashed];



             five -> three [style = dashed];

            // Ensure layout
            {rank = same; one; two;four}
            {rank = same; three; five}
        }
    ")