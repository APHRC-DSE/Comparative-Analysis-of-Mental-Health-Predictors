***********MENTTAL HEALTH DATA*********************************************

******************COMMUNITY ANALYSIS*********************************************
clear all
log using communitycleaning.dta, replace
import excel "D:\Mental Health\Mental_Health_Survey_-_all_versions_-_labels_-_2024-10-29-04-28-10.xlsx", sheet("Mental Health Survey") firstrow

***Keeping only students in the dataset
encode YearofStudy, gen (study)
keep if study ==. //2,805 observations dropped 

drop SECT* // Dropping section titles
drop BN-CC //Dropping variables with no observations

mdesc
****SOCIODEMOGRAPHIC VARIABLES

***Age of the respondent
desc Age
**Age stored as string, converted to numeric
replace Age = "" if substr(Age, 1, 3) == "011" ///
			|       substr(Age, 1, 3) == "072" ///
			|	    substr(Age, 1, 3) == "074"
tab Age, m
replace Age = "60" in 2493
destring Age, gen(age) ignore("," "see" "years" "yrs" "-") // Some observations had , and o as part of the entries
replace age = . if age ==228 ///
			| age == 3 ///
			| age == 228 ///
			| age ==5 // Changing outliers to missing
sum age, detail
recode age (min/24 =1 "<25 years") (25/34=2 "25-34 years") (35/44=3 "35-44 years") (45/54=4 "45-54 years") (55/64 =5 "55-64 years") (65/max=6 "> 65 years"), gen (Agegrps) 
// generating catergories for age
lab var Agegrps "AGE GROUPS"
tab Agegrps


***Gender
tab Gender
encode Gender, gen (gender) // gender entered as a string
recode gender (2=2 "Male") (1=1 "Female") (3 4=.), gen (sex) // prefer not to say changed to missing
lab var sex "Sex of the participant"
tab sex, m


**Marital status
tab MaritalStatus
encode MaritalStatus, gen(marital_status)
recode marital_status (2 3 4 =2 "single/widowed/separated") (1 =1 "Married"), gen (marstat) 
lab var marstat "Marital status"
tab marstat

**Education level
tab LevelofEducation, nol
encode LevelofEducation, gen (edu)
tab edu, nol
recode edu (4=1 "No formal education") (6=2 "Primary level") (7=3 "Secondary level") (1 2 3 5 = 4 "Tertiary level"), gen (Education)
lab var Education "Education level"
tab Education

**Head of the household
**# Bookmark #1
tab HouseholdHead
tab HouseholdHead, nol
encode HouseholdHead, gen (HH_head) // variable stored as a string
recode HH_head (2=2 "Yes") (1=1 "No"), gen (HHead)
lab var HHead "Head of the household"
tab HHead


/***Village
tab village
encode village, gen (Village)
tab Village
**tab 8*/

/***Income // variable ignored due to inconsistencies in data entry
desc income
destring income, gen (Income)
sum Income, det*/ 

**SOCIOECONOMIC FACTORS

**Activity level
tab Howoftendoyouengageinsocia
encode Howoftendoyouengageinsocia, gen (activity)
recode activity (2 =1 "Never") (4=2 "Rarely") (3=3 "Occasionally") (1=4 "Frequently"), gen (Activitylevel)
lab var Activitylevel "Activity level"
tab Activitylevel, m

**Social Media
tab Howmuchtimedoyouspendonso
encode Howmuchtimedoyouspendonso, gen (SM)
recode SM (4=1 "< 1 hour") (1=2 "1-3 hours") (2=3 "3-5 hours") (3=4 "> 5 hours"), gen (SocialMedia)
lab var SocialMedia "Social Media Use"
tab SocialMedia, m


**tab How supportive do you |
tab Howsupportivedoyoufindyour
encode Howsupportivedoyoufindyour, gen (supportive)
recode supportive (2=1 "Not suppotive") (3=2 "Slightly supportive") (1=3 "Moderately Supportive") (4=4 "Supportive") (5=5 "Very Supportive"), gen (Supportive)
lab var Supportive "family support"
tab Supportive

 
/***Sources of Income //98.8% missing
tab Whatisyourprimarysourceoff
encode Whatisyourprimarysourceoff, gen (SourceIncome)
recode SourceIncome (1=1 "Family") (2=2 "Loan") (4=3 "Personal") (5=4 "Scholarship") (3=5 "Other"), gen (Incomesource)
lab var Incomesource "Sources of Income"
tab Incomesource
 */
 
/***Difficulties // 98.6% missing
tab Haveyoufacedfinancialdifficu
tab Haveyoufacedfinancialdifficu, m
encode Haveyoufacedfinancialdifficu, gen (difficult)
recode difficult (1=1 "No") (2=2 "Yes") , gen (Difficult)
lab var Difficult "Financial difficulty in time at University"
tab Difficult*/ 
 
***If yes, how many times //99.3% missing
/*tab Ifyeshowoftenhavethesefin, m
encode Ifyeshowoftenhavethesefin, gen (howoften)
recode howoften (2=1 "Never") (4=2 "Rarely") (3=3 "Occasionally") (1=4 "Frequently"), gen (HowOften)
lab var HowOften "How often have you faced financial difficulties"
tab HowOften*/
 

/**Awareness of mental health facilities  // 98.9% missing
tab Areyouawareofthementalheal, m
encode Areyouawareofthementalheal, gen (awareness)
recode awareness (1=1 "No") (2=2 "Yes"), gen (Awareness)
lab var Awareness "Awareness of mental health services available in the university"
tab Awareness*/

**Used mental healths ervices in the last 12 months
/*tab Overthepast12monthshaveyo, m // 98.9% missing
encode Overthepast12monthshaveyo, gen (MHservices)
recode MHservices (1=1 "No") (2=2 "Yes"), gen (MHServices)
lab var MHServices "Used mental health services in the last 12 months"
tab MHServices*/

***How would you rate the effectiveness of these mental health services?
tab Howwouldyouratetheeffective, m // 99.8% missing


***Experienced symptoms of depression, anxiety, or stress
/*tab AE, m  // 98.7% missing
encode AE, gen (symptoms)
recode symptoms (1=1 "No") (2=2 "Yes"), gen (Symptoms)
lab var Symptoms "Expreienced symptoms of depression, anxiety or stress"
tab Symptoms*/

***if yes
/*tab Ifyesdidyouseekanyprofess, m // 99.5% missing
encode Ifyesdidyouseekanyprofess, gen (seekhelp)
recode seekhelp (1=1 "No") (2=2 "Yes"), gen (SeekHelp)
lab var SeekHelp "Did you seek help"
tab SeekHelp*/



**Generalized Anxiety Disorder 7-item (GAD-7)
***The Generalized Anxiety Disorder 7-item (GAD-7) is a easy to perform initial screening tool for generalized anxiety disorder

**GAD variables
rename Feelingnervousanxiousoron nervous
rename Notbeingabletostoporcontro control_worrying
rename Worryingtoomuchaboutdifferen worry_too_much
rename Troublerelaxing  relaxing
rename Beingsorestlessthatitshard restless
rename BecomingeasilyannoyedorIrrit irritable
rename Feelingafraidasifsomethinga afraid

foreach var of varlist nervous - afraid {
	encode `var', gen (`var'_1)
}

foreach var of varlist nervous_1 - afraid_1 {
	recode `var' (2=0 "not at all") (4=1 "several days") (3=2 "more than half the days") (1=3 "Nearly everyday"), gen (`var'_2)
}

lab var nervous_1_2 "Feeling nervous, anxious, or on edge"
lab var control_worrying_1_2 "Not being able to stop or control worrying"
lab var worry_too_much_1_2 "Worrying too much about different things"
lab var relaxing_1_2 "Trouble relaxing"
lab var restless_1_2 "Being so restless that it's hard to sit still"
lab var irritable_1_2 "Becoming easily annoyed or Irritable"
lab var afraid_1_2 "Feeling afraid as if something awful might happen"

rename nervous_1_2 Nervous
rename control_worrying_1_2 Worrying
rename worry_too_much_1_2 Things
rename relaxing_1_2 Trouble
rename restless_1_2 Restless
rename irritable_1_2 Irritable
rename afraid_1_2 Afraid

foreach var of varlist Nervous-Afraid {
  tab `var', m
   }
   
count if Nervous==. | Worrying==. | Things==. | Trouble==.| Restless==.| Irritable==.| Afraid ==.
**84 Participants miss atleast one of the components required to calculate GAD score

egen GAD = rowtotal (Nervous-Afraid)
replace GAD =. if Nervous==. | Worrying==. | Things==. | Trouble==.| Restless==.| Irritable==.| Afraid ==.

***GAD7 SCORES
sum GAD, detail

**Generating GAD categories 
***Computing GAD-7 scores: Score 0-4: Minimal Anxiety. Score 5-9: Mild Anxiety. Score 10-14: Moderate Anxiety. Score greater than 15: Severe Anxiety.
recode GAD (min/4 =1 "Minimal") (5/9=2 "Mild") (10/14= 3 "Moderate ") (15/max=4 "Severe") (.=.), gen (GADcategory)
lab var GADcategory "GAD Category"
tab GADcategory, m




**PHQ9 QUESTIONNAIRE
***The PHQ-9 is a multipurpose instrument for screening, diagnosing, monitoring and measuring the severity of depression.

rename Littleinterestorpleasureind interest
rename Feelingdowndepressedorhope hopeless
rename Troublefallingorstayingaslee sleeping
rename Feelingtiredorhavinglittlee tired
rename Poorappetiteorovereating appetite
rename Feelingbadaboutyourselfor family
rename Troubleconcentratingonthings concentrating
rename Movingorspeakingsoslowlytha moving
rename Thoughtsthatyouwouldbebette thoughts

foreach var of varlist interest - thoughts {
	encode `var', gen (`var'_1)
}

foreach var of varlist interest_1 - thoughts_1 {
	recode `var' (3=0 "not at all") (4=1 "several days") (1=2 "more than half the days") (2=3 "Nearly everyday"), gen (`var'_2)
}

lab var interest_1_2 "Little interest or pleasure in doing things"
lab var hopeless_1_2 "Feeling down, depressed, or hopeless"
lab var sleeping_1_2 "Trouble falling or staying asleep, or sleeping too much"
lab var tired_1_2 "Feeling tired or having little energy"
lab var appetite_1_2 "Poor appetite or overeating"
lab var family_1_2 "Feeling bad about yourself — or that you are a failure or have let yourself or your family down"
lab var concentrating_1_2 "Trouble concentrating on things, such as reading the newspaper or watching television"
lab var moving_1_2 "Moving or speaking so slowly that other people could have noticed? Or the opposite — being so fidgety or restless that you have been moving around a lot more than usual"
lab var thoughts_1_2 "Thoughts that you would be better off dead or of hurting yourself in some way"

rename interest_1_2 Interest
rename hopeless_1_2 Hopeless
rename sleeping_1_2 Sleeping
rename tired_1_2 Tired
rename appetite_1_2 Appetite
rename family_1_2 Family
rename concentrating_1_2 Concentrating
rename moving_1_2 Moving
rename thoughts_1_2 Thoughts

foreach var of varlist Interest-Thoughts {
  tab `var', m
   }


****PHQ-9 ANALYSIS 
**PHQ-9 SCORES
egen PHQ = rowtotal(Interest-Thoughts), m
sum PHQ, det

count if Interest==. | Hopeless==. | Sleeping==. | Tired==.| Appetite==.| Family ==. |Concentrating==.| Moving==.| Thoughts==.

**54 Participants missing at least one variable required to calculate a PHQ score

***PHQ-9 categories: 0-4 None/minimal; 5-9 Mild; 10-14 Moderate; 15-19 Moderately severe; 20-27 Severe
replace PHQ=. if Interest==. | Hopeless==. | Sleeping==. | Tired==.| Appetite==.| Family ==. |Concentrating==.| Moving==.| Thoughts==.

***PHQ-9 categories: 0-4 None/minimal; 5-9 Mild; 10-14 Moderate; 15-19 Moderately severe; 20-27 Severe
recode PHQ (min/4=1 "None/minimal") (5/9=2 "Mild") (10/14=3 "Moderate") (15/19=4 "Moderate severe") (20/27=4 "Severe"), gen (PHQcat)
lab var PHQcat "PHQ Category"
tab PHQcat




***Psychosis Screening Questionnaire
**Hypomania Probe
tab HypomaniaprobeOverthepasty
tab Wasthereanobviousreasonfor, m //55% missing
lab var HypomaniaprobeOverthepasty "you felt very happy indeed without a break for days on end"
lab var Wasthereanobviousreasonfor "Hypomania"
encode HypomaniaprobeOverthepasty, gen (Hypomania)
encode Wasthereanobviousreasonfor, gen (Hypomania2)


**Thought insertion
tab ThoughtinsertionOverthepast
tab Ifyesdidthiscomeaboutina, m //75% missing
lab var ThoughtinsertionOverthepast "felt that your thoughts were directly interfered with or controlled by some outside force or person"
lab var Ifyesdidthiscomeaboutina "Though insertion"
encode ThoughtinsertionOverthepast, gen (Thoughtinsertion)
encode Ifyesdidthiscomeaboutina, gen (Thoughtinsertion2)

**Paranoia Probe
tab ParanoiaProbeOverthepastye
tab Ifyeshavetherebeentimeswh, m //70% missing
lab var ParanoiaProbeOverthepastye "u felt that people were against youu felt that people were against you"
lab var Ifyeshavetherebeentimeswh"Paranoia"
encode ParanoiaProbeOverthepastye, gen (Paranoia)
encode Ifyeshavetherebeentimeswh, gen (Paranoia2)

**Strange experiences
tab StrangeexperiencesOverthepa
tab Ifyesdidyoufeelitwassos, m //76% missing
lab var StrangeexperiencesOverthepa "have there been times when you felt that something strange was going on"
lab var Ifyesdidyoufeelitwassos "Strange experiences"
encode StrangeexperiencesOverthepa, gen (Strangeexperiences)
encode Ifyesdidyoufeelitwassos, gen (Strangeexperiences2)

**Hallucinations
tab HallucinationsOverthepastye
tab BL, m // 78% missing
lab var HallucinationsOverthepastye "heard or saw things that other people couldn't"
lab var BL "Hallucinations"
encode HallucinationsOverthepastye, gen (Hallucinations)
encode BL, gen (Hallucinations2)

foreach var of varlist Hypomania-Hallucinations2 {
  tab `var', m
   }

/*rename hypomania_supplement hypomania
rename hypomania_supplement_001 hypomania2
rename thoughtinsertion_supplement insertion
rename thoughtinsertion_supplement_001 insertion2
rename paranoia_supplement paranoia
rename paranoia_supplement_001 paranoia2
rename strangeexperience_supplement strange 
rename strangeexperience_supplement_001 strange2
rename hallucinations_supplement hallucination
rename hallucinations_supplement_001 hallucination2

foreach var of varlist hypomania-hallucination2{
	encode `var', gen (`var'_1)
}*/


**For an item to be considered positive, both the root and the additional corroborating questions have to be endorsed either in the last 12 months or lifetime.
gen Hypomaniasymp =. 
replace Hypomaniasymp =1 if Hypomania==2 & Hypomania2 ==2
replace Hypomaniasymp = 2 if Hypomaniasymp !=1
lab var Hypomaniasymp "Symptoms of hypomania"
tab Hypomaniasymp

gen Insertion =. 
replace Insertion =1 if Thoughtinsertion==2 & Thoughtinsertion2 ==2
replace Insertion = 2 if Thoughtinsertion ==1 | Thoughtinsertion2 == 1
replace Insertion =. if Thoughtinsertion == . 
lab var Insertion "Thought insertion"
tab Insertion

gen Paranoiasymp =. 
replace Paranoiasymp =1 if Paranoia==2 & Paranoia2 ==2
replace Paranoiasymp = 2 if Paranoia ==1 | Paranoia2 ==1
replace Paranoiasymp = . if Paranoia == . 
lab var Paranoiasymp "Symptoms of paranoia"
tab Paranoiasymp

gen Strange =. 
replace Strange = 1 if Strangeexperiences==2 & Strangeexperiences2==2
replace Strange = 2 if Strangeexperiences==1 | Strangeexperiences2==1
replace Strange = . if  Strangeexperiences ==.
lab var Strange "Strange experiences"
tab Strange

gen Hallucinationsymp =. 
replace Hallucinationsymp =1 if Hallucinations==2 & Hallucinations2 ==2
replace Hallucinationsymp = 2 if Hallucinations == 1 | Hallucinations2 == 1
replace Hallucinationsymp = . if Hallucinations ==.
lab var Hallucinationsymp "Symptoms of Hallucination"
tab Hallucinationsymp


***In addition to these five binary measures, a composite screening measure was created using responses across all five psychotic symptoms (0 = negative on all; 1 = positive on any) and was further categorized into past-year 

gen Psychosis =.
replace Psychosis =1 if Hypomaniasymp ==1 | Insertion==1 | Paranoiasymp ==1 | Strange==1 | Hallucinationsymp==1
replace Psychosis =. if Hypomaniasymp ==. & Insertion==. & Paranoiasymp ==. & Strange==. & Hallucinationsymp==.
replace Psychosis =2 if Psychosis !=1
lab var Psychosis "Diagnosis of psychosis"
tab Psychosis



keep age Agegrps  sex marstat Education HHead Activitylevel SocialMedia Nervous Worrying Things Trouble Restless Irritable Afraid GADcategory GAD Interest Hopeless Sleeping  Tired Appetite Family Concentrating Moving Thoughts PHQcat PHQ Hypomania Hypomania2 Thoughtinsertion Thoughtinsertion2 Paranoia Paranoia2 Strangeexperiences Strangeexperiences2 Hallucinations Hallucinations2 Hypomaniasymp Insertion Paranoiasymp Strange Hallucinationsymp Psychosis

save community2.dta, replace



clear all
*log using MHdataset2.log, replace

****Importing dataset
 import excel "D:\Mental Health\Excel Datasets\Uchunguzi_wa_afya_ya_akili_-_all_versions_-_labels_-_2024-09-16-10-23-32.xlsx", sheet("Uchunguzi wa afya ya akili") firstrow

mdesc

drop OtherSpecify -Thoughtsthatyouwouldbebette
drop SECTION1DEMOGRAPHICINFORMATI
drop SECTION2SOCIOECONOMICFACTORS
drop SECTION4GENERALMENTALHEALTH
drop AE
drop SECTION3ACCESSIBILITYTOMENT
drop SECTION6PHQ9
drop SECTION5GAD7
drop SECTION7PSYCHOSIS
drop Nyinginezotafadhalifafanua
mdesc

****Variables with no observations dropped
*drop  OtherSpecify-Thoughtsthatyouwouldbebette //100% missing observations


**Age
replace Umri = "45" in 67

replace Umri = "29" in 131

replace Umri = "75" in 567

destring Umri, gen (age)
sum age, det

lab var age "Age in years"

recode age (min/24 =1 "<25 years") (25/34=2 "25-34 years") (35/44=3 "35-44 years") (45/54=4 "45-54 years") (55/64 =5 "55-64 years") (65/max=6 "> 65 years"), gen (Agegrps) 
lab var Agegrps "AGE GROUPS"
tab Agegrps

**Gender
tab Jinsia
tab Jinsia, nol
encode Jinsia, gen (gender)
tab gender, nol
recode gender (1=1 "Female") (2=2 "Male") (3=.), gen (sex)
lab var sex "Sex the participant"
tab sex

**Marital status
tab HaliyaNdoa
encode HaliyaNdoa, gen (marital_status)
tab marital_status , nol
tab marital_status
recode marital_status (1 3 4 =1 "single/widowed/separated") (2=2 "Married"), gen (marstat)
lab var marstat "Marital status"
tab marstat

**Education level
tab KiwangochaElimu
encode KiwangochaElimu, gen (education)
tab education, nol
recode education (2=1 "No formal education") (5=2 "Primary level") (6=3 "Secondary level") (1 3 4 = 4 "Tertiary level"), gen (Education)
lab var Education "Education level"
tab Education

**Head of the household
tab KichwachaKaya
encode KichwachaKaya, gen (household_head)
tab household_head, nol
recode household_head (2=1 "Yes") (1=2 "No"), gen (HHead)
lab var HHead "Head of the household"
tab HHead 



***Village
tab Kijiji
encode Kijiji, gen (Vill)
recode Vill (5=1 "Prison Viwandani") (4=2 "Mwezangombe") (1=3 "Green Estate") (2=4 "Kasarani") (3=5 "Mtaani"), gen (Village)
lab var Village "Village of residence"
tab Village


***Income
tab ChanzochaMapato
desc ChanzochaMapato
destring ChanzochaMapato, ignore ("Business, Familia, Kshs, N/A, ,") gen (Income)
sum Income, det
lab var Income "Income level"

**SOCIOECONOMIC FACTORS

**Activity level
tab Jeunashirikimarangapikatika
encode Jeunashirikimarangapikatika, gen (activity)
tab activity, nol
recode activity (1 =1 "Never") (4=2 "Rarely") (2=3 "Occasionally") (3=4 "Frequently"), gen (Activitylevel)
lab var Activitylevel "Activity level"
tab Activitylevel, m


**Social Media
tab Unatumiamudaganikwenyemitand
encode Unatumiamudaganikwenyemitand, gen(SM)
tab SM, nol
recode SM (1=1 "< 1 hour") (2=2 "1-3 hours") (3=3 "3-5 hours") (4=4 "> 5 hours"), gen (SocialMedia)
lab var SocialMedia "Social Media Use"
tab SocialMedia, m



**** cariable with > 96% missing observations
/*
tab Chanzochakokikuuchaufadhili, m
tab Jeumewahikukumbwanamatatizo, m
tab Ikiwandionimarangapimatati, m
tab Jeunafahamuhudumazaafyaya, m
tab Katikamiezi12iliyopitaumewa, m
tab Kwakipimocha15ungepimajeu, m
tab Kwamaoniyakomaboreshoganiy, m
tab AE, m
tab Ikiwandioulitafutamsaadawa, m
tab Nimsaadaganiuliofikiwakatika, m
*/

/**Accessibility to mental health mitigation measures (over the last 12 months)
tab aware, m // 98.4% missing
tab mhservices_supplement, m // 98.2% missing
tab mhservices_supplement_001, m //99.2% missing
tab mhopinion, m // 99% missing


**General Mental Health (Over the last 12 months)
**symptoms_supplement
tab symptoms_supplement, m // 98% missing
tab symptoms_supplement // 98% missing
tab accessible_service, m // 99.4% missing*/


**Generalized Anxiety Disorder 7-item (GAD-7)
***The Generalized Anxiety Disorder 7-item (GAD-7) is a easy to perform initial screening tool for generalized anxiety disorder

foreach var of varlist Kwakipindichawikimbiliziliz- Mwenyewoganikanakwambajambo{
tab `var', m
}

**GAD variables
rename Kuwanawasiwasimwenyehofu nervous
rename Kutowezakuzuiahofuaukudhibit control_worrying
rename Mwenyewasiwasikupitakiasiku worry_too_much
rename Mwenyetatizolakutulianakupu  relaxing
rename Uliyekosautulivusanakiasicha restless
rename Mwenyehasiraamakukerwakwah irritable
rename Mwenyewoganikanakwambajambo afraid


foreach var of varlist nervous - afraid {
	encode `var', gen (`var'_1)
}

recode nervous_1 (1=0 "Not at all") (4=1 "several days") (6=2 "more than half the days") (5=3 "Nearly everyday") (3 2=.), gen (nervous_1_2)

recode control_worrying_1 (1=0 "Not at all") (3=1 "several days") (5=2 "more than half the days") (4=3 "Nearly everyday") (2=.), gen (control_worrying_1_2)

recode worry_too_much_1 (1=0 "Not at all") (4=1 "several days") (6=2 "more than half the days") (5=3 "Nearly everyday") (3 2=.), gen (worry_too_much_1_2)

recode relaxing_1 (1=0 "Not at all") (3=1 "several days") (5=2 "more than half the days") (4=3 "Nearly everyday") (2=.), gen (relaxing_1_2)

recode restless_1 (1=0 "Not at all") (4=1 "several days") (6=2 "more than half the days") (5=3 "Nearly everyday") (3 2=.), gen (restless_1_2)

recode irritable_1 (1=0 "Not at all") (4=1 "several days") (6=2 "more than half the days") (5=3 "Nearly everyday") (3 2=.), gen (irritable_1_2)

recode afraid_1 (1=0 "Not at all") (4=1 "several days") (6=2 "more than half the days") (5=3 "Nearly everyday") (3 2=.), gen (afraid_1_2)

foreach var of varlist nervous_1_2- afraid_1_2{
	tab `var', m
	}


lab var nervous_1_2 "Feeling nervous, anxious, or on edge"
lab var control_worrying_1_2 "Not being able to stop or control worrying"
lab var worry_too_much_1_2 "Worrying too much about different things"
lab var relaxing_1_2 "Trouble relaxing"
lab var restless_1_2 "Being so restless that it's hard to sit still"
lab var irritable_1_2 "Becoming easily annoyed or Irritable"
lab var afraid_1_2 "Feeling afraid as if something awful might happen"

rename nervous_1_2 Nervous
rename control_worrying_1_2 Worrying
rename worry_too_much_1_2 Things
rename relaxing_1_2 Trouble
rename restless_1_2 Restless
rename irritable_1_2 Irritable
rename afraid_1_2 Afraid

foreach var of varlist Nervous-Afraid {
  tab `var', m
   }
   
count if Nervous==. | Worrying==. | Things==. | Trouble==.| Restless==.| Irritable==.| Afraid ==.
**59 Participants are missing atleast one of the components required to calculate GAD score

egen GAD = rowtotal (Nervous-Afraid), m 

replace GAD =. if Nervous==. | Worrying==. | Things==. | Trouble==.| Restless==.| Irritable==.| Afraid ==.

***GAD7 SCORES
sum GAD, detail

**Generating GAD categories 
***Computing GAD-7 scores: Score 0-4: Minimal Anxiety. Score 5-9: Mild Anxiety. Score 10-14: Moderate Anxiety. Score greater than 15: Severe Anxiety.
recode GAD (min/4 =1 "Minimal") (5/9=2 "Mild") (10/14= 3 "Moderate ") (15/max=4 "Severe") (.=.), gen (GADcategory)
lab var GADcategory "GAD Category"
tab GADcategory, m




**PHQ9 QUESTIONNAIRE
***The PHQ-9 is a multipurpose instrument for screening, diagnosing, monitoring and measuring the severity of depression.

foreach var of varlist Usiyekuwanakukosahamuauraha - Kujihisikwambaingekuwaboraka{
	tab `var', m
}

rename Usiyekuwanakukosahamuauraha interest
rename Kujihisimnyongemwenyehuzuni hopeless
rename Unamatatizoyakupatausingizi sleeping
rename Kujihisimchovuamamlegevu tired
rename Mwenyekukosahamuyachakulaau appetite
rename Kujihisivibayaaukuhisikamam family
rename Kuwamatatizoyakuwamakinikam concentrating
rename Kutembeaaukuongeapolepolesa moving
rename Kujihisikwambaingekuwaboraka thoughts

foreach var of varlist interest - thoughts {
	encode `var', gen (`var'_1)
}

foreach var of varlist interest_1 - moving_1 {
	recode `var' (1=0 "not at all") (5=1 "several days") (6=2 "more than half the days") (2=3 "Nearly everyday") (3 4=.), gen (`var'_2)
}

recode thoughts_1 (1=0 "not at all") (4=1 "several days") (5=2 "more than half the days") (2=3 "Nearly everyday") (3=.), gen (thoughts_1_2)

lab var interest_1_2 "Little interest or pleasure in doing things"
lab var hopeless_1_2 "Feeling down, depressed, or hopeless"
lab var sleeping_1_2 "Trouble falling or staying asleep, or sleeping too much"
lab var tired_1_2 "Feeling tired or having little energy"
lab var appetite_1_2 "Poor appetite or overeating"
lab var family_1_2 "Feeling bad about yourself — or that you are a failure or have let yourself or your family down"
lab var concentrating_1_2 "Trouble concentrating on things, such as reading the newspaper or watching television"
lab var moving_1_2 "Moving or speaking so slowly that other people could have noticed? Or the opposite — being so fidgety or restless that you have been moving around a lot more than usual"
lab var thoughts_1_2 "Thoughts that you would be better off dead or of hurting yourself in some way"

rename interest_1_2 Interest
rename hopeless_1_2 Hopeless
rename sleeping_1_2 Sleeping
rename tired_1_2 Tired
rename appetite_1_2 Appetite
rename family_1_2 Family
rename concentrating_1_2 Concentrating
rename moving_1_2 Moving
rename thoughts_1_2 Thoughts

foreach var of varlist Interest-Thoughts {
	tab `var', m
}

****PHQ-9 ANALYSIS 
**PHQ-9 SCORES
egen PHQ = rowtotal(Interest-Thoughts), m
sum PHQ, det

count if Interest==. | Hopeless==. | Sleeping==. | Tired==.| Appetite==.| Family ==. |Concentrating==.| Moving==.| Thoughts==.

**72 Participants missing at least one variable required to calculate a PHQ score
replace PHQ=. if Interest==. | Hopeless==. | Sleeping==. | Tired==.| Appetite==.| Family ==. |Concentrating==.| Moving==.| Thoughts==.

***PHQ-9 categories: 0-4 None/minimal; 5-9 Mild; 10-14 Moderate; 15-19 Moderately severe; 20-27 Severe
recode PHQ (min/4=1 "None/minimal") (5/9=2 "Mild") (10/14=3 "Moderate") (15/19=4 "Moderate severe") (20/27=4 "Severe"), gen (PHQcat)
lab var PHQcat "PHQ Category"
tab PHQcat, m




***Psychosis Screening Questionnaire
**Hypomania Probe
encode Hypomaniaprobe1Katikamuda, gen (hypomania)
encode Jekulikuwanasababudhahiriy, gen (hypomania2)
recode hypomania (1=1 "No") (2=2 "Yes"), gen (Hypomania)
recode hypomania2 (1=1 "No") (2=2 "Yes"), gen (Hypomania2)
lab var Hypomania "you felt very happy indeed without a break for days on end"
tab Hypomania2, m // 93% missing
lab var Hypomania2 "Hypomania"


**Thought insertion
encode Thoughtinsertion2Katikamud, gen (thoughtinsertion)
encode Jehilililitokeakwanjiaamba, gen (thoughtinsertion2)
recode thoughtinsertion (1=1 "No") (2=2 "Yes"), gen (Thoughtinsertion)
recode thoughtinsertion2 (1=1 "No") (2=2 "Yes"), gen (Thoughtinsertion2)
lab var Thoughtinsertion"felt that your thoughts were directly interfered with or controlled by some outside force or person"
tab Thoughtinsertion2, m //90% missing
lab var Thoughtinsertion2 "Though insertion"


**Paranoia Probe
encode ParanoiaProbe3Katikamudaw, gen (paranoia)
encode Jekunanyakatiambapoulihisi, gen (paranoia2)
recode paranoia (1=1 "No") (2=2 "Yes"), gen (Paranoia)
recode paranoia2 (1=1 "No") (2=2 "Yes"), gen (Paranoia2)
lab var Paranoia "u felt that people were against youu felt that people were against you"
tab Paranoia2, m //80% missing
lab var Paranoia2 "Paranoia"


**Strange experiences
encode StrangeexperiencesKatikamuda, gen (strangeexperiences)
encode Jeulihisikuwanijambolaaja, gen (strangeexperiences2)
recode strangeexperiences (1=1 "No") (2=2 "Yes"), gen (Strangeexperiences)
recode strangeexperiences2 (1=1 "No") (2=2 "Yes"), gen (Strangeexperiences2)
lab var strangeexperiences "have there been times when you felt that something strange was going on"
tab strangeexperiences2, m //91% missing
lab var strangeexperiences2 "Strange experiences"


**Hallucinations
encode Hallucinations5Katikamudaw , gen (hallucinations)
encode Jewakatiwowoteulisikiasauti, gen (hallucinations2)
recode hallucinations (1=1 "No") (2=2 "Yes"), gen (Hallucinations)
recode hallucinations2 (1=1 "No") (2=2 "Yes"), gen (Hallucinations2)
lab var Hallucinations "heard or saw things that other people couldn't"
tab Hallucinations2, m // 93% missing
lab var Hallucinations2 "Hallucinations"


/*rename hypomania_supplement hypomania
rename hypomania_supplement_001 hypomania2
rename thoughtinsertion_supplement insertion
rename thoughtinsertion_supplement_001 insertion2
rename paranoia_supplement paranoia
rename paranoia_supplement_001 paranoia2
rename strangeexperience_supplement strange 
rename strangeexperience_supplement_001 strange2
rename hallucinations_supplement hallucination
rename hallucinations_supplement_001 hallucination2

foreach var of varlist hypomania-hallucination2{
	encode `var', gen (`var'_1)
}

*/
**For an item to be considered positive, both the root and the additional corroborating questions have to be endorsed either in the last 12 months or lifetime.

gen Hypomaniasymp =. 
replace Hypomaniasymp =1 if Hypomania==2 & Hypomania2 ==2
replace Hypomaniasymp = 2 if Hypomaniasymp !=1
lab var Hypomaniasymp "Symptoms of hypomania"
tab Hypomaniasymp

gen Insertion =. 
replace Insertion =1 if Thoughtinsertion==2 & Thoughtinsertion2 ==2
replace Insertion = 2 if Thoughtinsertion ==1 | Thoughtinsertion2 == 1
replace Insertion =. if Thoughtinsertion == . 
lab var Insertion "Thought insertion"
tab Insertion

gen Paranoiasymp =. 
replace Paranoiasymp =1 if Paranoia==2 & Paranoia2 ==2
replace Paranoiasymp = 2 if Paranoia ==1 | Paranoia2 ==1
replace Paranoiasymp = . if Paranoia == . 
lab var Paranoiasymp "Symptoms of paranoia"
tab Paranoiasymp

gen Strange =. 
replace Strange = 1 if Strangeexperiences==2 & Strangeexperiences2==2
replace Strange = 2 if Strangeexperiences==1 | Strangeexperiences2==1
replace Strange = . if  Strangeexperiences ==.
lab var Strange "Strange experiences"
tab Strange

gen Hallucinationsymp =. 
replace Hallucinationsymp =1 if Hallucinations==2 & Hallucinations2 ==2
replace Hallucinationsymp = 2 if Hallucinations == 1 | Hallucinations2 == 1
replace Hallucinationsymp = . if Hallucinations ==.
lab var Hallucinationsymp "Symptoms of Hallucination"
tab Hallucinationsymp



***In addition to these five binary measures, a composite screening measure was created using responses across all five psychotic symptoms (0 = negative on all; 1 = positive on any) and was further categorized into past-year 

gen Psychosis =.
replace Psychosis =1 if Hypomaniasymp ==1 | Insertion==1 | Paranoiasymp ==1 | Strange==1 | Hallucinationsymp==1
replace Psychosis =. if Hypomaniasymp ==. & Insertion==. & Paranoiasymp ==. & Strange==. & Hallucinationsymp==.
replace Psychosis =2 if Psychosis !=1
lab var Psychosis "Diagnosis of psychosis"
tab Psychosis

lab define symp 1 "Yes" 2 "No"
lab values Psychosis symp
tab Psychosis

**Year of Study
tab MwakawaMasomo, m
encode MwakawaMasomo , gen (YearofStudy)

save swahili.dta, replace



clear all 
use swahili.dta

keep if YearofStudy ==. // subsetting community data


keep age Agegrps  sex marstat Education HHead Activitylevel SocialMedia Nervous Worrying Things Trouble Restless Irritable Afraid GADcategory GAD Interest Hopeless Sleeping  Tired Appetite Family Concentrating Moving Thoughts PHQcat PHQ Hypomania Hypomania2 Thoughtinsertion Thoughtinsertion2 Paranoia Paranoia2 Strangeexperiences Strangeexperiences2 Hallucinations Hallucinations2 Hypomaniasymp Insertion Paranoiasymp Strange Hallucinationsymp Psychosis

save community.dta, replace

****
log close

clear all

log using communitydata.log, replace
use community2.dta
append using community.dta
mdesc


**Generating descriptive statistics




sum age, det
foreach var of varlist Agegrps-SocialMedia {
	tab `var', m
}

foreach var of varlist Nervous-GADcategory {
	tab `var', m
}

foreach var of varlist Interest -PHQcat {
	tab `var', m
}

foreach var of varlist Hypomania-Psychosis {
	tab `var', m
}


log close
