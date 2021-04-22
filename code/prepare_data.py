import pandas as pd
import numpy as np

def read_qualtrics(fn, prefix):
	df = pd.read_csv(fn)
	# Extract qualtrics question text
	question_text = df.iloc[:1].values[0]
	# Data begins from third row
	df = df.iloc[2:]
	# Drop bots and duplicates from pre-treatment df
	if prefix == 'PRE':
		df = df.loc[df.Bots.isnull(),]
	# Distinguish between pre- and post-treatment
	colnames = [prefix + c.replace('QID', '') if c != 'Q1' else 'Q1' for c in df.columns]
	df.columns = colnames
	# Save a mapping of the questions and the column name
	question_df = pd.DataFrame(list(zip(colnames, question_text)), columns=['colname', 'question_text'])
	return(df, question_df)

## Read in Raw data

# Read in raw Qualtrics data
post_treatment, post_treatment_question_df = read_qualtrics('../data/post_treatment.csv', 'POST')
post_treatment_text, _ = read_qualtrics('../data/post_treatment_text.csv', 'POST')
pre_treatment, pre_treatment_question_df = read_qualtrics('../data/pre_treatment.csv', 'PRE')
pre_treatment_text, _ = read_qualtrics('../data/pre_treatment_text.csv', 'PRE')

# Read in data from external treatment site
survey_web_data = pd.read_csv('../data/survey_web_data.csv')
survey_web_comment_data = pd.read_csv('../data/survey_web_comment_data.csv')
fillna = ['video_link','comment_1_report','comment_2_report','comment_3_report','comment_1_like','comment_2_like','comment_3_like']

# Process Django's null boolean fields; blanks are zeroes
survey_web_data[fillna] = survey_web_data[fillna].astype('bool').astype('float64')
treatments = ['factor_sensational','factor_refugee','factor_peer']
for t in treatments:
	survey_web_data[t] = survey_web_data[t].astype('float64')

## Correct user-inputted errors by forcing caps, removing whitespace

def fix_input_error(col):
	col = col.upper()
	return col.strip()

post_treatment['Q1'] = post_treatment['Q1'].apply(fix_input_error)
post_treatment_text['Q1'] = post_treatment_text['Q1'].apply(fix_input_error)
pre_treatment['Q1'] = pre_treatment['Q1'].apply(fix_input_error)
pre_treatment_text['Q1'] = pre_treatment_text['Q1'].apply(fix_input_error)

## Merge pre- post- and external site data

# Rename columns as specified in a question name mapping CSV
q_lookup = pd.read_csv('../data/survey_question_lookup.csv')
# Identify columns that are non-numeric
non_num = q_lookup.loc[q_lookup['non-numeric'].notnull(),'colname']
# Replace non-numeric rows with text responses
# This works because both dfs are in the same order
pre_treatment[non_num] = pre_treatment_text[non_num]
# Merge pre- and post-treatment dataframes
pre_post = pre_treatment.merge(post_treatment, on='Q1')
# Merge qualtrics data with data from external site
data = pre_post.merge(survey_web_data, left_on='Q1', right_on='survey_code')

# Remove columns that are unused in the analysis; rename them
columns_to_keep = q_lookup.loc[q_lookup.delete.isnull(),'colname']
new_colnames = q_lookup.loc[q_lookup.delete.isnull(),'rename']
data = data[columns_to_keep]
data.columns = new_colnames

# Add user comments from survey website
for i, row in survey_web_comment_data.iterrows():
	comments = survey_web_comment_data.loc[survey_web_comment_data.survey_code == row['survey_code'], 'text']
	if len(comments) > 0:
		comment_text = [c for c in comments if type(c) == str]
		comment_text = " | ".join(list(comment_text))
		data.loc[data.survey_code == row.survey_code, "comment_text"] = comment_text

## Drop bots, straightliners, users completing survey in < 7 minutes, foreign users, pilot respondents

# Save complete survey data with exclusion reasons
data.to_csv('../data/complete_survey_data.csv', index=False)

# Remove pilot respondents (observations 1-29)
data = data.iloc[29:]

# Exclude bad data for analysis
data = data.loc[data.exclude.isin(["Keep", "Attention check"]) | data.exclude.isnull(),]

## Add annotation from user comments and open-ended responses

comments = pd.read_csv('../data/comments.csv')
open_ended = pd.read_csv('../data/open_ended.csv')

comments = comments.drop(['comment_text'], axis=1)
open_ended = open_ended.drop(['react_mob', 'react_hom'], axis=1)

annotation = pd.merge(open_ended, comments.set_index('survey_code_com'), left_on='survey_code_oe', right_index=True, how='left')
annotation = annotation.rename(index=str, columns={"survey_code_oe": "survey_code"})

data = data.merge(annotation, how='left', on='survey_code')

## Calculate variables and scales for analysis

# Create additive authoritarian personality scale; log 

data['auth'] = data[['auth_inde_resp', 'auth_obed_self', 'auth_curi_mann', 'auth_cons_beha']].astype('float64').sum(axis=1)
data['auth_log'] = np.log(data['auth'] + 1)

# Create variable for Republican Party ID

data['pi_R'] = 0
data.loc[data['party_id'] == "Republican", 'pi_R'] = 1

# Create ethnocentrism scale

data['ethno_we_og'] = data[['ethno_we_black','ethno_we_asian','ethno_we_hisp','ethno_we_refugee','ethno_we_muslim']].astype('float64').mean(axis=1)
data['ethno_int_og'] = data[['ethno_int_black','ethno_int_asian','ethno_int_hisp','ethno_int_refugee','ethno_int_muslim']].astype('float64').mean(axis=1)
data['ethno_tru_og'] = data[['ethno_tru_black','ethno_tru_asian','ethno_tru_hisp','ethno_tru_refugee','ethno_tru_muslim']].astype('float64').mean(axis=1)
data['ethno_og'] = data[['ethno_we_og','ethno_int_og','ethno_tru_og']].astype('float64').mean(axis=1)
data['ethno_og'] = np.abs(100 - data['ethno_og'])

# Create anti-muslim sentiment scale

data['muslim_sentiment'] = data[['ethno_we_muslim','ethno_int_muslim','ethno_tru_muslim']].astype('float64').mean(axis=1)
data['muslim_sentiment'] = np.abs(100 - data['muslim_sentiment'])

# Create variables for ethnicities

eths = ['Native American', 'Asian/Pacific Islander', 'African/Afro-Caribbean', 'Hispanic/Latino', 'Other', 'Mixed', 'White']
eth_nm = ['native', 'asian', 'african', 'hispanic', 'eth_other', 'eth_mixed', 'white']
for i, eth in enumerate(eths):
	data[eth_nm[i]] = 0
	data.loc[data.ethnicity.str.contains(eth), eth_nm[i]] = 1

# Religious Fundamentalism

data.loc[data.relig_fund.isnull(), 'relig_fund'] = 0
data['relig_fund_log'] = np.log(data['relig_fund'].astype('float64') + 1)

# Create factor for South (US Census Bureau designation, 17 states)

south = ['Delaware', 'Florida', 'Georgia', 'Maryland', 'North Carolina', 'South Carolina', 'Virginia', 'West Virginia', 'Alabama', 'Kentucky', 'Mississippi', 'Tennessee', 'Arkansas', 'Louisiana', 'Oklahoma', 'Texas']

data['south'] = 0
data.loc[data.state.isin(south), 'south'] = 1

# Symbolic Racism scales: reverse slavery and deserve; average; log

data['symb_rac_slavery'] = np.abs(data['symb_rac_slavery'].astype('float64') - 6)
data['symb_rac_deserve'] = np.abs(data['symb_rac_deserve'].astype('float64') - 6)

data['symb_rac'] = data[['symb_rac_work', 'symb_rac_slavery', 'symb_rac_deserve', 'symb_rac_try']].mean(axis=1)
data['symb_rac_log'] = np.log(data['symb_rac'] + 1)

# Create binary variable for commenting

data['commenter'] = 0
data.loc[data.comment_text.notnull(), 'commenter'] = 1

# Create scale for lenient punishment

data['lenient_hom'] = data[['pun_nopu_hom','pun_comm_hom']].astype('float64').mean(axis=1)

# Create scale for non-lethal judicial violence

data['prison_hom'] = data[['pun_short_pris_hom', 'pun_long_pris_hom','pun_life_pris_hom']].astype('float64').mean(axis=1)

# Create ordered variable for lethal judicial violence

data['lethal_hom'] = data['pun_exe_inje_hom']

# Create additive scale for extreme lethal judicial violence

data['elethal_hom'] = data[['pun_exe_fire_hom', 'pun_exe_hang_hom', 'pun_exe_publ_hom']].astype('float64').mean(axis=1)

# Create additive scale for extrajudicial violence

data['ejv_hom'] = data[['torture','detain']].astype('float64').mean(axis=1)

## Save updated data frame

data = data.drop_duplicates('survey_code')
data.to_csv('../data/survey_data.csv', index=False)

## Create ICR sample

icr = data.loc[data.react_hom.notnull() & data.react_mob.notnull(), ]
ids = icr.index

columns = ['cog_dissonance_oe', 'fake_news_oe', 'info_seeking_oe', 'racism_hate_oe', 'social_desirability_race_oe', 'violence_oe', 'lethal_oe', 'extrajudicial_oe', 'dehumanizing_oe', 'demonizing_oe', 'group_oe', 'ingroup_oe', 'anger_oe', 'disgust_oe', 'sadness_oe', 'fear_oe', 'anxiety_oe']

sample = set()
for c in columns:
	sample.update(icr.loc[icr[c] == 1, ].sample(10, random_state=1988).survey_code)

cols = ['react_hom', 'react_mob', 'survey_code']
to_code = icr.loc[icr.survey_code.isin(sample), cols]
to_code.to_csv('../data/icr_sample.csv', index=False)
