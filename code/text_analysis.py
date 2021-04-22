import itertools
import multiprocessing as mp
import numpy as np
import nltk
import pandas as pd
import re
import sklearn
import string

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression 
from sklearn.model_selection import train_test_split
from sklearn.metrics import f1_score

from nltk import pos_tag
from nltk.tokenize import TweetTokenizer
from nltk.corpus import wordnet
from nltk.stem import WordNetLemmatizer 

from spellchecker import SpellChecker

lemmatizer = WordNetLemmatizer()
tokenizer =  TweetTokenizer()
spell = SpellChecker()

# Load in words not recognized by the spell checker
spell.word_frequency.load_words(
	[
		'vigilantism',
		'traumatizing',
		'commenters',
		'habius',
		'nyc',
		'uncoerced',
		'envision',
		'slayings',
		'facebook',
		'19th',
		'jw',
		'yhvh',
		'veganism',
		'saddening',
		'aclu',
		'meting',
		'evilness',
		'misfiring',
		'demonizing',
		'fawned',
		'standers'
	]
)

def split_tokenizer(text):
    return text.split()

def normalize_contractions(tokens):
	token_map = {
		# Contractions
		"ain't" : 'is not',
		"aint" : 'is not',
		"aren't" : 'are not',
		"can't" : 'can not',
		"cannot" : 'can not',
		"could've" : 'could have',
		"couldn't" : 'could not',
		"couldnt" : 'could not',
		"didn't" : 'did not',
		"didnt" : 'did not',
		"doesn't" : 'does not',
		"doesnt" : 'does not',
		"don't" : 'do not',
		"gonna'" : 'going to',
		"gotta'" : 'got to',
		"hadn't" : 'had not',
		"hasn't" : 'has not',
		"haven't" : 'have not',
		"he'll" : 'he will',
		"he's" : 'he is',
		"he've" : 'he have',
		"how'd" : 'how did',
		"how'll" : 'how will',
		"how're" : 'how are',
		"how's" : 'how is',
		"i'd" : 'i would',
		"i'll" : 'i will',
		"i'm" : 'i am',
		"i'mm" : 'i am',
		"i've" : 'i have',
		"iäm" : 'i am',
		"isn't" : 'is not',
		"isnt" : 'is not',
		"it'd" : 'it would',
		"it'll" : 'it shall',
		"it's" : 'it is',
		"let's" : 'let us',
		"she'll" : 'she will',
		"she's" : 'she is',
		"should've" : 'should have',
		"shouldn't" : 'should not',
		"shouldnt" : 'should not',
		"that'll" : 'that will',
		"that's" : 'that is',
		"there's" : 'there is',
		"they'll" : 'they will',
		"they're" : 'they are',
		"theyre" : 'they are',
		"theyve" : 'they have',
		"wasn't" : 'was not',
		"we'll" : 'we will',
		"we'r" : 'we are',
		"we're" : 'we are',
		"we've" : 'we have',
		"weren't" : 'were not',
		"what's" : 'what is',
		"who'll" : 'who will',
		"who're" : 'who are',
		"who's" : 'who is',
		"why're" : 'why are',
		"won't" : 'will not',
		"would've" : 'would have',
		"wouldn't" : 'would not',
		"wouldnt" : 'would not',
		"you'll" : 'you will',
		"you're" : 'you are',
		"you've" : 'you have',
		# Misspellings
		"1st" : 'first',
		"afterall" : 'after all',
		"aggregous" : 'egregious',
		"agreeance" : 'agreement',
		"appaled" : 'appalled',
		"begits" : 'begets',
		"by-standers" : 'bystanders',
		"deservedbut" : 'deserved but',
		"detainment" : 'detention',
		"discusting" : 'disgusting',
		"disusing" : 'disgusting',
		"dserved" : 'deserved',
		"esspsaly" : 'especially',
		"groose" : 'gross',
		"hanious" : 'heinous',
		"hasnot" : 'has not',
		"hellholes" : 'hell holes',
		"idk" : 'i do not know',
		"ifelt" : 'i felt',
		"killlllled" : 'killed',
		"lawman" : 'law man',
		"lif" : 'life',
		"meatheads" : 'meat heads',
		"mofo" : 'mother fucker',
		"moreso" : 'more so',
		"mulsuman" : 'muslim',
		"neice" : 'niece',
		"ovpe" : 'hope',
		"perp" : 'perpetrator',
		"pinon" : 'opinion',
		"preparator" : 'perpetrator',
		"ralley" : 'rally',
		"slayings" : 'slaying',
		"trumpers" : 'Trump',
		"unbeleif" : 'disbelief',
		"unmerciful" : 'not merciful',
		"vigilanteism" : 'vigilantism',
		"vigilanty" : 'vigilante',
		"vigiliantism" : 'vigilantism',
		"viglantism" : 'vigilantism',
		"yound" : 'young',
	}
	norm_tokens = []
	for t in tokens:
		if t in token_map.keys():
			for item in token_map[t].split():
				norm_tokens.append(item)
		else:
			for item in re.split('\W+', t.replace("'s","")):
				if item != '':
					norm_tokens.append(item)
	return(norm_tokens)

def get_wordnet_pos(treebank_tag):
	if treebank_tag.startswith('J'):
		return wordnet.ADJ
	elif treebank_tag.startswith('V'):
		return wordnet.VERB
	elif treebank_tag.startswith('N'):
		return wordnet.NOUN
	elif treebank_tag.startswith('R'):
		return wordnet.ADV
	else:
		return None

def lemma_tokenizer(text):
	text = text.replace('’',"'")
	tokens = normalize_contractions(
		[t.lower() for t in tokenizer.tokenize(text)]
	)
	tokens = [t for t in tokens if t not in stop]
	unk = spell.unknown(tokens)
	if len(unk) > 0:
		new_tokens =[]
		for w in tokens:
			if w in unk:
				corr = spell.correction(w)
				new_tokens.append(corr)
			else:
				new_tokens.append(w)
		tokens = new_tokens
	lemmas = []
	for w, p in pos_tag(tokens):
		p = get_wordnet_pos(p)
		if p is None:
			lemmas.append(lemmatizer.lemmatize(w))
		else:
			lemmas.append(lemmatizer.lemmatize(w,p))
	return ' '.join([l for l in lemmas if l not in lemma_stop])

def fit_boot_model(sample):
  X_train = vect.transform(sample.text)
  X_test = vect.transform(test.text)
  y_train = sample.treatment
  y_test = test.treatment
  lm = LogisticRegression(penalty = 'l2')
  mod = lm.fit(X_train, y_train)
  y_pred = mod.predict(X_test)
  f1 = f1_score(y_test, y_pred)
  vocab = vect.get_feature_names()
  betas = mod.coef_.tolist()[0]
  output = list(zip(vocab, list(betas)))
  output.append(('f1', f1))
  return(output)

punc = list(string.punctuation)
stop = ['i', 'me', 'my', 'myself', 'you', 'your', 'yours', 'yourself', 'yourselves', 'she', 'her', 'hers', 'herself', 'it', 'its', 'itself', 'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which', 'who', 'whom', 'this', 'that', 'these', 'those', 'am', 'is', 'are', 'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had', 'having', 'do', 'does', 'did', 'doing', 'a', 'an', 'the', 'and', 'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of', 'at', 'by', 'for', 'with', 'about', 'against', 'between', 'into', 'through', 'during', 'before', 'after', 'above', 'below', 'to', 'from', 'up', 'down', 'in', 'out', 'on', 'off', 'over', 'under', 'again', 'further', 'then', 'once', 'here', 'there', 'when', 'where', 'why', 'how', 'all', 'any', 'both', 'each', 'few', 'more', 'most', 'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own', 'same', 'so', 'than', 'too', 'very', 'can', 'will', 'just', 'should', 'now']#nltk.corpus.stopwords.words('english')
stop = list(set(stop + punc))
# Remove uninformative words
lemma_stop = ['article', 'describe', 'attack', 'perpetrator', 'come', 'allege', 'reaction', 'homicide', 'child', 'think', 'reaction', 'person', 'people', 'child', 'man', 'he', 'him', 'his', 'read']

data = pd.read_csv('../data/survey_data.csv')
data['text'] = data.react_mob + " * " + data.react_hom
data = data.loc[data.text.notnull(), ]
data['text'] = data['text'].apply(lemma_tokenizer)
data.loc[data.exclude.isnull(), ['text', 'factor_sensational', 'factor_outgroup', 'factor_peer_viol', 'survey_code']].to_csv('../data/text_features.csv')

for treatment in ["factor_sensational", "factor_outgroup", "factor_peer_viol"]:
	## Regularized Logistic Regression
	data['treatment'] = data[treatment]
	full_data = data.loc[data.treatment.notnull(),]
	train, test, train_y, test_y = train_test_split(
		full_data,
		full_data.treatment,
		test_size=0.1,
		random_state=1988
	)
	## Bootstrap Samples
	T = len(train)
	samples = []
	for i in range(1000):
	  x = train.loc[train.treatment == 0,].sample(T//2, replace=True)
	  x = x.append(train.loc[train.treatment == 1,].sample(T//2, replace=True))
	  samples.append(x)
	## Classifier
	vect = TfidfVectorizer(ngram_range=(1,5), tokenizer=split_tokenizer, min_df=5/T)
	vect = vect.fit(full_data.text)
	# Train the logistic regression models on each bootstrap sample in parallel
	with mp.Pool(mp.cpu_count()) as p:
		outputs = p.map(fit_boot_model, samples)
	# Save samples to a dataframe
	coef_dat = pd.DataFrame(list(itertools.chain.from_iterable(outputs)))
	coef_dat.to_csv('../data/%s_text_coefs.csv' % treatment, index=False)
