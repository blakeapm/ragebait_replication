# Replication materials for The Dangers of False News: How Sensational Content and Outgroup Cues Strengthen Support for Violence and Anti-Muslim Policies

## Replication Instructions

To replicate the results execute the following scripts in order (on a UNIX system):

- `code/prepare_data.py`
- `code/text_analysis.py`
- `code/analysis.R`

## OS Specifications

Runtimes below are based on these specifications:

- System Version: macOS 10.14.5 (18F132)
- Kernel Version: Darwin 18.6.0
- Memory: 16GB RAM
- CPU Count: 4

## Files and Runtimes

- `prepare_data.py` takes data directly from the Qualtrics pre- and post-treatment survey, merges it with behavioral data from the article vignette, renames columns, and transforms variables.
	* Running time:
		```
		real	0m5.975s
		user	0m2.580s
		sys	0m0.190s
		```
	* Running Python 3.5.2, required packages: 
		```
		numpy==1.15.4
		pandas==0.23.4
		scikit-learn==0.19.2
		scipy==1.2.0
		six==1.10.0
		```
- `text_analysis.py` preprocesses and transforms texts (contraction normalization, lemmatization, spelling correction) and then saves the transformed text for analysis in `analysis.R.` This script also runs the bootstrap models used to identify key words in the open-ended and comment corpora.
	* Running time:
		```
		real	4m30.646s
		user	9m36.467s
		sys	0m29.745s
		```
	* Running Python 3.5.2, required packages: 
		```
		numpy==1.15.4
		pandas==0.23.4
		scikit-learn==0.19.2
		scipy==1.2.0
		six==1.10.0
		```
- `analysis.R` runs all models and outputs every figure and table used in the paper and supplementary materials.
	* Running time:
		```
		real	46m13.615s
		user	44m11.899s
		sys	1m26.153s
		```
	* Running R version 3.5.2 (2018-12-20), required packages:
		```
		ggcorrplot==0.1.3
		ggjoy==0.4.1
		ggplot2==3.2.0
		gridExtra==2.3
		irr==0.84.1
		lmtest==0.9-37
		mediation==4.4.7
		MLmetrics==1.1.1
		sampleSelection==1.2-6
		quanteda==1.5.0
		sandwich==2.5-1
		stargazer==5.2.2
		stm==1.3.3
		xtable==1.8-4
		```
