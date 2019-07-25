library(ggcorrplot)
library(ggjoy)
library(ggplot2)
library(gridExtra)
library(irr)
library(lmtest)
library(mediation)
library(MLmetrics)
library(sampleSelection)
library(quanteda)
library(sandwich)
library(stargazer)
library(stm)
library(xtable)

set.seed(1234)

'%!in%' <- function(x,y)!('%in%'(x,y))

## Read data

data <- read.csv('../data/survey_data.csv')
treatments <- c("Sensational", "Outgroup", "Peer Violence")
covariate_labs <- c(treatments, "Auth. (Log)", "Ethno. (Log)", "Symb. Racism (Log)", "South", "Republican", "Outgroup x Auth. (Log)", "Outgroup x Ethno (Log)", "Outgroup x Symb. Racism (Log)")

## Plot themes

cbPalette <- c("#000000", "#009E73", "#E79F00", "#0072B2", "#D55E00", "#CC79A7", "#F0E442", "#9AD0F3", "#FFFFFF")
shapes <- c(19, 17, 15, 18, 0, 1, 2, 3, 4)
theme <- theme_minimal() +
	theme(
			plot.title = element_text(face="bold"),
			legend.title.align = 0.5,
			legend.title = element_blank(),
			legend.justification = c(0, 0), 
			legend.background = element_rect(color="grey80"),
	)

## ATE function
run_ATE_mod <- function(dvs, ivs, emotion=FALSE) {
	i <- 0
	mods <- list()
	plot_df <- data.frame()
	for (dv in dvs) {
		form <- as.formula(paste(dv, " ~ factor_sensational + factor_outgroup + factor_peer_viol", sep=""))
		if (emotion == TRUE) {
			form_full <- as.formula(paste(dv, " ~ factor_sensational*factor_outgroup*factor_peer_viol", sep=""))
		} else {
			form_full <- as.formula(paste(dv, " ~ factor_sensational + factor_outgroup + factor_peer_viol + auth_log*factor_outgroup + ethno_og*factor_outgroup + symb_rac_log*factor_outgroup + south + pi_R", sep=""))
		}
		mod <- lm(as.formula(form), data = data)
		i <- i+1
		mods[[i]] <- mod
		mod_full <- lm(as.formula(form_full), data = data)
		i <- i+1
		mod_full <- coeftest(mod_full, vcov = vcovHC(mod_full, type="HC1"))
		mods[[i]] <- mod_full
		conf <- confint(mod)
		for (iv in ivs) {
			b <- coef(summary(mod))[iv,][[1]]
			se <- coef(summary(mod))[iv,][[2]]
			ci <- confint(mod)[iv, ]
			vals <- data.frame(
				beta = b,
				ci_bot = ci[[1]],
				ci_top = ci[[2]],
				out = dv,
				iv = iv
			)
			plot_df <- rbind(plot_df, vals)
		}
	}
	return(list(mods = mods, plot_df = plot_df))
}

## Plot effects function
plot_effects <- function(df, fn, on, pd=0.9, lp1=.7, lp2=.8, lpb=FALSE, lfont=9) {
	if (lpb == TRUE) {
		theme <- theme + theme(legend.position='bottom')
	} else {
		theme <- theme + theme(legend.position=c(lp1, lp2))
	}
	ggplot(df, aes(x=iv, y=beta)) +
		geom_point(aes(color = out, shape = out), size = 2, position = position_dodge(pd), stat="identity") +
		geom_errorbar(aes(ymin = ci_bot, ymax = ci_top, color = out), position = position_dodge(pd), width = 0.25) +
		scale_x_discrete(labels = covariate_labs) +
		xlab("Treatment") +
		ylab("Difference in Means") +
		geom_hline(yintercept = 0, color = gray(1/2), lty = 2) +
		scale_color_manual(values=cbPalette, labels = on) +
		scale_shape_manual(values = shapes, labels = on) +
		theme + 
		theme(
			legend.text=element_text(size=lfont)
		)
	ggsave(fn, width = 7.5, height = 5)
}

## Save TeX tables function
save_tables <- function(mods, fn, on, title, cl=covariate_labs, stretch=TRUE) {
	output <- capture.output(stargazer(mods, title=title, header=FALSE, omit = c("Constant", "(Intercept)"), table.placement = "H", omit.stat=c("rsq", "adj.rsq", "f", "ser", "ll", "aic"),dep.var.labels.include=FALSE, model.names=FALSE, covariate.labels=cl, column.labels=rep(on, each=2)))
	if (stretch == TRUE) {
		output <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", output, fixed=TRUE)
		output <- gsub("\\end{tabular}", "\\end{tabular}}", output, fixed=TRUE)
	}
	cat(output, file=fn, sep="\n")
}

## ATE for comment text outcomes (without Heckman correction)

ivs <- c("factor_sensational", "factor_outgroup", "factor_peer_viol")
dvs <- c("violence_com", "extrajudicial_com")
on <- c("Violence", "Extrajudicial Violence")

comment <- run_ATE_mod(dvs, ivs)
fn <- paste("../figures/ATE_com.pdf", sep="")
#plot_effects(comment$plot_df, fn, on)
#save_tables(comment$mods, paste("../tables/com_ate.tex", sep=""), on, title)

## ATE Heckman Selection Model for Comments

# ATE for commenting

commenter <- run_ATE_mod(c("commenter"), ivs)
title <- "OLS Model for Propensity to Post a Comment"
save_tables(commenter$mods, "../tables/ATE_commenter.tex", c("Posted a Comment"), title, stretch=FALSE)

# Demonstrate non-random selection into commenting
selection <- glm(commenter ~ as.numeric(gender) + age + education + auth_log + ethno_og + symb_rac_log + south + pi_R, family = binomial(link = "logit"), data = data)
title <- "Logit Model for Propensity to Post a Comment"
save_tables(selection, "../tables/com_selection.tex", c("Posted a Comment"), title, cl=c("Gender", "Age", "Education", "Authoritarian (Log)", "Ethnocentrism (Log)", "Symbolic Racism (Log)", "South", "Republican"), stretch=FALSE)

# Correct for non-random selection using a 2-step Heckman selection model
plot_df <- data.frame()
for (dv in c('violence', 'extrajudicial')) {
	out_form <- as.formula(paste(dv, "_com ~ factor_sensational + factor_outgroup + factor_peer_viol", sep=""))
	heck_mod <- selection(selection = commenter ~ gender + age + education + auth_log + ethno_og + symb_rac_log + south + pi_R, outcome = out_form, data = data, method = "2step")
	conf <- confint(heck_mod)
	for (iv in ivs) {
		b <- coef(summary(heck_mod))[iv,][[1]]
		se <- coef(summary(heck_mod))[iv,][[2]]
		ci <- confint(heck_mod)[iv, ]
		vals <- data.frame(
			beta = b,
			ci_bot = ci[[1]],
			ci_top = ci[[2]],
			out = dv,
			iv = iv
		)
		plot_df <- rbind(plot_df, vals)
	}
}
title <- "Heckman 2-Step Selection Model OLS Model for Expressed Support for Violence, Extrajudicial Violence, and Lethal Violence in Comments"
plot_effects(plot_df, paste("../figures/ATE_com_heckman.pdf", sep=""), on, lpb=TRUE)
save_tables(heck_mod, paste("../tables/com_ate_heck.tex", sep=""), on, title, cl=treatments, stretch=FALSE)

## ATE for open ended text outcomes

dvs <- c("violence_oe", "extrajudicial_oe")

open_ended <- run_ATE_mod(dvs, ivs) # ATE for open-ended; 
title <- "OLS Model for Expressed Support for Violence, Extrajudicial Violence, and Lethal Violence in Open-ended Responses"
fn <- paste("../figures/ATE_oe.pdf", sep="")
plot_effects(open_ended$plot_df, fn, on)
save_tables(open_ended$mods, paste("../tables/oe_ate.tex", sep=""), on, title)

## Behavioral Outcomes

dvs <- c('com_viol_like','com_pos_like', 'com_viol_report', 'commenter')
on <- c("Liked Viol.", "Liked Non-viol.", "Reported Viol.", "Wrote Comment")

behavioral <- run_ATE_mod(dvs, ivs)
title <- "OLS Models for Behavioral Indicators"
fn <- paste("../figures/ATE_behavioral.pdf", sep="")
plot_effects(behavioral$plot_df, fn, on, pd = .5, lpb=TRUE)
save_tables(behavioral$mods, paste("../tables/behavioral_ate.tex", sep=""), on, title)

## Punitiveness

dvs <- c('punitive_refugee', 'punitive_mus_imm', 'punitive_mus_mon', 'punitive_mus_reg')
on <- c("Refugees", "Muslim Immigration", "Monitoring", "Registry")

punitive <- run_ATE_mod(dvs, ivs)
title <- "OLS Models for Attitudes Toward Outgroups"
fn <- paste("../figures/ATE_punitive.pdf", sep="")
plot_effects(punitive$plot_df, fn, on, pd = .5, lpb = TRUE)
save_tables(punitive$mods, paste("../tables/punitive_ate.tex", sep=""), on, title)

## Emotions

dvs <- c('emo_ang_hom', 'emo_dis_hom', 'emo_fea_hom', 'emo_anx_hom')
on <- c("Anger", "Disgust", "Fear", "Anxiety")

emotion <- run_ATE_mod(dvs, ivs)
title <- "Emotional Responses by Treatment Condition"
fn <- paste("../figures/ATE_emotion.pdf", sep="")
plot_effects(emotion$plot_df, fn,on, pd = .5, lpb = TRUE)
save_tables(emotion$mods, paste("../tables/emotion_ate.tex", sep=""), on, title)

## Support for Punishment for Alleged Homicide Perpetrator

dvs <- c('pun_nopu_hom', 'pun_comm_hom', 'prison_hom', 'lethal_hom', 'elethal_hom', 'torture', 'detain')
on <- c("No Punishment", "Community Service", "Prison", "Lethal", "Extreme Lethal", "Torture", "Detainment")

punish_hom <- run_ATE_mod(dvs, ivs)
title <- "Attitudes Toward Punishment of the Alleged Perpetrator"
fn <- paste("../figures/ATE_punish_hom_outcomes.pdf", sep="")
plot_effects(punish_hom$plot_df, fn, on, pd = .5, lpb = TRUE)
save_tables(punish_hom$mods, paste("../tables/punish_hom_ate.tex", sep=""), on, title)

## Support for Punishment for Mob

dvs <- c('pun_nopu_mob', 'pun_comm_mob', 'legal_def')
on <- c("No Punishment", "Community Service", "Legal Defense")

punish_mob <- run_ATE_mod(dvs, ivs)
title <- "Attitudes About Punishment for Mob Attack on the Alleged Perpetrator"
fn <- paste("../figures/ATE_punish_mob_outcomes.pdf", sep="")
plot_effects(punish_mob$plot_df, fn, on, pd = .5, lpb = TRUE)
save_tables(punish_mob$mods, paste("../tables/punish_mob_ate.tex", sep=""), on, title)

## Attitudes Toward Judicial Punishment of the Alleged Perpetrator

dvs <- c('lenient_hom', 'prison_hom', 'lethal_hom', 'elethal_hom')
on <- c("Lenient", "Prison", "Lethal", "Extreme Lethal")

punish_hom <- run_ATE_mod(dvs, ivs)
title <- "Attitudes Toward Judicial Punishment of the Alleged Perpetrator"
save_tables(punish_hom$mods, paste("../tables/punish_jud_ate.tex", sep=""), on, title)

## Direct Attitudes Toward Extrajudicial Punishment of the Alleged Perpetrator

dvs <- c('torture', 'detain')
on <- c("Torture", "Detainment")

extrajudicial_direct <- run_ATE_mod(dvs, ivs)
title <- "Direct Questions About Extrajudicial Punishment of the Alleged Perpetrator"
save_tables(extrajudicial_direct$mods, paste("../tables/punish_exjud_d_ate.tex", sep=""), on, title, stretch=FALSE)

## Indirect Attitudes Toward Extrajudicial Punishment of the Alleged Perpetrator

dvs <- c('pun_nopu_mob', 'pun_comm_mob', 'legal_def')
on <- c("No Punish.", "Comm. Service", "Leg. Defense")

extrajudicial_indirect <- run_ATE_mod(dvs, ivs)
title <- "Indirect Questions About Extrajudicial Punishment of the Alleged Perpetrator"
save_tables(extrajudicial_indirect$mods, paste("../tables/punish_exjud_i_ate.tex", sep=""), on, title)

## Additional Text Responses (Comments)

dvs <- c('cog_dissonance_com', 'info_seeking_com', 'fake_news_com', 'racism_hate_com')
on <- c("Cognitive Dissonance", "Info. Seeking", "Fake News", "Racism", "Social Desirability")

additional_text_com <- run_ATE_mod(dvs, ivs)
title <- "Additional Text Responses (Comments)"
fn <- paste("../figures/additional_text_com.pdf", sep="")
plot_effects(additional_text_com$plot_df, fn, on, pd = .5, lpb = TRUE)
save_tables(additional_text_com$mods, paste("../tables/additional_text_com.tex", sep=""), on, title)

## Additional Text Responses (Open Ended)

dvs <- c('cog_dissonance_oe', 'info_seeking_oe', 'fake_news_oe', 'racism_hate_oe', 'social_desirability_race_oe')

additional_text_oe <- run_ATE_mod(dvs, ivs)
title <- "Additional Text Responses (Open Ended)"
fn <- paste("../figures/additional_text_oe.pdf", sep="")
plot_effects(additional_text_oe$plot_df, fn, on, pd = .5, lpb = TRUE)
save_tables(additional_text_oe$mods, paste("../tables/additional_text_oe.tex", sep=""), on, title)

## Measurement Validity Check

title <- "Willingness to Donate to Legal Defense Fund for Mob Attackers and Expressed Support for Extrajudicial Violence in Open-ended Responses"
cl <- c("Legal Defense Fund")
on <- c("Extrajudicial")
val_check_mod <- lm(extrajudicial_oe ~ legal_def, data=data)
save_tables(val_check_mod, paste("../tables/val_check_mod.tex", sep=""), on, title, cl=cl, stretch=FALSE)

## Causal Mediation Analysis

mediator_vars <- c("emo_anx_hom", "emo_fea_hom", "emo_ang_hom")
mediator_names <- c("Anxiety", "Fear", "Anger")
dv_vars <- c("legal_def", "extrajudicial_oe")
dv_names <- c("Legal Fund", "Extrajudicial")
for (m in seq_along(mediator_vars)) {
	fn <- paste("../figures/med_", mediator_vars[m], ".tex", sep="")
	out <- ""
	for (dv in seq_along(dv_vars)) {
		cols <- c(dv_vars[dv], mediator_vars[m], "factor_sensational")
		med_form <- as.formula(paste(mediator_vars[m], "~ factor_sensational + auth_log + symb_rac_log + ethno_og + south"))
		med_fit <- lm(med_form, data = data[complete.cases(data[, cols]),])
		m_b <- coef(med_fit)[[2]]
		m_p <- summary(med_fit)$coefficients[2,4]
		out_form <- as.formula(paste(dv_vars[dv], "~", mediator_vars[m], "+ factor_sensational + auth_log + symb_rac_log + ethno_og + south"))
		out_fit <- lm(out_form, data = data[complete.cases(data[, cols]),])
		o_b <- coef(out_fit)[[2]]
		o_p <- summary(out_fit)$coefficients[2,4]
		med <- mediate(med_fit, out_fit, treat = "factor_sensational", mediator = mediator_vars[m], control.value = 0, treat.value = 1, sims = 100)
		sm <- summary(med)
		out <- paste(out, paste(
			"\\tikzset{mynode/.style={draw,text width=1.5in,align=center}}\\begin{tikzpicture}\\node[mynode] (m){", mediator_names[m], "};\\node[mynode,below left=of m](a) {Sensational};\\node[mynode,below right=of m](b) {", dv_names[dv], "};\\draw[-latex] (a.north) -- node[auto,font=\\footnotesize] {$b=", round(m_b, 3), "$, $p=", round(m_p, 3), "$} (m.west);\\draw[-latex] (m.east) -- node[auto,font=\\footnotesize] {$b=", round(o_b, 3), "$, $p=", round(o_p, 3), "$} (b.north);\\draw[-latex] (a.east) -- node[below=3mm,font=\\footnotesize,align=center] {Direct effect, $b=", round(sm$z0, 3), "$, $p=", round(sm$z0.p, 3), "$ \\\\ Indirect effect, $b=", round(sm$d0, 3), "$, 95\\% CI [", round(sm$d0.ci[1], 3), ", ", round(sm$d0.ci[2], 3), "]}(b.west);\\end{tikzpicture}\\vspace{2em}\n\n", sep=""
			)
		)
	}
	cat(out, file=fn, sep="\n")
}

## Descriptive Stats

print_stats <- function(title, vars, fn) {
	output <- capture.output(stargazer(data[vars], title=title, header=FALSE, table.placement = "H", omit.summary.stat = c("p25", "p75")))
	cat(output, file=fn, sep="\n")
}

title <- "Descriptive Statistics for Survey Responses"
vars <- c("age", "education", "auth_log", "ethno_og", "symb_rac_log", "emo_ang_hom", "emo_sat_hom", "emo_sad_hom", "emo_dis_hom", "emo_hap_hom", "emo_fea_hom", "emo_anx_hom", "pun_nopu_hom", "pun_comm_hom", "pun_short_pris_hom", "pun_long_pris_hom", "pun_life_pris_hom", "pun_exe_inje_hom", "pun_exe_fire_hom", "pun_exe_hang_hom", "pun_exe_publ_hom", "torture", "detain", "pun_nopu_mob", "pun_comm_mob", "legal_def", "punitive_hom", "punitive_refugee", "punitive_mus_imm", "punitive_mus_mon", "punitive_mus_reg")
print_stats(title, vars, "../tables/d_stats_survey.tex")

title <- "Descriptive Statistics for Open-ended Responses"
vars <- c("violence_oe", "lethal_oe", "extrajudicial_oe", "dehumanizing_oe", "demonizing_oe", "group_oe", "ingroup_oe", "anger_oe", "disgust_oe", "sadness_oe", "fear_oe", "anxiety_oe")
print_stats(title, vars, "../tables/d_stats_oe.tex")

title <- "Descriptive Statistics for Behavioral Indicators"
vars <- c("video_link", "com_viol_report", "com_info_report", "com_pos_report", "com_viol_like", "com_info_like", "com_pos_like", "commenter", "violence_com", "lethal_com", "extrajudicial_com", "dehumanizing_com", "demonizing_com", "group_com", "ingroup_com", "anger_com", "disgust_com", "sadness_com", "fear_com", "anxiety_com")
print_stats(title, vars, "../tables/d_stats_beh.tex")

## Distribution of Likes

comment_types <- c("com_viol_like", "com_pos_like", "com_info_like")
comment_names <- c("Liked Violent Comment", "Liked Positive Comment", "Liked Info-Seeking Comment")

like_data <- data[complete.cases(data[, ivs]),]
for (t in ivs) {
	like_data[,t] <- as.factor(like_data[,t])
}
ylims <- list(c(.475, .6), c(.65, .85), c(.5, .65))
for (i in seq_along(comment_types)) {
	plots <- list()
	for (j in seq_along(ivs)) {
		plot <- ggplot(like_data, aes_string(ivs[j], comment_types[i], color = ivs[j])) +
		stat_summary(fun.y = mean, geom = "line", aes_string(group = ivs[j]), na.rm = TRUE) + 
		stat_summary(fun.y = mean, geom = "point", na.rm = TRUE, aes_string(color = ivs[j], shape = ivs[j]), size=3) + 
		stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.05, na.rm = TRUE) +
		theme_minimal() +
		theme(
			legend.position="none",
			plot.title = element_text(hjust = 0.5)
		) +
		labs(x = treatments[j], y = "Predicted Probability", title=comment_names[i]) +
		scale_color_manual(values = cbPalette) +
		scale_shape_manual(values = c(17, 15)) +
		scale_x_discrete(labels = c("Control", "Treatment")) +
		coord_cartesian(ylim = ylims[[i]])
		plots[[j]] <- plot
	}
	g <- do.call(grid.arrange, c(plots, ncol=3))
	ggsave(file=paste("../figures/", comment_types[i], ".pdf", sep=""), g)
}

## Correlation Plot for Measures of Support for Violence

corr_df <- na.omit(data[ ,c("violence_oe", "pun_nopu_mob","legal_def", "lethal_hom", "pun_life_pris_hom", "lenient_hom")])
names(corr_df) <- c("Violence (OE)", "No Punishment (Mob)", "Legal Defense (Mob)", "Death Penalty (AP)", "Life Prison (AP)", "Lenient Punishment (AP)")
corr_mat <- cor(corr_df)
corr_plot <- ggcorrplot(corr_mat, hc.order = TRUE, type = "lower", lab = TRUE, insig = "blank", colors=cbPalette[c(9,2,1)])
ggsave(file="../figures/corr_plot.pdf", corr_plot)

## Visualizations of Important Terms by Treatment Group

# Bootstrapped Regularized Logistic Regression for Feature Extraction and Weighting (See text_analysis.py)

for (i in seq_along(ivs)) {
	dat <- read.csv(paste("../data/", ivs[i], "_text_coefs.csv", sep=""))
	names(dat) <- c("feature_name", "beta")
	f1 <- mean(dat[dat$feature_name == 'f1','beta'])
	dat <- dat[dat$feature_name != 'f1',]

	vocab <- unique(dat$feature_name)

	# Identify all significant coefficients
	sig_vocab <- c()
	while (length(sig_vocab) < 70) {
		if (length(sig_vocab) == 0) {
			d <- 200
		} else {
			d <- d - 20
		}
		for (v in vocab) {
			if (v %!in% sig_vocab) {			
			  samples <- sort(dat[dat$feature_name == v, "beta"])
			  n <- length(samples)
			  if (sign(samples[floor(n/d)]) == sign(samples[n-floor(n/d)])) {
			    sig_vocab <- c(sig_vocab, v)
			  }
			}
		}
	}

	# Identify top 30 tokens in the vocabulary with the top mean coefficient value

	means <- c()
	for (v in sig_vocab) {
		means <- c(means, mean(dat[dat$feature_name == v, "beta"]))
	}

	top_n <- 30

	bot_ids <- sort(means, index.return=T)$ix[1:top_n]
	top_ids <- sort(means, index.return=T, decreasing=T)$ix[1:top_n]
	sig_dat <- dat[dat$feature_name %in% sig_vocab[c(top_ids, bot_ids)],]

	order <- sig_vocab[c(top_ids, rev(bot_ids))]
	sig_dat$feature_name = factor(sig_dat$feature_name, levels=unique(order))
	# Create joy plot of coefficient bootstrap distributions

	ggplot(sig_dat, aes(x = beta, y = factor(feature_name))) + 
	geom_joy() + 
	geom_vline(xintercept=0, color = gray(1/2), lty = 2) + 
	labs(
		x = "Term Log Odds",
		y = paste("Top", top_n * 2, "Significant Features"),
		title = paste(treatments[i], ", Model Performance (F1): ", round(f1, 2), sep="")
	) + theme(
		plot.title = element_text(size=12, face="bold"),
		legend.title.align = 0.5,
	)

	# Save as a vector graphic
	ggsave(paste("../figures/", ivs[i], "_terms.pdf", sep = ""), width = 8, height = 10, units="in")
}

## Visualizations of Important Topics by Treatment Group (STM)

# Reference: https://github.com/dondealban/learning-stm

# Process text data

text_data <- read.csv('../data/text_features.csv')
text_data <- na.omit(text_data)
survey_codes <- text_data$survey_code
text_data$survey_code <- NULL
oe_corpus <- corpus(as.character(text_data$text), docvars = text_data[,ivs])
oe_dfm <- dfm(oe_corpus)
oe_dfm_stm <- convert(oe_dfm, to = "stm")

# Select the best model based on semantic coherance, held out likelihood, and qualitative assessment of topic coherence

search <- manyTopics(oe_dfm_stm$documents, oe_dfm_stm$vocab, K = 2:30, prevalence = ~factor_peer_viol + factor_sensational + factor_outgroup, data=oe_dfm_stm$meta, runs = 10, seed = 1988)
mod <- search$out[[5]]
K <- mod$settings$dim$K

# Print table of most representative documents

thoughts <- findThoughts(mod, texts=survey_codes, n=2, topics=1:K)$docs
cols <- c('survey_code', 'react_mob', 'react_hom', ivs)

thoughts_df <- data[data$survey_code %in% unlist(thoughts), cols]
for (n in paste("Topic", 1:K)) {
	thoughts_df[thoughts_df$survey_code %in% unlist(thoughts[n]), 'Topic'] <- n
}

thoughts_df$Text <- paste(thoughts_df$react_hom, thoughts_df$react_mob)
thoughts_df <- thoughts_df[order(thoughts_df$Topic), c(7,8,4:6)]
names(thoughts_df) <- c(names(thoughts_df)[1:2], treatments)
print.xtable(xtable(thoughts_df), file = "../tables/stm_thoughts.tex", include.rownames=FALSE)

# Get words with top term score as labels

term_labels <- function(mod, n = ) {
	labs <- labelTopics(mod, n = n)
	return(apply(labs$score, 1, paste, collapse=', '))
}
labels_8 <- term_labels(mod, n = 8)

# Plot proportions

fn <- "../figures/topic_props.pdf"
proportion <- as.data.frame(colSums(mod$theta/nrow(mod$theta)))
prop_plot <- cbind(labels_8, proportion)
colnames(prop_plot) <- c("Labels", "Probability")
prop_plot <- prop_plot[order(-proportion), ] 
prop_plot$Labels <- factor(prop_plot$Labels, levels = rev(prop_plot$Labels))
prop_plot$Probability <- as.numeric(prop_plot$Probability)
prop_plot$Probability <- round(prop_plot$Probability, 4)

ggplot(prop_plot, aes(x = Labels, y = Probability)) + 
  geom_bar(aes(fill=Labels), width = .8, stat = "identity") +
  geom_text(aes(x = Labels, y=0, label = Labels),  position = position_dodge(width = 1), hjust = -0.025, color="white") +
  coord_flip() +
  scale_fill_manual(values=cbPalette[order(proportion)]) +
  scale_x_discrete(labels=paste("Topic",order(proportion))) +
  theme(
  	legend.position="none",
    aspect.ratio = 1/4,
    plot.background = element_rect(fill = "transparent", color = NA)
  )
ggsave(fn)

# Plot effect of treatments on topic probabilities

prep <- estimateEffect(1:K ~ factor_sensational + factor_outgroup + factor_peer_viol, mod, meta=oe_dfm_stm$meta, uncertainty="Global")
fn <- "../figures/topic_effects.pdf"

sp <- summary(prep)
plot_df <- data.frame()
for (t in 1:K) {
	tab <- sp$tables[[t]]
	for (iv in ivs) {
		b <- tab[iv, 1]
		se <- tab[iv, 2]
		ci <- b - (c(-1,1) * (1.96 * se))
		vals <- data.frame(
			beta = b,
			ci_bot = ci[[1]],
			ci_top = ci[[2]],
			out = paste("Topic", t),
			iv = iv
		)
		plot_df <- rbind(plot_df, vals)
	}
}

plot_effects(plot_df, fn, labels_8, pd = .5, lpb = TRUE, lfont=5)

## Intercoder reliability

jeff <- read.csv('../data/jeff_icr.csv')
blake <- read.csv('../data/blake_icr.csv')
cols <- c('cog_dissonance', 'fake_news', 'info_seeking', 'racism_hate', 'social_desirability', 'violence', 'lethal', 'extrajudicial', 'dehumanizing', 'demonizing', 'group', 'ingroup', 'anger', 'disgust', 'sadness', 'fear', 'anxiety')
colnames <- c('Cognitive Dissonance', 'Fake News', 'Info. Seeking', 'Racism/Hate', 'Social Desirability', 'Violence', 'Lethal Violence', 'Extrajudicial Violence', 'Dehumanizing', 'Demonizing', 'Group', 'Ingroup', 'Anger', 'Disgust', 'Sadness', 'Fear', 'Anxiety')
df <- data.frame()
for (i in seq_along(cols)) {
	col <- cols[i]
	k <- kappa2(cbind(jeff[,col],blake[,col]), 'unweighted')$value
	f <- F1_Score(jeff[,col], blake[,col])
	df <- rbind(df, data.frame(Category=colnames[i],Kappa=k,F1=f))
}

print.xtable(xtable(df, caption="Intercoder Reliability Measures"), caption.placement = "top", file = "../tables/icr.tex", include.rownames=FALSE)


