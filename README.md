# R code for paper Precision of Estimates of Nonresponse Bias in Means by Eckman, Unangst, Dever, and Antoun

All files also in [OSF code repository](https://osf.io/rbzyd/?view_only=)

[Published paper](https://academic.oup.com/jssam/article-abstract/11/4/758/6619042?redirectedFrom=fulltext)

Run code in this order (see master.R):

1_sims.R 

	Generates the populations and sample draws for the simulation study
	Calls in 1.1_sims_continuous.R, 1.2_sims_binomial.R, and 1.3_sims_poisson.R. Each sims program (1.1, 1.2, 1.3) sets the parameters for the simulated populations. The corresponding functions programs (1.1_functions_cntns.R, 1.2_functions_binom.R, 1.3_functions_poisson.R) generate the populations, draw the samples, and calculate the standard error estimates for the nonresponse bias estimate from each sample draw
 

2_process.R

	Processes and restructures the results from the simulation study to prepare for program 4_results.R
	Calls in 2.1_rq1_datasets.R and 2.2_rq2_datasets.R

3_examples.R

	Processes and produces estimates of nonresponse bias for GSS and LISS datasets 
	Calls in 3.1_prepGSS.R and 3.2_prepLISS.R

4_results.R

	Produces the graphs presented in the paper 

for_paper.Rmd -- numbers mentioned in paper

Subfolder structure -- programs expect these subfolders:
\normal\
\binomial\
\poisson\
\outputs\ -- graphs will be saved here
\example_data\ -- should contain LISS and GSS data
	must be downloaded separately
	files needed:
		GSS.dat
		GSS.dct
		GSS_panel2010w123.dta
		jq13b_EN_2.0p.dta
		jf13a_EN_1.0p.dta
	see 3.1 and 3.2 R code for download information

0_setup.R is called by other programs

edit this file carefully, especially:

	num.cores -- number of CPUs machine has. 16 cores will allow simultaneous processing of 16 populations
 
	full or test sample sizes -- comment out test sizes if you want full sizes
 
	full sizes reported in paper, but can take > 24 hours to run, depending on number of cores

Final run of code used 28 core machine and took ~30 hours to run the entire set of programs
