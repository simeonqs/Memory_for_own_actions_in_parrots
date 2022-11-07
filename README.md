# parrot_repeat

The R code and data needed to replicate results from the article:

```
Sara Torres Ortiz, Simeon Q. Smeele, Juliette Champenois et al. What did I do? Memory for own actions in parrots, 17 October 2022, PREPRINT (Version 1) available at Research Square [https://doi.org/10.21203/rs.3.rs-2098690/v1]
```
------------------------------------------------

**Abstract**

The ability to mentally represent and recall past own actions is a crucial prerequisite for mental self-representation and episodic memory. We studied whether blue-throated macaws, a social macaw species, could remember their previous actions. The parrots were trained to repeat four learned actions upon command. Test sessions included trials without repeat, repeat and double repeat trials intermixed to test if the parrots only repeated when requested and not relying on a representation of the last behavioral command. Following their success, the parrots also received sessions with increasing time delays preceding the repeat command and successfully mastered 12-15 sec delays. The parrots successfully transferred  the repeat command to 3 behaviors they had not been trained with spontaneously at first trial and in a second trial intermixed with the already trained actions (untrained repeat tests), which corroborates that successful repeating is not just an artifact of intense training. The results suggest that blue-throated macaws can mentally represent  and memorize their own actions This implies that an important aspect of self-representation has evolved and constituted a survival benefit in an avian group. This finding complies with the complex socio-ecological environment and lifestyle of parrots and previous demonstrations of their problem solving and planning abilities. Whether the parrots encode their memory explicitly thus relying on  a prepared behavioral response, or accidentally, hence relying on episodic-like memory needs to be addressed by future studies in which the parrots are requested to repeat their own spontaneous actions.

------------------------------------------------

**The folders contain:**

ANALYSIS:
  - CODE: the code to replicate results
  - DATA: raw data
  - RESULTS: results and figures

------------------------------------------------

**File information and meta data:**

Below are all files in the repository. The first bullet point under the path is a short explanation of the file. Other bullet points are meta data for the columns if relevant.

- README.md
	- overview of repo and all files
- .gitignore
	- which files not to sync to GitHub
- parrot_repeat.Rproj
	- R Studio Project file; if you open the code from this file all paths are relative to the main folder

- ANALYSIS/CODE/01_load_clean_data.R 
	- R script to load and clean all the data for later scripts, **remember to run this before any other script**, objects are not saved for later scripts but stored in the environment, if you want to source the scripts, do this from within an R session to make sure objects are available for later steps
- ANALYSIS/CODE/02_run_single_repeat_model.R
	- R script to run the model for the single repeat without delay using *ulam* from the *rethinking* package, the model fit is stored in an RData file in case the session crashes
- ANALYSIS/CODE/03_plot_single_repeat.R
	- R script to create the figure of the single repeat data and model results
- ANALYSIS/CODE/04_run_double_repeat_model.R
	- R script to run the model for the double repeat using *ulam* from the *rethinking* package, the model fit is stored in an RData file in case the session crashes
- ANALYSIS/CODE/05_plot_double_repeat.R
	-  R script to create the figure of the double repeat data and model results
- ANALYSIS/CODE/06_run_delay_model.R
	- R script to run the model for the single repeat with delay using *ulam* from the *rethinking* package, the model fit is stored in an RData file in case the session crashes
- ANALYSIS/CODE/07_plot_delay.R
	- R script to create the figure of the delayed repeat data and model results
	
NOTE: each code file contains additional information about author, date modified and description. 

- ANALYSIS/DATA/parrots/Delay-responses.csv
	- csv file with the data for the delayed single repeat
	- Animal: the name of the test subject
	- Session: index of the session
	- correct.or.not.1: response for first behaviour for the trial, `c` = correct response, `n` = incorrect response
	- correct.or.not.2: response for second behaviour for the trial, `c` = correct response, `n` = incorrect response
	- delay: duration in seconds of the delay between first and second behaviour
	- b1: first behaviour to be peformed
	- b2: second behaviour to be performed
- ANALYSIS/DATA/parrots/Parrots_repeat.csv 
	- csv file with the data for the single and double repeats without delay
	- Animal: the name of the test subject
	- Session: index of the session
	- Behaviour_1:  first behaviour to be peformed
	- Offered_1: first behaviour performed by the animal
	- Behaviour_2:  second behaviour to be peformed
	- Offered_2: second behaviour performed by the animal
	
- ANALYSIS/RESULTS/delay - scatter.pdf
	- pdf with the results of the delayed trials
- ANALYSIS/RESULTS/double - densities.pdf
	- pdf with the results of the double repeats
- ANALYSIS/RESULTS/single - densities.pdf
	- pdf with the results of the single repeats without delay
- ANALYSIS/RESULTS/m_delay.RData
	- RData object with the model fit from *ulam* for the delayed trials, can be loaded into R for plotting
- ANALYSIS/RESULTS/m_double.RData
	- RData object with the model fit from *ulam* for the double repeat trials, can be loaded into R for plotting
- ANALYSIS/RESULTS/m_single.RData
	- RData object with the model fit from *ulam* for the single repeat trials without delay, can be loaded into R for plotting

------------------------------------------------

**Maintainers and contact:**

Please contact Simeon Q. Smeele, <ssmeele@ab.mpg.de>, if you have any questions or suggestions. 




