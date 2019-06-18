Audit Workflow
==========================

The audit workflow allows you to make a estimate of the population misstatement in an audit population using frequentist statistics.

Workflow
-----------
The audit workflow consists of four separate stages, each with their own purpose for the analysis:
- Planning: Compute the sample size that is required for your desired population statement
- Selection: Select the required observations from your population
- Execution: Annotate your data set with your assessment of the fairness of the selected observations
- Evaluation: Make a population statement based on your annotated selection

Default options
-------
### Population materiality:
- Absolute: Enter your population materiality as a monetary value
- Relative: Enter your population materiality as a percentage relative to the total value

### How would you like to evaluate your variables?
- Audit values: When selected, you will annotate the selection with the observations' true values
- Correct / Incorrect: When selected, you will annotate the selection with an indicator for whether the observations are correct (0) or incorrect (1)

Advanced options
-------
### Inherent risk and control risk:
- High: 100%
- Medium: 60%
- Low: 50%

### Expected errors:
- Absolute: Enter your expected errors as a number (e.g., 2 means that you expect two full errors in your selection)
- Relative: Enter your expected errors as a percentage relative to the total size of the selection

### Explanatory text:
- Enables explanatory text throughout the workflow to help you interpret the statistical results and procedure

### Planning distribution:
- Poisson: The poisson distribution for broken taints
- Binomial: The infinite population binomial distribution for complete taints
- Hypergeometric: The finite population hypergeometric distribution for complete taints

### Selection type:
- Monetary unit sampling: Performs selection proportional to the observations' book values
- Record sampling: Performs selection with equal probabilities

### Selection method
- Random sampling: Performs random selection with replacement 
- Cell sampling: Performs interval selection. Any observation that is larger than twice the interval will be selected multiple times.
- Systematic sampling: Performs interval selection. Any observation that is larger than the interval will be selected multiple times.

### Seed:
- Random number generator seed to make results reproducible

### Estimator
- Stringer: The Stringer bound

Default Output
-------

### Planning summary
- Materiality: The population materiality
- Inherent risk: Risk assessment for the inherent risk
- Control risk: Risk assessment for the control risk
- Expected errors: The number of expected errors in the selection
- Required sample size: The sample size that is required for your population statement

### Selection summary:
- Sample size: The size of the selected subset 
- % of total observations: The relative size of the subset
- % of total value: The relative value of the subset
- Interval: The size of the interval used in the selection method

### Evaluation summary:
- Materiality: The population materiality
- Sample size: The size of the selected subset
- Errors: The number of errors in the selection
- Total taining: The sum of the proportional errors
- x-% Confidence bound: The estimate of the maximum misstatement in percentages
- Maximum misstatement: The estimate of the maximum misstatement in monetary values

Tables and plots
-------

### Book value descriptives
- Produces a table containing several statistics about the book values including the population size, total value, mean, standard deviation and quartiles.

### Book value distribution
- Produces a histogram of the distribution of book values in the population. Important statistics like the mean, standard deviation, and quartiles are indicated with colors.

### Decision plot
- Produces a dot plot of the number of errors that are allowed in the selection before the population should be rejected.

### Display selected observations
- Produces a table containing the selected observations along with any additional observations inserted in the corresponding field

### Selection descriptives
- Produces a table containing descriptive information about numerical variables in the selection

### Most likely error (MLE)
- Adds a cell to the evaluation summary table containing an estimate of the errors in the total population

### Evaluation information
- Produces a bar chart comparing the materiality, maximum misstatement, most likely error, and expected errors

### Correlation plot
- Produces a scatter plot comparing book values of the selection against their audit values. Observations that are in error are colored in red.