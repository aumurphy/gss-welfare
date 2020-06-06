# gss-welfare
Conditional Average Treatment Effect Estimation of US Attitude on Welfare Assistance
and ECON 293 @ Stanford

The General Social Survey (GSS) aims to monitor social change in the United States of America by gathering data on contemporary American trends, behaviors, and attitudes. Since 1972, it has conducted roughly biannual surveys on various topics ranging from science and health, to politics and discrimination. The GSS data are among the most widely studied United States survey datasets by researchers in the social sciences. In our project, we analyze the Trends in National Spending Priorities Survey which evaluates the attitude of Americans toward government spending on a number of programs. 

This analysis studies the response to spending questions from the GSS, influenced by the analysis done in [Modeling Heterogeneous Treatment Effects in Survey Experiments with Bayesian Additive Regression Trees](http://www.donaldgreen.com/wp-content/uploads/2015/09/BART-Green-Kern-POQ-2012.pdf). In that paper, Green and Kern analyzed the well-established American dislike of programs labeled as 'welfare' by comparing how many respondents believe that the United States is spending too much money on 'welfare programs' vs how many respondents believe that the United States is spending too much money on 'assisting the poor'. In addition, we calculate heterogeneous treatment effects of respondents preferences for a government spending on assisting African Americans.

The text of the question is given below, and respondents were given one of the two options: 

> We are faced with many problems in this country, none of which can be solved easily or inexpensively. I'm going to name some of these problems, and for each one I'd like you to tell me whether you think we're spending too much money on it, too little money, or about the right amount... Are we spending too much, too little, or about the right amount on **Improving the conditions of Blacks** or **Assistance to Blacks**
>
- Too little
- About right
- Too much
- Don't know

We analyze the conditional average treatment effect of how many respondents selected 'Too much' is being spent on "Improving the conditions of Blacks" vs how many respondents selected 'Too much' is being spent on "Assistance to Blacks".
