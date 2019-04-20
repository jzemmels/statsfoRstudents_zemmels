#' @name hypTest_prompt
#' @export
#' @author Joe Zemmels
#'
#' @title Returns a randomly selected hypothesis test prompt
#'
#' @description Returns a randomly selected hypothesis test prompt as a list
#'
#' @return a randomly selected hypothesis test prompt as a list
#'
#' @examples
#' hypTest_prompt()
#'
#' @importFrom purrr flatten

hypTest_prompt <- function(){
  Q1 <- list(prompt = "According the the U.S. Department of Education, full-time graduate students receive an average salary of $12,800. The dean of graduate studies at a large state university in PA claims that his graduate students earn more than this. He surveys 46 randomly selected students and finds their average salary is $13,445 with a standard deviation of $1800. With alpha = 0.05, is the dean’s claim correct?",
             nullVal = 12800,
             altDir = ">",
             altVal = 12800,
             testStat = (13445-12800)/(1800/sqrt(46)),
             signifLevel = .05,
             decision = "Reject Null",
             source = "https://sites.psu.edu/stat200psbsp2013/2013/03/19/one-sample-z-test-problems-with-solutions/")

  Q2 <- list(prompt = "A health researcher read that a 200-pound male can burn an average of 524 calories per hour playing tennis. 37 males were randomly selected and the mean number of calories burned per hour playing squash was 534.8 with a standard deviation of 45.9 calories. Do squash players burn more calories per hour than tennis players? Test with alpha = 0.01.",
             nullVal = 524,
             altDir = ">",
             altVal = 524,
             testStat = (534.8-524)/(45.9/sqrt(37)),
             signifLevel = .01,
             decision = "Fail to Reject Null",
             source = "https://sites.psu.edu/stat200psbsp2013/2013/03/19/one-sample-z-test-problems-with-solutions/")

  Q3 <- list(prompt = "To see if young men ages 8-17 years spend a different amount than the national average of $24.44 per shopping trip to a local mall, the manager surveyed 30 young men. She found that the average amount spent per trip was $23.37 with a standard deviation of $3.70. With alpha = 0.05, can it be concluded that 8-17 years old spend a different amount at the local mall than the national average?",
             nullVal = 24.44,
             altDir = intToUtf8("8800"),
             altVal = 24.44,
             testStat = (23.37-24.44)/(3.70/sqrt(30)),
             signifLevel = .05,
             decision = "Fail to Reject Null",
             source = "https://sites.psu.edu/stat200psbsp2013/2013/03/19/one-sample-z-test-problems-with-solutions/")

  Q4 <- list(prompt = "A herd of 1,500 steer was fed a special high‐protein grain for a month. A random sample of 29 were weighed and had gained an average of 6.7 pounds. If the standard deviation of weight gain for the entire herd is 7.1 and we choose alpha = .1, test the hypothesis that the average weight gain per steer for the month was more than 5 pounds.",
             nullVal = 5,
             altDir = ">",
             altVal = 5,
             testStat = (6.7-5)/(1.7/sqrt(29)),
             signifLevel = .1,
             decision = "Fail to Reject Null",
             source = "https://www.cliffsnotes.com/study-guides/statistics/univariate-inferential-tests/one-sample-z-test")

  Q5 <- list(prompt = "In national use, a vocabulary test is known to have a mean score of 68 and a standard deviation of 13. A class of 19 students takes the test and has a mean score of 65. Is the class typical of others who have taken the test? Assume a significance level of alpha = 0.05.",
             nullVal = 68,
             altDir = intToUtf8("8800"),
             altVal = 68,
             testStat = (65-68)/(13/sqrt(19)),
             signifLevel = .05,
             decision = "Fail to Reject Null",
             source = "https://www.cliffsnotes.com/study-guides/statistics/univariate-inferential-tests/one-sample-z-test")

  Q6 <- list(prompt = "Twenty five high school students complete a preparation program for taking the SAT test. The mean of these scores is 536.00.  We know that the population average for SAT scores is 500 with a standard deviation of 100. The question is, are these students’ SAT scores significantly greater than a population mean of 500 with a population standard deviation of 100? Assume alpha = .05.",
             nullVal = 500,
             altDir = ">",
             altVal = 500,
             testStat = (536-500)/(100/sqrt(25)),
             signifLevel = .05,
             decision = "Reject Null",
             source = "https://sites.berry.edu/vbissonnette/index/stats-homework/documentation/one-sample-z-test/one-sample-z-test-solution/")

  Q7 <- list(prompt = "In the population, the average IQ is 100 with a standard deviation of 15. A team of scientists wants to test a new medication to see if it has either a positive or negative effect on intelligence, or no effect at all. A sample of 30 participants who have taken the medication has a mean of 140. Did the medication affect intelligence, using alpha = 0.05?",
             nullVal = 100,
             altDir = intToUtf8("8800"),
             altVal = 100,
             testStat = (140-100)/(15/sqrt(30)),
             signifLevel = .05,
             decision = "Reject Null",
             source = "http://www.statisticslectures.com/topics/onesamplez/")


  Q8 <- list(prompt = "A teacher wanted to know how well a class of gifted students performs relative to her other classes. She administers a standardized test with a mean of 50 and standard deviation of 10. Her class of 31 students has an average score of 55. Assuming alpha = .01, Is there evidence to suggest that her class performs better relative to other classes?",
             nullVal = 50,
             altDir = ">",
             altVal = 50,
             testStat = (55-50)/(10/sqrt(31)),
             signifLevel = .01,
             decision = "Reject Null",
             source = "http://www.usablestats.com/lessons/zexamples")

  Q9 <- list(prompt = "A rental car company claims the mean time to rent a car on their website is 60 seconds with a standard deviation of 30 seconds. A random sample of 36 customers attempted to rent a car on the website. The mean time to rent was 75 seconds. Is this enough evidence to contradict the company's claim? ",
             nullVal = 60,
             altDir = ">",
             altVal = 60,
             testStat = (75-60)/(30/sqrt(36)),
             signifLevel = .01,
             decision = "Reject Null",
             source = "http://www.usablestats.com/lessons/zexamples")

  Q10 <- list(prompt = "Blood glucose levels for obese patients have a mean of 100 with a standard deviation of 15. A researcher thinks that a diet high in raw cornstarch will have a positive or negative effect on blood glucose levels. A sample of 30 patients who have tried the raw cornstarch diet have a mean glucose level of 140. Test the hypothesis that the raw cornstarch had an effect if alpha = .05.",
              nullVal = 100,
              altDir = intToUtf8("8800"),
              altVal = 100,
              testStat = (140-100)/(15/sqrt(30)),
              signifLevel = .05,
              decision = "Reject Null",
              source = "https://www.statisticshowto.datasciencecentral.com/probability-and-statistics/hypothesis-testing/#HTExamples")

  randQuestion <- sample(list(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10),1)
  return(purrr::flatten(randQuestion))
}
