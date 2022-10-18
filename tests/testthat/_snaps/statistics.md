# Confidence interval for the mean

    Code
      confidence_mean(x, type = "student")
    Output
         lower    upper 
      17.91768 22.26357 

---

    Code
      confidence_mean(x, type = "normal")
    Output
         lower    upper 
      18.00243 22.17882 

# Confidence interval for binomial proportions

    Code
      confidence_binomial(118, n = 236)
    Output
          lower     upper 
      0.4362086 0.5637914 

---

    Code
      confidence_multinomial(x)
    Output
                lower     upper
      [1,] 0.12234021 0.2276598
      [2,] 0.30308797 0.4369120
      [3,] 0.06663649 0.1533635
      [4,] 0.27911853 0.4108815

---

    Code
      confidence_multinomial(x, corrected = TRUE)
    Output
                lower     upper
      [1,] 0.11984021 0.2301598
      [2,] 0.30058797 0.4394120
      [3,] 0.06413649 0.1558635
      [4,] 0.27661853 0.4133815

