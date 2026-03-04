#Tables and visualization
kbl(
  cbind(
    (rbind(
      (tidy(Tot.nb2.1)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))),
      (tidy(Inc.nb2.zi.1)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))),
      (tidy(Hatc.p.1)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))),
      (tidy(Fled.p.1)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))))),
    (rbind(
      (tidy(Tot.nb2.2)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))),
      (tidy(Inc.nb2.zi.2)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))),
      (tidy(Hatc.p.2)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))),
      (tidy(Fled.p.2)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept")))))
  )[-c(1,2,9:12)],
  col.names = c("grp", "Term", "Estimate", "Std. Err.", "z value", "p value","Estimate", "Std. Err.", "z value", "p value"), 
  caption="summary of probability models shown in log(odds) scale",
  booktabs = T)%>%
  pack_rows(index = c("Eggs laid"= 6,
                      "incubation" = 6, 
                      "hatching" = 6, 
                      "fledging" = 6))%>%
  kable_styling(latex_options = "HOLD_position")


library(factoextra)
install.packages("factoextra")
