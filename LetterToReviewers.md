---
output:
  pdf_document: default
  html_document: default
---
# Letter to Reviewers

Dear 2NN,

thank you very much for your valuable comments!

To a large extend we have been following your suggestions. We would like to let you know:

- A sample and a discussion regarding the actual parameter estimates of the coefficients are now included, to address the non shrunken covariates in the lasso. 

- Thank you for the tip that lasso in grplasso and gglasso are differently implemented, however, we apply the gglasso only due to no existing cv function in grouplasso of our knowledge. Therefore we never use grouplasso for the cv, only to obtain the lambdagrid to inspect and optimize through gglasso. But for future reference, this is something we will keep in mind when choosing packages to apply in the next project.

- A justification why a split into training and test set is not affordable was included now. The variance is actually not super huge, but still to avoid effect depending on the particular split, we decide to do inference only with methods that do not require a test set.


Best regards,
Yellow Submarine