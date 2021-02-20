# Question for 22.02.2021

FB: What are the relative computation times for 1) to 6) in the hyperparameter tuning algorithm?
FB: Can this algorithm handle newly-incoming training data or has it to run from beginning again?
HMO: Would you use this algorithm for HP tuning or would you use something else? 





YL: why does the method still work if there is ground truth for validation.
YL: Can you elaborate more on how this method enables to gain new knowledge about the interactions between factors (hyperparameters)?

SR: In the paper the decision to do fold-over was chosen after the initial fractional factorial experiments, do you think this is correct or should criteria for this be set before-hand?
SR: The paper argues that less expert knowledge is required with their method, do you agree? (I personally don't because you now need expert knowledge on fractional factorial design, when to cross-fold, when to drop into RSM, what are accurate levels at the start etc...)
SR: The paper argues that the ntree was not relevant for their tested low level of 100 and high level of 500, but they still chose 250 on some "expert knowlegde" do you agree with that decision?
SR: They introduce the center-point approach to test for non-linear responses, but do not use it in their factorial design, only for the responce surface method (RSM), would you also do that or include it in earlier steps of this method?
