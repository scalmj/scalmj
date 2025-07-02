
INFO<-list()
INFO[["adjusting"]]<-"<p>Here you can obtain an adjusting function to adjust the target scores
                    by continuous or categorical predictors.</p>
                    <p> Please select:</p>
                    <ul>
                    <li> The test score (<i> Raw score</i>)  </li>
                    <li> At least one covariate (<i>Continuous Pretictors</i>) </li>
                    <li> If available, one factor (<i>Dichotomous Pretictors</i>)  </li>
                    </ul>
                   <p> The output returns the function to calculate the adjusted score, 
                 the equivalent scores, and the percentile table based on the adjusted scores.
                   </p>
                  <p> Please select <b> Model selection</b> and  <b> Terms refinements</b> options to 
                      customize the selection process.
                   </p>

                   "

INFO[["adjusting_step"]]<-"<p>Here you can obtain an adjusting function to adjust the target scores
                    by  continuous or categorical predictors.</p>
                    <p> The selected method is <b> stepwise </b>:</p>
                    <ul>
                    <li> For each continuous predictor, one simple regressions for each transformation is estimated (Table <i>Univariate</i>)</li>
                    <li> The best transformation of each variable is selected based on the lowest AIC, highest R^2 </li>
                    <li> blablab  </li>
                    </ul>
                    
                   "
INFO[["adjusting_sig"]]<-"<p>Here you can obtain an adjusting function to adjust the target scores
                    by  continuous or categorical predictors.</p>
                    <p> The selected method is <b> Keep significant </b>:</p>
                    <ul>
                    <li> For each continuous predictor, one simple regressions for each transformation is estimated (Table <i>Univariate</i>)</li>
                    <li> The best transformation of each variable is selected based on the lowest AIC, highest R^2 </li>
                    <li> blablab  </li>
                    </ul>
                   "

INFO[["adjusting_comb"]]<-"<p>Here you can obtain an adjusting function to adjust the target scores
                    by continuous or categorical predictors.</p>
                    <p> The selected method is <b> Transformations combinations </b>:</p>
                    <ul>
                    <li> All possible combinations of transformations of the (Table <i>Transformation combinations details</i>)</li>
                    <li> blablabXXX  </li>
                    </ul>
                   "
