
**Descriptive Statistics and Test of Association
ta FamilySupportive Moutcome,chi2 r
ta NewMH_Service Moutcome,chi2 r
ta NewYearofStudy Moutcome,chi2 r
ta NewAccomodation Moutcome,chi2 r
ta NewAgeCat Moutcome,chi2 r
ta Insuarance_Status Moutcome,chi2 r
ta TimeonMedia Moutcome,chi2 r
ta AwareofMHserviceatUniversity Moutcome,chi2 r
*Univariate Logistic Regression
logistic Moutcome i.FamilySupportive
logistic Moutcome i.NewMH_Service
logistic Moutcome i.NewYearofStudy
logistic Moutcome i.NewAccomodation
logistic Moutcome i.NewAgeCat
logistic Moutcome i.NewAgeCat
logistic Moutcome i.Insuarance_Status
logistic Moutcome i.TimeonMedia
logistic Moutcome i.AwareofMHserviceatUniversity
***Mental Health Model
logistic Moutcome i.FamilySupportive i.NewMH_Service i.NewYearofStudy i.NewAccomodation i.NewAgeCat i.Insuarance_Status i.TimeonMedia i.AwareofMHserviceatUniversity
**Multicollinearity test
regress Moutcome FamilySupportive NewMH_Service NewYearofStudy NewAccomodation NewAgeCat Insuarance_Status TimeonMedia AwareofMHserviceatUniversity
**Compuation of Variance Inflation Factor (VIF)
vif
*** Area Under Curve Plot-Model Diagnostic
lroc
