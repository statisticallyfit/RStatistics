conda create --name rdatasci_TEMP_env

# Activate!
conda activate rdatasci_TEMP_env

  # =======
  # core
  # =======
conda install -c r r=3.5.0 # need 3.5 for rstudio 1.2, no higher unfortunately
conda install -c rdonnelly rstudio=1.2.502


  # ==============
  # Data Science
  # ==============
conda install -c r r-rpart 
conda install -c r r-e1071 
conda install -c conda-forge r-gbm 
conda install -c conda-forge r-rsample 
conda install -c conda-forge r-tree 
conda install -c r r-randomforest 
conda install -c conda-forge r-factoextra
conda install -c conda-forge r-factominer
conda install -c r r-cluster 
conda install -c conda-forge r-nbclust 
conda install -c conda-forge r-dendextend
conda install -c r r-hmisc 
  ##- r-matrixstats
conda install -c r r-catools
conda install -c conda-forge r-aod
  ####- r-aed
  ####- r-dae
conda install -c r r-boot 
conda install -c r r-caret 
conda install -c r r-class 
conda install -c r r-car 
conda install -c conda-forge r-cardata
conda install -c conda-forge r-leaps
conda install -c r r-glmnet 
  #- r-nnet # feedforward neural networks # already a dependency
conda install -c r r-pls
conda install -c r r-lattice 
conda install -c r r-lme4 
conda install -c r r-nlme
#conda install -c chameroy r-aiccmodavg 
  ##- r-splines
conda install -c conda-forge r-geepack 



### clean up #####
conda clean -tipsy
##################



  # ==============
  # Plotting
  # ==============
conda install -c r r-ggplot2
conda install -c conda-forge r-ggfortify
conda install -c conda-forge r-ggally 
#conda install -c conda-forge r-autoplotly 
conda install -c conda-forge r-ggcorrplot 
conda install -c conda-forge r-corrplot 
conda install -c conda-forge r-ggeffects 
conda install -c conda-forge r-effects
conda install -c conda-forge r-ggdendro
conda install -c conda-forge r-rpart.plot
conda install -c conda-forge r-gridextra
conda install -c r r-magrittr 
conda install -c r r-rocr 
conda install -c conda-forge r-cowplot


  # ==============
  # Data Tools
  # ==============
#conda install -c r r-dplyr 
#conda install -c r r-stringr 
#conda install -c r r-tidyr 
#conda install -c r r-purrr
#conda install -c r r-reshape2


  # ==============
  # Data Sets (from books, web)
  # ==============
  # AmesHousing
  # AppliedPredictiveModeling
conda install -c r r-mass
conda install -c erblast r-islr 
#conda install -c jmorrison r-faraway  # downgrades rstudio and R version and ggplot





## Step 2: cloning environment so links break away
#conda create --name rdatasci_env --clone rdatasci_TEMP_env

## Step 3: clean up
conda clean -tipsy
