# CHOIR Symptom Cluster Classification Tool

This app implements a classification system of chronic pain severity using PROMIS-based multidimensional pain-agnostic symptom assessments. The output provides a label (the number 1, 2, or 3) indicating a general graded scale of severity such that label 1 reflects the least severe condition, label 2 reflects medium severity, and label 3 reflects the worst severity. The labels for these three different clusters were developed in a sample of patients with chronic pain, indicating diagnostic- and prognostic-like properties. For more information on the development and validation of this classification system, please see the referenced paper. Please see the instructions for guidance on usability.

Citation:
> Classifying chronic pain using multidimensional pain-agnostic symptom assessments and clustering analysis  Gadi Gilam, Eric M. Cramer, Kenneth A. Webber II, Maisa S. Ziadni, Ming-Chih Kao, Sean C. Mackey  medRxiv 2021.04.21.21255885; doi: https://doi.org/10.1101/2021.04.21.21255885"

To build from scratch:
```
docker build --no-cache -t symptom-cluster-tool .
```

To run:
```
docker run --rm -p 3838:3838 -v $PWD/srv/shinyapps/:/srv/shiny-server/ -v $PWD/srv/shinylog/:/var/log/shiny-server/ -e APPLICATION_LOGS_TO_STDOUT=true symptom-cluster-tool
```
