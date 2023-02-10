![License](https://img.shields.io/static/v1?label=license&message=CC-BY-NC-4.0&color=green)

# Machine Learning & Pediatric Appendicitis

This repository holds the code for the analysis of the data from pediatric patients with suspected appendictis. Machine learning (ML) models are trained and validated to perform the prediction of the diagnosis, management, and severity. The resulting models were deployed as a research prototype of the [Pediatric Appendicitis Prediction Tool](https://papt.inf.ethz.ch/). Further details can be found in the [*Frontiers in Pediatrics* paper](https://www.frontiersin.org/articles/10.3389/fped.2021.662183/full). Follow-up work on using supersparse linear integer models for interpretable classification was published in [2021 20<sup>th</sup> IEEE International Conference on Machine Learning and Applications](https://ieeexplore.ieee.org/abstract/document/9680150).

This reserach was carried out by the [Medical Data Science research group](https://mds.inf.ethz.ch/), ETH Zurich, Switzerland, in collaboration with the [University Children's Hospital Regensburg (KUNO)](https://www.barmherzige-hedwig.de/kinderchirurgie-und-kinderorthopaedie/ueber-uns.html), Germany, Research and Development Campus Regensburg (WECARE), Germany, and Department of Pediatric Surgery and Pediatric Orthopedics, Hospital St. Hedwig of the Order of St. John, Regensburg, Germany.

### Requirements

To run this code, you will need
- [R](https://www.r-project.org/), version ≥ 3.6.2
- [RStudio](https://rstudio.com/), version ≥ 1.1.456

All the necessary packages can be installed by running `setup_script.R`.


### Data

The data are provided in CSV and RDA fromats in `app_data.csv` and `app_data_clean.Rda` files, respectively.

### Usage

- `setup_script.R` sets up necessary packages and utility functions
- `eda.R` performs some basic exploratory analysis on the dataset
- `diagnosis.R`, `management.R`, and `severity.R` validate logistic regression, random forest, and gradient boosting models for predicting diagnosis, management, and severity, respectively
- `variable_selection.R` performs variable selection based on the random forest variable importance

### Maintainer 

This repository is maintained by Ričards Marcinkevičs ([ricards.marcinkevics@inf.ethz.ch](mailto:ricards.marcinkevics@inf.ethz.ch)).

### Citation

If you use the dataset, please cite papers below:
```
@article{MarcinkevicsWolfertstetter2021,
  title={Using Machine Learning to Predict the Diagnosis, Management and Severity of Pediatric Appendicitis},
  author={Marcinkevics, Ricards and Reis Wolfertstetter, Patricia and Wellmann, Sven and Knorr, Christian and Vogt, Julia E},
  journal={Frontiers in Pediatrics},
  volume={9},
  pages={360},
  year={2021},
  publisher={Frontiers}
}

@inproceedings{RoigAparicio2021,
  title = {Learning Medical Risk Scores for Pediatric Appendicitis},
  author = {Pedro Roig Aparicio and Ricards Marcinkevics and Patricia Reis Wolfertstetter and Sven Wellmann and Christian Knorr and Julia E. Vogt},
  booktitle = {2021 20th {IEEE} International Conference on Machine Learning and Applications ({ICMLA})}
  year = {2021},
  publisher = {{IEEE}}
  
}
```

### Copyright

This repository is copyright (c) 2021 Marcinkevics R, Reis Wolfertstetter P, Wellmann S, Knorr C and Vogt JE.

This repository is additionally licensed under CC-BY-NC-4.0.
