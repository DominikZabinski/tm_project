# Overall

This is the repository of Text Mining projects based on [The Polish Parliamentary Corpus / Korpus Dyskursu Parlamentarnego](https://clip.ipipan.waw.pl/PPC) data.

In order to execute all off this one has to:

- download the data from [this link](https://manage.legis.nlp.ipipan.waw.pl/download/ppc-anno.tar.gz) and extract it to this repository directory
- setup the database with scripts/00_database.R
  - this will create project_database.sqlite file. that will be used to store data from .xml
- set up [KRNNT](https://github.com/kwrobel-nlp/krnnt/tree/master) instance with docker run -p 9003:9003 -it djstrong/krnnt:1.0.0
  - it will create a container with morphological tagger for Polish running locally (online version is available [here](https://krnnt-f3esrhez2q-ew.a.run.app/))
  - you have to have docker installed on your machine
  - unfortunately it does not work on Macs with M1/M2 processors

# Repository structure

- ppc-nanno/ - downloaded from [this link](https://manage.legis.nlp.ipipan.waw.pl/download/ppc-anno.tar.gz)
