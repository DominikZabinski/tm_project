# Overview

This is the repository of Text Mining projects based on [The Polish Parliamentary Corpus / Korpus Dyskursu Parlamentarnego](https://clip.ipipan.waw.pl/PPC) data.

# Step 1: Processing raw files

To process raw files:

- download data from [this link](https://manage.legis.nlp.ipipan.waw.pl/download/ppc-anno.tar.gz) and extract it to this repository directory
- process .xml files with `scripts/00_database.R`
  - this will create `data` directory and `<type>_<period>.sqlite` files. that will be used to store data from .xml files from `ppc-nanno`
  - the data will be stored on Google Drive

# Step 2: Tagging

Since changing word to its base form in Polish language is difficult and context dependent the external tool will be used

- set up [KRNNT](https://github.com/kwrobel-nlp/krnnt/tree/master) instance with 
```bash
docker run -p 9003:9003 -it djstrong/krnnt:1.0.0
```
  - it will create a container with morphological tagger for Polish running locally (online version is available [here](https://krnnt-f3esrhez2q-ew.a.run.app/))
  - one has to have Docker installed on the machine
  - unfortunately it does not work on Macs with M1/M2 processors
- set up the database with tagged word using `01_tagger.R`
  - this will create the `tagged_<type>_<period>.sqlite` files in `data` directory with each of the documents from `data/<type>_<period>.sqlite` tagged

# Step 3: Download processed data

All the processed data is stored in Google Drive. To make sure one has all necessary files, one has to download them on local machine. To do this run `scripts/02_processed_data_download.R`

# Repository structure

- `renv.*` - directories and files needed for enviroment control via `{renv}` package
- `scripts/` - directory with all needed scripts
- `ppc-nanno/` - downloaded from [this link](https://manage.legis.nlp.ipipan.waw.pl/download/ppc-anno.tar.gz)
- `data/` - processed data in form of .sqlite files
- `dicts/` - files with dictionaries:
  - `słownik_anotacji_emocjonlanej.csv` (downloaded from [PLWORDNET](http://plwordnet.pwr.wroc.pl/wordnet/download))
