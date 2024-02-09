tt <- read.table(file = "/Users/dominikzabinski/Downloads/cross-lingual.tsv", sep = "\t")
tail(tt)
tt <- read.csv(file = "/Users/dominikzabinski/Downloads/supervised-individual.csv")
tail(tt)
length(unique(tt$phrase))
head(tt)
tt <- read.csv(file = "/Users/dominikzabinski/Downloads/supervised-lexical.csv")
head(tt)
oo <- read.csv("/Users/dominikzabinski/Downloads/plwordnet_4_2/słownik_anotacji_emocjonlanej.csv")
head(oo)
kk <- list.files("/Users/dominikzabinski/Downloads/plwordnet_4_2/", full.names = T)
kk
# https://zil.ipipan.waw.pl/SlownikWydzwieku
oo <- read.csv("/Users/dominikzabinski/Downloads/slownikWydzwieku01.csv", sep = "\t")
nrow(oo)
head(oo)
# Tokenizer
# https://github.com/kwrobel-nlp/krnnt
# metoda z curl
system(command = 'curl -X POST "https://krnnt-f3esrhez2q-ew.a.run.app/?output_format=conll" -d "Ala ma kota."')
# https://hub.docker.com/r/djstrong/krnnt/ - to na macu nie idzie z powodu problemów jakie tensorflow ma z procesorem m1/m2/m2 pro