##################################################
# CHARGER LES PACKAGES
# (Anton Perdoncin, EHESS, ERC Lubartworld)
##################################################


library(tidyverse)

library(curl)

library(purrr)

library(questionr)

library(FactoMineR) 

library(RColorBrewer) 

library(ggrepel) 

library(explor) 

library(flextable)

# Options

options(scipen=999) # pour desactiver l'ecriture scientifique des nombres

set_flextable_defaults(decimal.mark = ",", big.mark = "") # pour changer séparateur de décimales et de milliers par défaut dans les tableaux flextable
