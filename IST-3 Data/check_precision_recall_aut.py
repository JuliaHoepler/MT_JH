import pandas as pd
import random
import pyreadr
from synthcity.metrics.eval_statistical import AlphaPrecision
from synthcity.plugins.core.dataloader import GenericDataLoader

# Read .Rds file
result = pyreadr.read_r("Raw Data/data_small.Rds")

# Extract the DataFrame (usually only one object in the dict)
real = list(result.values())[0]

syn = pd.read_csv("Data/tabsyn/syn_data_small_tabsyn_1")

# OC

data_loader_real = GenericDataLoader(real)
data_loader_syn = GenericDataLoader(syn)

data_loader_real_encoded = data_loader_real.encode()[0]
data_loader_syn_encoded = data_loader_syn.encode()[0]

metric_pra = AlphaPrecision()

pra = metric_pra._evaluate(data_loader_real_encoded, data_loader_syn_encoded)
pra_oc = list(pra.values())[:3]

# for naive, add random target (there is a mistake in synthcity function _normalize_covariates wit target variable)
real['tar'] = random.choices([0, 1], k=len(real))
syn['tar'] = random.choices([0, 1], k=len(syn))

data_loader_real = GenericDataLoader(real)
data_loader_syn = GenericDataLoader(syn)

data_loader_real_encoded = data_loader_real.encode()[0]
data_loader_syn_encoded = data_loader_syn.encode()[0]

metric_pra = AlphaPrecision()

pra = metric_pra._evaluate(data_loader_real_encoded, data_loader_syn_encoded)
pra_naive = list(pra.values())[3:6]