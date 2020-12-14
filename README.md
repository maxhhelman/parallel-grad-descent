# Parallel Gradient Descent

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Build (as of 12/13/20): `stack build `   
Run (as of 12/13/20): `stack run <FILEPATH> <line/linear/Line/Linear> <GUESS>` 

`---- e.g. stack run data/big-data-test.csv line [0.0,0.0] `

Generate data (as of 12/7/20): `python data/datagen.py --functiontype <FUNCTION TYPE> --filename <FILEPATH> --slopes <SLOPES> --ints <INTERCEPTS> --nrows <NUMBER OF ROWS> --noisemag <NOISE MAGNITUDE>`  
Example data generation (as of 12/7/20): `python data/datagen.py --functiontype linear --filename data/test.csv --slopes 1.0 2.0 --ints -5.0 4.0 --nrows 100 --noisemag 5.0`
