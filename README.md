# Parallel Gradient Descent

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Build: `stack build `   
Run: `stack run <FILEPATH> <linear/Linear/Linear/logistic/Logistic/LOGISTIC> <GUESS> <parallel/sequential>`  
Example: `stack run data/big-data-test.csv linear [0.0,0.0] sequential`

Run with Eventlog: `stack exec parallel-gradient-descent-exe <FILEPATH> <linear/Linear/Linear/logistic/Logistic/LOGISTIC> <GUESS> --RTS -- +RTS -ls -N<NUMBER OF CORES>`  
Install Threadscope: `stack install threadscope`  
Run Threadscope: `threadscope parallel-gradient-descent-exe.eventlog`  

Generate data: `python data/datagen.py --functiontype <FUNCTION TYPE> --filename <FILEPATH> --slopes <SLOPES> --ints <INTERCEPTS> --nrows <NUMBER OF ROWS> --noisemag <NOISE MAGNITUDE>`  
Example data generation (as of 12/7/20): `python data/datagen.py --functiontype linear --filename data/test.csv --slopes 1.0 2.0 --ints -5.0 4.0 --nrows 100 --noisemag 5.0`
