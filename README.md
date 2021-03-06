# Parallel Gradient Descent

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Project proposal, report, and slides located in report directory  

Build: `stack build `   
Run: `stack run <FILEPATH> <linear/Linear/LINEAR/logistic/Logistic/LOGISTIC> <GUESS> <parallel/sequential> <number of chunks>`  
Example (Par): `stack run data/test-4.csv linear [0.0,0.0] parallel 256`

Test: `stack test`

Run with Eventlog: `stack exec parallel-gradient-descent-exe <FILEPATH> <linear/Linear/Linear/logistic/Logistic/LOGISTIC> <GUESS> --RTS -- +RTS -ls -N<NUMBER OF CORES>`  
Install Threadscope: `stack install threadscope`  
Run Threadscope: `threadscope parallel-gradient-descent-exe.eventlog`  

Generate data: `python data/datagen.py --functiontype <FUNCTION TYPE> --filename <FILEPATH> --slopes <SLOPES> --ints <INTERCEPTS> --nrows <NUMBER OF ROWS> --noisemag <NOISE MAGNITUDE>`  
Example data generation: `python data/datagen.py --functiontype linear --filename data/test.csv --slopes 1.0 2.0 --ints -5.0 4.0 --nrows 100 --noisemag 5.0`
