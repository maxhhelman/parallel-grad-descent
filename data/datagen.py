'''
Names: Max Helman, Riya Chakraborty
UNI: mhh2148, rc3242
'''

import csv
import random
import argparse

def datagen_linear(filename, slopes, ints, nrows, noisemag):
    """
    Generates linear data and writes it to a csv file
    :param filename: Name of file to write to.
    :param slopes: List of actual slopes of columns in generated data.
    :param ints: List of actual intercepts of columns in generated data.
    :param nrows: Number of rows of data to write.
    :param noisemag: Magnitude of noise to add to data.
    """
    # Name the columns
    cols = ['y'] + ['x' + str(n) for n in range(1, len(slopes))]
    
    # Write to the file
    with open(filename, 'w') as file:
        writer = csv.writer(file)
        writer.writerow(cols)
        for i in range(nrows):
            row_temp = []
            for x in range(len(slopes)):
                row_temp.append((i * slopes[x]) + (((random.random() * 2) - 1) * noisemag) + ints[x])
            writer.writerow([z for z in row_temp])

if __name__ == "__main__":

    # Read in arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('--functiontype', required=True, choices=['linear'])
    parser.add_argument('--filename', default='temp-data.csv')
    parser.add_argument('--slopes', required=True, type=float, nargs = '+')
    parser.add_argument('--ints', required=True, type=float, nargs = '+')
    parser.add_argument('--nrows', type=int, default=1000)
    parser.add_argument('--noisemag', type=float, default=0.0)
    args = parser.parse_args()
    
    # Linear function
    if args.functiontype == 'linear':
        datagen_linear(args.filename, args.slopes, args.ints, args.nrows, args.noisemag)