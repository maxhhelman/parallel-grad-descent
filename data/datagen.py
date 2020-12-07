import csv
import random

def datagen_linear(filename, slopes, nrows, noisemag):

    cols = ['y'] + ['x' + str(n) for n in range(1, len(slopes))]

    with open(filename, 'w') as file:
        writer = csv.writer(file)
        writer.writerow(cols)
        for i in range(nrows):
            row_temp = []
            for x in range(len(slopes)):
                row_temp.append((i * slopes[x]) + (((random.random() * 2) - 1) * noisemag))
            writer.writerow([z for z in row_temp])