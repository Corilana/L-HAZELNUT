'''
Exploratory analysis of annual shoot
- AIM1: freq dist per each rank --> nb of shoots
- AIM2: relationship length (node) ~ length (cm)
- AIM3: relationship length(cm) ~ diameter (mm)
'''

import pandas as pd
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
import numpy as np

def compute_freq_rank(shoot_df: pd.DataFrame, node_col = str):
    max_node = shoot_df[node_col].max()
    freq_rank = pd.DataFrame({'nb_shoots': [shoot_df[shoot_df[node_col] == i][node_col].count() for i in range(1, max_node + 1)]})
    return freq_rank

def length_distribution(df: pd.DataFrame, max_nodes = int, title: str = 'Length distribution of annual shoots',
                       xlabel: str = 'Length (nodes)', ylabel: str = 'Nb of shoots'):
    plt.bar(df.index, df['nb_shoots'])
    plt.title(title)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.ylim(0, max_nodes)
    return plt

def length_nbnodes(df: pd.DataFrame, class_column: str, x_column: str,
                   y_column: str, size: int,
                   title: str = 'Relationship #node/length',
                   xlabel: str = 'Parent length (cm)', ylabel: str = 'Parent #nodes'):
    for class_level, marker in zip(df['class'].unique(), ['o', '^', 's', 'p']):
        subset = df[df[class_column] == class_level]
        plt.scatter(subset[x_column], subset[y_column], marker=marker, label=class_level, s=size)
    plt.title(title)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.legend(title='Class')
    return plt


def model_func(x, a, b):
    return a * (x ** b)


def len_diamet(data, xcol="length", ycol="diam"):
    # Prepara i dati
    xdata = data[xcol]
    ydata = data[ycol]

    # Esegui il fitting del modello ai dati
    # `popt` conterrà i parametri ottimizzati (a e b in questo caso)
    popt, pcov = curve_fit(model_func, xdata, ydata, p0=[1, 1])

    # Usa i parametri ottimizzati per predire i valori di y
    xrange = np.linspace(0, 75, 100)
    y_pred = model_func(xrange, *popt)

    # plot data and model
    plt.scatter(xdata, ydata, label='Data')
    plt.plot(xrange, y_pred, color='red', linewidth=2, label='Fitted model')
    plt.xlabel(xcol)
    plt.ylabel(ycol)
    plt.legend()
    plt.show()

    print(f"Parameters: a={popt[0]:.2f}, b={popt[1]:.2f}")
