import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import to_rgba
from statsmodels.api import add_constant
from sklearn.preprocessing import PolynomialFeatures
import seaborn as sns

def plot_length_nbnodes(shoot, model, xlim=None, save = False, save_path = ""):
    # Creazione delle colonne polinomiali
    shoot['length_05'] = shoot['length'] ** 0.5
    shoot['length_2'] = shoot['length'] ** 2

    # Creazione del nuovo asse x per le previsioni
    if xlim is None:
        xlim = (0, shoot['length'].max())
    new_x = np.linspace(xlim[0], xlim[1], 100)

    # Creazione del DataFrame per le nuove previsioni
    new_data = pd.DataFrame({
        'length': new_x,
        'length_05': new_x ** 0.5,
        'length_2': new_x ** 2
    })

    new_data = sm.add_constant(new_data, has_constant='add')

    # Previsioni dal modello
    pred = model.get_prediction(new_data)
    summary_frame = pred.summary_frame(alpha=0.05)
    new_y = summary_frame['mean']
    upr = summary_frame['mean_ci_upper']
    lwr = summary_frame['mean_ci_lower']

    # Creazione del grafico
    plt.figure(figsize=(12, 9))
    plt.scatter(shoot['length'], shoot['node'], color='blue', label='Observed data')
    plt.plot(new_x, new_y, color='black', linewidth=2, label='Fitted line')
    plt.fill_between(new_x, lwr, upr, color='grey', alpha=0.5, label='95% Prediction interval')

    plt.xlabel('length(cm)')
    plt.ylabel('length(node)')
    plt.title('length(cm) vs length(node)')
    plt.legend()
    plt.show()
    if save:
        plt.savefig(f"{path_save}/1_lengthnodes.png", dpi=150)

def plot_blind_node_probability(metamer_proleptic, model, response_col = "b", predictor_col = "rank_node", save = False, path_save = ""):
    """
    This function plots the probability of having a sylleptic shoot on that rank node.

    Parameters:
    - metamer_proleptic: DataFrame containing the data.
    - model
    - response_col: Name of the response variable column.
    - predictor_col: Name of the predictor variable column.
    """
    # Relative frequency table of proportion of sylleptic related to normal_distance
    syl_norm = pd.crosstab(metamer_proleptic[response_col], metamer_proleptic[predictor_col])
    syl_norm_freq = syl_norm.div(syl_norm.sum(axis=1), axis=0)
    syl_norm_freq_matrix = syl_norm_freq.reset_index().rename_axis(None, axis=1)
    syl_norm_freq_matrix[predictor_col] = syl_norm_freq_matrix.index

    # Rename the first column to "Blind"
    syl_norm_freq_matrix.columns = ['Blind'] + list(syl_norm_freq_matrix.columns[1:])

    # Convert response variable to categorical
    metamer_proleptic[response_col] = metamer_proleptic[response_col].astype('category')

    # Plot the data
    plt.figure(figsize=(10, 7))
    plt.scatter(metamer_proleptic[predictor_col], metamer_proleptic[response_col].cat.codes, color='blue', alpha=0.5)
    plt.xlabel('Rank Node')
    plt.ylabel('Proportion of Blind Nodes')

    # Plot observed data
    plt.plot(syl_norm_freq_matrix[predictor_col], syl_norm_freq_matrix['Blind'], 'o', color='orange', label='observed')

    # Create new_x for predictions
    new_x = np.linspace(syl_norm_freq_matrix[predictor_col].min(), syl_norm_freq_matrix[predictor_col].max(), 100)
    new_y = model.predict(sm.add_constant(new_x))

    # Plot predicted data
    plt.plot(new_x, new_y, lw=2, color='black', label='predicted')

    # Confidence intervals
    pred = model.get_prediction(sm.add_constant(new_x))
    pred_summary = pred.summary_frame(alpha=0.05)
    upr = pred_summary['mean_ci_upper']
    lwr = pred_summary['mean_ci_lower']

    # Fill between for confidence interval
    plt.fill_between(new_x, lwr, upr, color=to_rgba('black', alpha=0.2))

    # Add legend
    plt.legend(loc='upper right')
    plt.show()

    if save:
        plt.savefig(f"{path_save}/2_proba_blind_node.png", dpi=150)

def plot_shoot_type(metamer_proleptic, model, response_col = "abs_norm_median_distance", predictor_col = "shoot_type", save=False, path_save=""):
    """
    This function plots the probability of having a sylleptic shoot on that rank node.

    Parameters:
    - metamer_proleptic: DataFrame containing the data.
    - response_col: Name of the response variable column.
    - predictor_col: Name of the predictor variable column.
    - model: The fitted GLM model.
    - save: Boolean to indicate if the plot should be saved.
    - path_save: Path where to save the plot if save is True.
    """
    # Relative frequency table of proportion of sylleptic related to normalized distance
    syl_norm = pd.crosstab(metamer_proleptic[predictor_col], metamer_proleptic[response_col])
    syl_norm_freq = syl_norm.div(syl_norm.sum(axis=1), axis=0)
    syl_norm_freq_matrix = syl_norm_freq.reset_index().rename_axis(None, axis=1)
    syl_norm_freq_matrix[predictor_col] = syl_norm_freq_matrix.index

    # Rename the appropriate column (assumes 'SYLLEPTIC' is a valid column name in your context)
    syl_norm_freq_matrix.columns = ['abs_norm_median_distance'] + list(syl_norm_freq_matrix.columns[1:])
    syl_norm_freq_matrix.columns = [response_col if col == response_col else col for col in
                                    syl_norm_freq_matrix.columns]

    # Convert response variable to categorical
    metamer_proleptic[response_col] = metamer_proleptic[response_col].astype('category')

    # Plot the data
    plt.figure(figsize=(10, 7))
    plt.scatter(metamer_proleptic[predictor_col], metamer_proleptic[response_col].cat.codes, color='blue', alpha=0.5)
    plt.xlabel('|Normalized Distance from Median Rank Node|')
    plt.ylabel('Proportion of Sylleptic Shoots')

    # Plot observed data
    plt.plot(syl_norm_freq_matrix[predictor_col], syl_norm_freq_matrix[response_col], 'o', color='orange',
             label='observed')

    # Create new_x for predictions
    new_x = np.linspace(syl_norm_freq_matrix[predictor_col].min(), syl_norm_freq_matrix[predictor_col].max(), 100)
    new_y = model.predict(sm.add_constant(new_x))

    # Plot predicted data
    plt.plot(new_x, new_y, lw=2, color='black', label='predicted')

    # Confidence intervals
    pred = model.get_prediction(sm.add_constant(new_x))
    pred_summary = pred.summary_frame(alpha=0.05)
    upr = pred_summary['mean_ci_upper']
    lwr = pred_summary['mean_ci_lower']

    # Fill between for confidence interval
    plt.fill_between(new_x, lwr, upr, color=to_rgba('black', alpha=0.2))

    # Add legend
    plt.legend(loc='upper right')
    plt.show()

    # Save the plot if needed
    if save:
        plt.savefig(f"{path_save}/3_proba_sylleptic.png", dpi=150)

def plot_mv_proportion(model, data_poly, predictor_col='rank_node', response_col='fate', save=False, path_save=""):
    """
    This function plots the bud fate proportion and predicted values from a multinomial model.

    Parameters:
    - model: The fitted multinomial logistic regression model.
    - data_poly: DataFrame used for fitting the model containing polynomial terms.
    - predictor_col: Name of the predictor variable column (default is 'rank_node').
    - response_col: Name of the response variable column (default is 'fate').
    """
    # Relative frequency table of M, V and B with rank node
    # Relative frequency table of M, V and B with rank node
    tab = pd.crosstab(data_poly[predictor_col], data_poly[response_col], normalize='index').values

    # Prepare data for prediction
    rank_node_vals = np.unique(np.sort(data_poly[predictor_col]))
    df = pd.DataFrame({
        predictor_col: rank_node_vals,
        f'{predictor_col}0.5': rank_node_vals ** 0.5,
        f'{predictor_col}2': rank_node_vals ** 2,
        f'{predictor_col}3': rank_node_vals ** 3,
        f'{predictor_col}4': rank_node_vals ** 4
    })

    # Predict probabilities
    pred_probs = model.predict(add_constant(df)).values
    pred = pd.DataFrame(pred_probs, columns=model.model.endog_names)
    pred[predictor_col] = df[predictor_col]

    # Define colors
    colors = sns.color_palette("Set1", n_colors=3)

    # Create bar plot
    fig, ax = plt.subplots(figsize=(12, 9))
    bar_width = 0.4
    r = np.arange(len(rank_node_vals))

    # Plot the relative frequencies
    for i in range(tab.shape[1]):
        ax.bar(r, tab[:, i], color=colors[i], width=bar_width, label=model.model.endog_names[i])

    # Plot the predicted values
    ax.plot(r, pred.iloc[:, 0], color='black', linewidth=2, label=f'Predicted {model.model.endog_names[0]}')
    ax.plot(r, pred.iloc[:, 1], color=colors[0], linewidth=2, linestyle='--',
            label=f'Predicted {model.model.endog_names[1]}')

    ax.set_xlabel('Rank Node')
    ax.set_ylabel('Bud Fate Proportion')
    ax.set_ylim(0, 1)
    ax.set_xticks(r)
    ax.set_xticklabels(rank_node_vals)

    # Add legend
    ax.legend(loc='upper right', title='Bud Fate')

    plt.show()
    # Save the plot if needed
    if save:
        plt.savefig(f"{path_save}/4_PRO_prop_MV.png", dpi=150)

def diameter_proba(shoot, xcol="length", ycol="diam"):
    # Prepara i dati
    xdata = shoot[xcol]
    ydata = shoot[ycol]
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


