# Script with the functions of finals GLMs used to create L-HAZELNUT model.
import pandas as pd
import statsmodels.api as sm
import numpy as np
from scipy.stats import norm
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import mean_squared_error
from scipy.stats import gamma
from scipy.optimize import minimize, curve_fit
from scipy.special import gammaln
import statsmodels.formula.api as smf


def shootnbnodesfromlength_proba(shoot, length = "length", nb_nodes = "node"):
    # Creazione delle colonne polinomiali
    shoot['length_05'] = shoot[length] ** 0.5
    shoot['length_2'] = shoot[length] ** 2

    # Definizione delle variabili indipendenti e dipendenti
    X = shoot[['length_05', 'length_2']]
    y = shoot[nb_nodes]

    # Creazione e addestramento del modello di regressione lineare
    model = sm.OLS(y, X).fit()

    # Stampa del sommario del modello
    sum = model.summary()
    print(sum)

    # Calcolo del Residual Standard Error (RSE)
    residuals = model.resid
    degrees_of_freedom = len(y) - len(model.params)
    rse = np.sqrt(np.sum(residuals**2) / degrees_of_freedom)
    print("Residual Standard Error (RSE):", rse)

    # Calcolo dell'AIC del modello
    aic = model.aic
    print("AIC shootnbnodesfromlength_proba:", aic)

    return model


def has_blind_node_proba(metamer_proleptic, b = "b", rank_node = "rank_node"):
    # Define the dependent variable 'b' and the independent variable 'rank_node'
    y = metamer_proleptic[b]
    X = metamer_proleptic[[rank_node]]

    # Add a constant term to the independent variables
    # In this case, the R formula 'b ~ rank_node + 0' suggests no intercept, so we skip adding a constant
    # X = sm.add_constant(X)

    # Fit the GLM model with binomial family
    model = sm.GLM(y, X, family=sm.families.Binomial()).fit()

    # Print the summary of the model
    sum = model.summary()
    print(sum)
    # Calcolo dell'AIC del modello
    aic = model.aic
    print("AIC has_blind_node_proba:", aic)

    return model

def has_sylleptic_proba(metamer_proleptic, shoot_type = "shoot_type", abs_norm_median_distance = "abs_norm_median_distance"):
    metamer_proleptic = metamer_proleptic.dropna(subset=[shoot_type, abs_norm_median_distance])

    # Converti 'shoot_type' in numerico, deve essere binario (0 o 1)
    metamer_proleptic['shoot_type_binary'] = pd.get_dummies(metamer_proleptic[shoot_type], drop_first=True)
    metamer_proleptic['abs_norm_median_distance'] = pd.to_numeric(metamer_proleptic[abs_norm_median_distance],errors='coerce')

    # Definisci la variabile dipendente e indipendente
    y = metamer_proleptic['shoot_type_binary']
    X = metamer_proleptic[['abs_norm_median_distance']]

    # Fit the GLM model with binomial family
    model = sm.GLM(y, X, family=sm.families.Binomial()).fit()

    # Print the summary of the model
    sum = model.summary()
    print(sum)
    # Calcolo dell'AIC del modello
    aic = model.aic
    print("AIC has_sylleptic_proba:", aic)

    return model


def nb_mv_in_sylleptic_lambda(met_sylleptic, parent_length_cm = "parent_length_cm",
                              abs_norm_median_distance = "abs_norm_median_distance", tot_buds_syl_m_v ="tot_buds_syl_m_v"):
    # Fit the binomial logistic regression model
    X = met_sylleptic[[parent_length_cm, abs_norm_median_distance]]
    y = met_sylleptic[tot_buds_syl_m_v]

    X = sm.add_constant(X)

    model = sm.GLM(y, X, family=sm.families.Poisson()).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC: nb_mv_in_sylleptic_lambda", aic)


def nb_v_in_sylleptic_lambda(MV_bud_SYL, fate = "fate"):
    """
    This function fits a Poisson regression model and prints the summary.

    Parameters:
    - MV_bud_SYL: DataFrame containing the data.
    """

    # Fit the Poisson regression model
    MV_bud_SYL["fate_binary"] = pd.get_dummies(MV_bud_SYL[fate], drop_first=True)
    y = MV_bud_SYL["fate_binary"]
    # Define the independent variable as a column of ones (intercept only model)
    X = np.ones(len(MV_bud_SYL))
    model = sm.GLM(y, X, family=sm.families.Binomial()).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC SYL_mv_proportion:", aic)

    return model


def burst_in_sylleptic_proba(MV_bud_SYL, m="m", fate="fate", v="v"):
    """
    This function fits a logistic regression model with interaction terms and prints the summary.

    Parameters:
    - MV_bud_SYL: DataFrame containing the data.
    - predictor1_col: Name of the first predictor variable column.
    - predictor2_col: Name of the second predictor variable column.
    - interaction_col: Name of the interaction variable column.
    """
    # Fit the logistic regression model with interaction terms
    formula = f'nb_new_shoots ~ {m}:{fate} + {v}:{fate}'
    model = smf.logit(formula, data=MV_bud_SYL).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC: burst_in_sylleptic_proba", aic)


def gamma_log_likelihood(params, data):
    shape, rate = params
    n = len(data)
    log_likelihood = np.sum((shape - 1) * np.log(data) - data / rate) - n * (shape * np.log(rate) + gammaln(shape))
    return -log_likelihood


def fit_gamma_distribution(data):
    # Initial guess for shape and rate parameters
    shape_init = 1.0
    rate_init = 1.0

    # Minimize the negative log-likelihood
    result = minimize(gamma_log_likelihood, [shape_init, rate_init], args=(data,), bounds=[(0, None), (0, None)])
    shape, rate = result.x
    return shape, rate


def calculate_aic(log_likelihood, num_params):
    aic = 2 * num_params - 2 * log_likelihood
    return aic


def length_new_in_sylleptic(MV_bud_SYL, length2yo = "length2yo"):
    """
    This function fits a gamma distribution to the response variable and calculates the AIC.
    Parameters:
    - df: DataFrame containing the data.
    - response_col: Name of the response variable column.
    """
    # Filter the DataFrame to exclude NA values in the response column
    df_filtered = MV_bud_SYL.dropna(subset=[length2yo])

    # Fit the gamma distribution to the data
    data = df_filtered[length2yo].values
    shape, rate = fit_gamma_distribution(data)

    # Calculate the log-likelihood of the fitted gamma distribution
    log_likelihood = -gamma_log_likelihood((shape, rate), data)

    # Calculate AIC
    num_params = 2  # shape and rate
    aic = calculate_aic(log_likelihood, num_params)
    print("AIC: length_new_in_sylleptic", aic)

    # Create a DataFrame with gamma distribution density values
    x_vals = np.arange(0, max(data), 0.1)
    y_vals = gamma.pdf(x_vals, a=shape, scale=1 / rate)
    density_df = pd.DataFrame({'x': x_vals, 'y': y_vals})

    return shape, rate, aic, density_df


def nb_mv_in_proleptic_lambda(met_proleptic, response_col="tot_buds_mv", shoot_type = "shoot_type",
                              PROLEPTIC = "PROLEPTIC", b = "b", m = "m", v = "v"):
    """
    This function fits a Poisson regression model and prints the summary.

    Parameters:
    - met_proleptic: DataFrame containing the data.
    - response_col: Name of the response variable column.
    """
    # Filter the DataFrame
    df_filtered = met_proleptic[(met_proleptic[shoot_type] == PROLEPTIC) & (met_proleptic[b] == 0)].copy()

    # Create the response variable
    df_filtered[response_col] = df_filtered[m] + df_filtered[v]

    # Fit the Poisson regression model
    y = df_filtered[response_col]

    # If predictor_col is '1', it means we want to fit an intercept-only model
    X = np.ones(len(y))

    model = sm.GLM(y, X, family=sm.families.Poisson()).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC nb_mv_in_proleptic_lambda:", aic)

    return model
def bud_type_in_proleptic(bud_proleptic, rank_node = "rank_node", fate = "fate", b = "B", m = "M", v = "V"):
    """
    This function fits a multinomial logistic regression model with polynomial terms and prints the summary.

    Parameters:
    - bud_proleptic: DataFrame containing the data.
    """
    data_poly = bud_proleptic[bud_proleptic[rank_node] <= 16].copy()
    data_poly = data_poly[data_poly[fate] != b].copy()
    data_poly['rank_node0_5'] = data_poly[rank_node] ** 0.5
    data_poly['rank_node2'] = data_poly[rank_node] ** 2
    data_poly['rank_node3'] = data_poly[rank_node] ** 3
    data_poly['rank_node4'] = data_poly[rank_node] ** 4

    # Encode the categorical variable 'fate'
    data_poly[fate] = data_poly[fate].map({m: v, v: m})
    # Encode the categorical variable 'fate'
    label_encoder = LabelEncoder()
    data_poly['fate_encoded'] = label_encoder.fit_transform(data_poly[fate])
    # Print the mapping of categories to integers
    print("Category mapping:")
    for i, class_label in enumerate(label_encoder.classes_):
        print(f"{class_label}: {i}")

    X = data_poly[['rank_node', 'rank_node0_5', 'rank_node2', 'rank_node3', 'rank_node4']]
    X = sm.add_constant(X)  # Adds a constant term to the model

    y = data_poly['fate_encoded']

    # Fit the multinomial logistic regression model
    model = sm.MNLogit(y, X)
    result = model.fit()
    print(result.summary())
    # Calcola l'AIC
    AIC = result.aic

    # Calcola la residual deviance manualmente
    residual_deviance = -2 * result.llf

    # Stampare il risultato
    print("Residual Deviance:", residual_deviance)
    # Stampare i risultati
    print("AIC:", AIC)

    return model


def burst_in_proleptic_proba(MV_bud_PRO, siblings_mv="siblings_mv", fate="fate",
                             norm_median_distance="norm_median_distance"):
    """
    This function fits a logistic regression model with interaction terms and prints the summary.

    Parameters:
    - MV_bud_PRO: DataFrame containing the data.
    - predictor1_col: Name of the first predictor variable column.
    - predictor2_col: Name of the second predictor variable column.
    - interaction_col: Name of the interaction variable column.
    """
    # Rename column 24 to "presence_new_shoot" if column 24 exists
    if MV_bud_PRO.shape[1] >= 24:
        MV_bud_PRO.columns.values[23] = "presence_new_shoot"  # Column index in pandas is 0-based

    # Fit the logistic regression model with interaction terms
    formula = f'presence_new_shoot ~ {siblings_mv}:{fate} + {norm_median_distance}:{fate}'
    model = smf.logit(formula, data=MV_bud_PRO).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC: burst_in_proleptic_proba", aic)

    return model


def length_new_in_proleptic_proba(MV_bud_PRO, length2yo = "length2yo", fate = "fate",
                                  m = "M", length = "length", norm_median_distance = "norm_median_distance"):
    """
    This function fits a linear regression model with interaction terms and prints the summary.

    Parameters:
    - MV_bud_PRO: DataFrame containing the data.
    """
    # Filter out rows with NA values in response_col
    df_filtered = MV_bud_PRO.dropna(subset=[length2yo])

    # Remove outliers where fate is "M" and length2yo > 20
    df_filtered = df_filtered[~((df_filtered[fate] == m) & (df_filtered[length2yo] > 20))]

    # Fit the linear regression model with interaction terms
    formula = f'{length2yo} ~ {fate}:{length} + {norm_median_distance}:{fate}'
    model = smf.ols(formula, data=df_filtered).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC length_new_in_proleptic_proba:", aic)

    return


def have_clusters_proba(bud, fate = "fate", m = "M", cl = "cl", length = "length",
                        siblings_mv = "siblings_mv",abs_norm_median_distance= "abs_norm_median_distance"):
    """
    This function fits a binomial logistic regression model to predict the probability of having clusters.

    Parameters:
    - bud: DataFrame containing the data.
    """
    # Filter the DataFrame for rows where fate is equal to fate_value
    df_filtered = bud[bud[fate] == m].copy()

    # Adjust the cluster column
    df_filtered.loc[df_filtered[cl] > 1, cl] = 1

    # Calculate cluster_set and nut_set
    cluster_set = df_filtered[cl].sum() / len(df_filtered)

    print(f'Cluster set proportion: {cluster_set}')

    # Rename the specified column
    if len(df_filtered.columns) > 8:
        df_filtered.columns.values[8] = "Length_node"

    # Fit the binomial logistic regression model
    X = df_filtered[[length, siblings_mv, abs_norm_median_distance]]
    y = df_filtered[cl]

    model = sm.GLM(y, X, family=sm.families.Binomial()).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC: have_clusters_proba", aic)

    return model


def number_nuts_lambda(bud, fate = "fate", m = "M", cl = "cl", nu = "nu",Length_node = "Length_node",
                    median_distance= "median_distance"):
    """
    This function fits a binomial logistic regression model to predict the probability of having clusters.

    Parameters:
    - bud: DataFrame containing the data.
    """
    # Filter the DataFrame for rows where fate is equal to fate_value
    df_filtered = bud[bud[fate] == m].copy()
    df_filtered = df_filtered[df_filtered[cl] > 0].copy()
    df_filtered.columns = df_filtered.columns.str.strip()  # Rimuovi eventuali spazi bianchi prima e dopo i nomi delle colonne

    nut_set = df_filtered[nu].sum() / len(df_filtered)

    print(f'Nut set proportion: {nut_set}')

    df_filtered["norm_median_distance"] = df_filtered[median_distance] / df_filtered[Length_node]

    # Fit the binomial logistic regression model
    X = df_filtered["norm_median_distance"]
    y = df_filtered[nu]

    X = sm.add_constant(X)

    model = sm.GLM(y, X, family=sm.families.Poisson()).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC: number_nuts_lambda", aic)

    return model

def model_func(x, a, b):
    return a * (x ** b)

def diameter_proba(shoot, length = "length", diam = "diam"):
    # Prepara i dati
    xdata = shoot[length]
    ydata = shoot[diam]

    # Esegui il fitting del modello ai dati
    # `popt` conterrà i parametri ottimizzati (a e b in questo caso)
    popt, pcov = curve_fit(model_func, xdata, ydata, p0=[1, 1])

    print(f"Parameters: a={popt[0]/10:2f}, b={popt[1]:.2f}")

    y_pred = model_func(xdata, *popt)

    # Calcola l'errore quadratico medio (RMSE)
    rmse = np.sqrt(mean_squared_error(ydata, y_pred))
    print(f"RMSE: {rmse/10:.2f}")


