#Script with the functions of finals GLMs used to create L-HAZELNUT model.
import pandas as pd
import statsmodels.api as sm
import numpy as np
from scipy.stats import norm
from sklearn.preprocessing import PolynomialFeatures

def shootnbnodesfromlength_proba(shoot):
    # Creazione delle colonne polinomiali
    shoot['length_05'] = shoot['length']**0.5
    shoot['length_2'] = shoot['length']**2

    # Definizione delle variabili indipendenti e dipendenti
    X = shoot[['length_05', 'length_2']]
    y = shoot['node']

    # Aggiunta dell'intercetta (costante) al modello
    X = sm.add_constant(X, has_constant='add')

    # Creazione e addestramento del modello di regressione lineare
    model = sm.OLS(y, X).fit()

    # Stampa del sommario del modello
    sum = model.summary()
    print(sum)

    # Calcolo dell'AIC del modello
    aic = model.aic
    print("AIC length_nbnodes:", aic)

    return model

def has_blind_node_proba(metamer_proleptic):
    # Define the dependent variable 'b' and the independent variable 'rank_node'
    y = metamer_proleptic['b']
    X = metamer_proleptic[['rank_node']]

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
    print("AIC prop_blind_node:", aic)

    return model

def has_sylleptic_proba(metamer_proleptic):
    # Define the dependent variable 'b' and the independent variable 'rank_node'
    y = metamer_proleptic['shoot_type']
    X = metamer_proleptic[['abs_norm_median_distance']]

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
    print("AIC PRO_shoot_type:", aic)

    return model

def bud_type_in_proleptic(bud_proleptic, response_col = "fate", predictor_col = "rank_node", ranklimit = 16, degree=4):
    """
    This function fits a multinomial logistic regression model with polynomial terms and prints the summary.

    Parameters:
    - bud_proleptic: DataFrame containing the data.
    - response_col: Name of the response variable column.
    - predictor_col: Name of the predictor variable column.
    - degree: Degree of the polynomial terms (default is 4).
    """
    data_poly = bud_proleptic[bud_proleptic[predictor_col] <= ranklimit].copy()

    # Generate polynomial features
    poly = PolynomialFeatures(degree)
    poly_features = poly.fit_transform(data_poly[[predictor_col]])

    poly_df = pd.DataFrame(poly_features, columns=poly.get_feature_names([predictor_col]))
    poly_df[f'{predictor_col}0.5'] = np.sqrt(data_poly[predictor_col])

    # Combine the original data with the new polynomial features
    data_poly = pd.concat([data_poly.reset_index(drop=True), poly_df], axis=1)

    # Fit the multinomial logistic regression model
    y = data_poly[response_col]
    X = data_poly.drop(columns=[response_col])

    # Adding a constant term
    X = sm.add_constant(X)

    model = sm.MNLogit(y, X).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate the AIC
    aic = model.aic
    print("AIC PRO_mv_proportion:", aic)

    # Compute the exponentiated coefficients
    exp_coef = np.exp(model.params)
    print("Exponentiated Coefficients:\n", exp_coef)

    # Compute p-values
    z = model.params / model.bse
    p_values = (1 - norm.cdf(np.abs(z))) * 2
    print("P-values:\n", p_values)

    return model, data_poly

def nb_mv_in_proleptic_lambda(met_proleptic, response_col = "tot_buds_mv"):
    """
    This function fits a Poisson regression model and prints the summary.

    Parameters:
    - met_proleptic: DataFrame containing the data.
    - response_col: Name of the response variable column.
    """
    # Filter the DataFrame
    df_filtered = met_proleptic[(met_proleptic["shoot_type"] == "PROLEPTIC") & (met_proleptic["b"] == 0)].copy()

    # Create the response variable
    df_filtered[response_col] = df_filtered['m'] + df_filtered['v']

    # Fit the Poisson regression model
    y = df_filtered[response_col]

    # If predictor_col is '1', it means we want to fit an intercept-only model
    if predictor_col == '1':
        X = np.ones(len(y))
    else:
        X = df_filtered[[predictor_col]]
        X = sm.add_constant(X)  # Add intercept

    model = sm.GLM(y, X, family=sm.families.Poisson()).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC PRO_mv_number:", aic)

    return model

def burst_in_proleptic_proba():
def length_new_in_proleptic_proba():
def have_clusters_proba():
def number_nuts_lambda():
def diameter_proba():
def length_new_juven_proba():

def SYL_mv_proportion(MV_bud_SYL, response_col = "fate"):
    """
    This function fits a Poisson regression model and prints the summary.

    Parameters:
    - MV_bud_SYL: DataFrame containing the data.
    - response_col: Name of the response variable column.
    """

    # Fit the Poisson regression model
    y = MV_bud_SYL[response_col]

    # If predictor_col is '1', it means we want to fit an intercept-only model
    if predictor_col == '1':
        X = np.ones(len(y))
    else:
        X = MV_bud_SYL[[predictor_col]]
        X = sm.add_constant(X)  # Add intercept

    model = sm.GLM(y, X, family=sm.families.Binomial()).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC SYL_mv_proportion:", aic)

    return model

def nb_mv_in_sylleptic_lambda():
def nb_v_in_sylleptic_lambda():
def burst_in_sylleptic_proba():
def length_new_in_sylleptic():




