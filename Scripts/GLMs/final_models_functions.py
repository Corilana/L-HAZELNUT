# Script with the functions of finals GLMs used to create L-HAZELNUT model.
import pandas as pd
import statsmodels.api as sm
import numpy as np
from scipy.stats import norm
from sklearn.preprocessing import PolynomialFeatures
from scipy.stats import gamma
from scipy.optimize import minimize
from scipy.special import gammaln


def shootnbnodesfromlength_proba(shoot):
    # Creazione delle colonne polinomiali
    shoot['length_05'] = shoot['length'] ** 0.5
    shoot['length_2'] = shoot['length'] ** 2

    # Definizione delle variabili indipendenti e dipendenti
    X = shoot[['length_05', 'length_2']]
    y = shoot['node']

    # Aggiunta dell'intercetta (costante) al modello
    X = sm.add_constant(X)

    # Creazione e addestramento del modello di regressione lineare
    model = sm.OLS(y, X).fit()

    # Stampa del sommario del modello
    sum = model.summary()
    print(sum)

    # Calcolo dell'AIC del modello
    aic = model.aic
    print("AIC shootnbnodesfromlength_proba:", aic)

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
    print("AIC has_blind_node_proba:", aic)

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
    print("AIC has_sylleptic_proba:", aic)

    return model


def bud_type_in_proleptic(bud_proleptic, response_col="fate", predictor_col="rank_node", ranklimit=16, degree=4):
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
    print("AIC bud_type_in_proleptic:", aic)

    # Compute the exponentiated coefficients
    exp_coef = np.exp(model.params)
    print("Exponentiated Coefficients:\n", exp_coef)

    # Compute p-values
    z = model.params / model.bse
    p_values = (1 - norm.cdf(np.abs(z))) * 2
    print("P-values:\n", p_values)

    return model, data_poly


def nb_mv_in_proleptic_lambda(met_proleptic, response_col="tot_buds_mv"):
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
    print("AIC nb_mv_in_proleptic_lambda:", aic)

    return model


def nb_v_in_sylleptic_lambda(MV_bud_SYL, response_col="fate"):
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


def burst_in_proleptic_proba(MV_bud_PRO, predictor1_col="siblings_mv", interaction_col="fate",
                             predictor2_col="norm_median_distance"):
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
    formula = f'presence_new_shoot ~ {predictor1_col}:{interaction_col} + {predictor2_col}:{interaction_col}'
    model = smf.logit(formula, data=MV_bud_PRO).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC: burst_in_proleptic_proba", aic)

    return model


def length_new_in_proleptic_proba(MV_bud_PRO):
    """
    This function fits a linear regression model with interaction terms and prints the summary.

    Parameters:
    - MV_bud_PRO: DataFrame containing the data.
    """
    # Filter out rows with NA values in response_col
    df_filtered = MV_bud_PRO.dropna(subset=["length2yo"])

    # Remove outliers where fate is "M" and length2yo > 20
    df_filtered = df_filtered[~((df_filtered["fate"] == "M") & (df_filtered["length2yo"] > 20))]

    # Fit the linear regression model with interaction terms
    formula = f'length2yo ~ fate:length + norm_median_distance:fate'
    model = smf.ols(formula, data=df_filtered).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC length_new_in_proleptic_proba:", aic)

    return


def burst_in_sylleptic_proba(MV_bud_SYL, predictor1_col="m", interaction_col="fate", predictor2_col="v"):
    """
    This function fits a logistic regression model with interaction terms and prints the summary.

    Parameters:
    - MV_bud_SYL: DataFrame containing the data.
    - predictor1_col: Name of the first predictor variable column.
    - predictor2_col: Name of the second predictor variable column.
    - interaction_col: Name of the interaction variable column.
    """
    # Fit the logistic regression model with interaction terms
    formula = f'nb_new_shoots ~ {predictor1_col}:{interaction_col} + {predictor2_col}:{interaction_col}'
    model = smf.logit(formula, data=MV_bud_SYL, family=sm.families.Binomial()).fit()

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


def length_new_in_sylleptic(df):
    """
    This function fits a gamma distribution to the response variable and calculates the AIC.
    Parameters:
    - df: DataFrame containing the data.
    - response_col: Name of the response variable column.
    """
    # Filter the DataFrame to exclude NA values in the response column
    df_filtered = df.dropna(subset=["length2yo"])

    # Fit the gamma distribution to the data
    data = df_filtered[response_col].values
    shape, rate = fit_gamma_distribution(data)

    # Calculate the log-likelihood of the fitted gamma distribution
    log_likelihood = -gamma_log_likelihood((shape, rate), data)

    # Calculate AIC
    num_params = 2  # shape and rate
    n = len(data)
    aic = calculate_aic(log_likelihood, num_params, n)
    print("AIC: length_new_in_sylleptic", aic)

    # Create a DataFrame with gamma distribution density values
    x_vals = np.arange(0, max(data), 0.1)
    y_vals = gamma.pdf(x_vals, a=shape, scale=1 / rate)
    density_df = pd.DataFrame({'x': x_vals, 'y': y_vals})

    return shape, rate, aic, density_df


def have_clusters_proba(bud):
    """
    This function fits a binomial logistic regression model to predict the probability of having clusters.

    Parameters:
    - bud: DataFrame containing the data.
    """
    # Filter the DataFrame for rows where fate is equal to fate_value
    df_filtered = bud[bud["fate"] == "M"].copy()

    # Adjust the cluster column
    df_filtered.loc[df_filtered["cl"] > 1, "cl"] = 1

    # Calculate cluster_set and nut_set
    cluster_set = df_filtered["cl"].sum() / len(df_filtered)

    print(f'Cluster set proportion: {cluster_set}')

    # Rename the specified column
    if len(df_filtered.columns) > 10:
        df_filtered.columns.values[10] = "Length_node"

    # Fit the binomial logistic regression model
    X = df_filtered["length", "siblings_mv", "abs_norm_median_distance"]
    y = df_filtered["cl"]

    # Add a constant term to the predictors
    X = sm.add_constant(X, has_constant='add')

    model = sm.GLM(y, X, family=sm.families.Binomial()).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC: have_clusters_proba", aic)

    return model


def number_nuts_lambda(bud):
    """
    This function fits a binomial logistic regression model to predict the probability of having clusters.

    Parameters:
    - bud: DataFrame containing the data.
    """
    # Filter the DataFrame for rows where fate is equal to fate_value
    df_filtered = bud[bud["fate"] == "M"].copy()

    nut_set = df_filtered["nu"].sum() / len(df_filtered)

    print(f'Nut set proportion: {nut_set}')

    # Adjust the cluster column
    df_filtered.loc[df_filtered["cl"] > 1, "cl"] = 1
    # Rename the specified column
    if len(df_filtered.columns) > 10:
        df_filtered.columns.values[10] = "Length_node"

    # Fit the binomial logistic regression model
    X = df_filtered[["norm_median_distance"]]
    y = df_filtered["nu"]

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


def diameter_proba(shoot, xcol="length", ycol="diam"):
    # Prepara i dati
    xdata = shoot[xcol]
    ydata = shoot[ycol]

    # Esegui il fitting del modello ai dati
    # `popt` conterrà i parametri ottimizzati (a e b in questo caso)
    popt, pcov = curve_fit(model_func, xdata, ydata, p0=[1, 1])

    print(f"Parameters: a={popt[0]:.2f}, b={popt[1]:.2f}")
def diameter_proba(length):
    mean = 0.152679 * (length ** 0.37395)
    std = 0.05933
    return mean, std


def length_new_juven_proba(length, norm_distance):
    mean = 0.07 * length + 87.60 * norm_distance
    std = sqrt(100.63)
    return mean, std


def nb_mv_in_sylleptic_lambda(met_sylleptic):
    # Fit the binomial logistic regression model
    X = met_sylleptic["parent_length_cm", "abs_norm_median_distance"]
    y = met_sylleptic["tot_buds_syl_m_v"]

    X = sm.add_constant(X)

    model = sm.GLM(y, X, family=sm.families.Poisson()).fit()

    # Print the summary of the model
    print(model.summary())

    # Calculate and print AIC
    aic = model.aic
    print("AIC: nb_mv_in_sylleptic_lambda", aic)
