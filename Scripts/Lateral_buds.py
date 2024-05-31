# Exploratory analysis of lateral buds

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.stats.proportion import proportions_ztest
from matplotlib.colors import ListedColormap
from matplotlib.ticker import MultipleLocator, AutoMinorLocator
from scipy.stats import chi2_contingency
import seaborn as sns
from matplotlib import cm


def freq_table_class_length(bud_df: pd.DataFrame, class_column: str, fate_column: str,
                            class_new_shoot_col: str, nb_new_shoots_col: str, shoot_id_col: str):
    class_length = pd.DataFrame()

    # Itera per ogni classe unica in `bud`.
    for i in bud_df[class_column].unique():
        class_buds = {}
        class_newshoot = {}

        # Calcola la frequenza dei boccioli per ogni 'fate' all'interno della classe.
        for j in bud_df[fate_column].unique():
            class_buds[j] = len(bud_df[(bud_df[class_column] == i) & (bud_df[fate_column] == j)])

        # Calcola il numero di nuovi germogli per ogni classe all'interno della classe.
        for q in bud_df[class_column].unique():
            class_newshoot[f"new_shoot_{q}"] = bud_df[(bud_df[class_column] == i) & (bud_df[class_new_shoot_col] == q)][
                nb_new_shoots_col].sum(skipna=True)

        # Aggiunge i risultati al DataFrame `class_length`.
        row_data = {'nb_shoots': bud_df[bud_df[class_column] == i][shoot_id_col].nunique(),
                    'tot_buds_m_v_b_c': len(bud_df[bud_df[class_column] == i][fate_column]),
                    'nb_new_shoots': bud_df[bud_df[class_column] == i][nb_new_shoots_col].sum(),
                    **class_buds,
                    **class_newshoot}

        # Crea un DataFrame temporaneo con i risultati per la classe corrente e lo appende a `class_length`.
        temp_df = pd.DataFrame([row_data], index=[i])
        class_length = pd.concat([class_length, temp_df])

    return class_length


def freq_table_prol_syl(bud_df: pd.DataFrame, shoot_type_col: str, fate_column: str,
                        nb_new_shoots_col: str, shoot_id_col: str):
    data_list = []

    # Itera per ogni classe unica in `bud`
    for i in bud_df[shoot_type_col].unique():
        data = {'shoot_type': i, 'nb_shoots': bud_df[bud_df[shoot_type_col] == i][shoot_id_col].nunique()}

        for j in ["M", "V"]:
            data[f'buds_from_{j}'] = len(bud_df[(bud_df[shoot_type_col] == i) & (bud_df[fate_column] == j)])

        for q in bud_df[fate_column].unique():
            data[f'new_shoot_from_{q}'] = bud_df[(bud_df[shoot_type_col] == i) & (bud_df[fate_column] == q)][
                nb_new_shoots_col].sum(skipna=True)

        data[nb_new_shoots_col] = bud_df[bud_df[shoot_type_col] == i][nb_new_shoots_col].sum()

        # Aggiunge i dati raccolti alla lista
        data_list.append(data)

    # Converti la lista in DataFrame
    shoot_type_summary = pd.DataFrame(data_list)

    # Calcola le proporzioni di "bud burst" e "errors"
    shoot_type_summary['bud_burst'] = (shoot_type_summary['new_shoot_from_M'] + shoot_type_summary[
        'new_shoot_from_V']) / (shoot_type_summary['buds_from_M'] + shoot_type_summary['buds_from_V'])
    shoot_type_summary['errors'] = (shoot_type_summary['new_shoot_from_C'] + shoot_type_summary['new_shoot_from_B']) / \
                                   shoot_type_summary['nb_new_shoots']

    return (shoot_type_summary)


def freq_buds_in_rank_sylleptic(metamer_syl_df: pd.DataFrame, rank_node_col: str,
                                c_col: str, v_col: str, m_col: str, b_col: str) -> pd.DataFrame:
    """
    Calcola la frequenza dei germogli silleptici per ogni rango nodale.

    Parametri:
    - metamer_syl_df (pd.DataFrame): DataFrame contenente i dati sui metameri.
    - rank_node_col (str): Nome della colonna che identifica il rango del nodo.
    - c_col (str): Nome della colonna per i conteggi di germogli 'c'.
    - v_col (str): Nome della colonna per i conteggi di germogli 'v'.
    - m_col (str): Nome della colonna per i conteggi di germogli 'm'.
    - b_col (str): Nome della colonna per i conteggi di germogli 'b'.

    Ritorna:
    pd.DataFrame: DataFrame riepilogativo con le frequenze dei germogli per rango nodale.
    """

    nline = len(metamer_syl_df[rank_node_col].unique())
    tab_syl = pd.DataFrame(index=np.arange(1, nline + 1),
                           columns=['rank_node', 'nb_shoots', c_col, v_col, m_col, b_col])

    for q, Q in enumerate(sorted(metamer_syl_df[rank_node_col].unique()), 1):
        subset = metamer_syl_df[metamer_syl_df[rank_node_col] == Q]
        tab_syl.loc[q, 'rank_node'] = Q
        tab_syl.loc[q, 'nb_shoots'] = len(subset)
        tab_syl.loc[q, c_col] = subset[c_col].sum()
        tab_syl.loc[q, v_col] = subset[v_col].sum()
        tab_syl.loc[q, m_col] = subset[m_col].sum()
        tab_syl.loc[q, b_col] = subset[b_col].sum()

    # Aggiungere la somma per ogni colonna e per ogni riga
    tab_syl.loc['sums', [c_col, v_col, m_col, b_col]] = tab_syl[[c_col, v_col, m_col, b_col]].sum()
    tab_syl['sums'] = tab_syl[[c_col, v_col, m_col, b_col]].sum(axis=1)

    # relative frequence
    for col in [c_col, v_col, m_col, b_col]:
        tab_syl[f'%{col.upper()}'] = (tab_syl[col].astype(float) / tab_syl['nb_shoots'].astype(float) * 100).round(2)

    return tab_syl

def plot_scatter(df, x_col, y_col, title):
    plt.scatter(df[x_col], df[y_col])
    plt.title(title)
    plt.xlabel(x_col)
    plt.ylabel(y_col)
    plt.show()


# Supponiamo che met_proleptic sia il DataFrame e che contenga una colonna 'shoot_type' categorica
# e diverse colonne continue come 'length', 'rank_node', ecc.

def plot_proportion(df, x_var, cat_var, ax, num_bins=10):
    # Bin the continuous variable
    bins = np.linspace(df[x_var].min(), df[x_var].max(), num_bins)
    df['binned'] = pd.cut(df[x_var], bins, include_lowest=True)

    # Calcola la proporzione per ciascun bin
    proportions = df.groupby(['binned', cat_var], observed=True).size().unstack().fillna(0)
    proportions = proportions.div(proportions.sum(axis=1), axis=0)

    # Plot
    proportions.plot(kind='bar', stacked=True, ax=ax)
    ax.set_title(f'Proportion of {cat_var} by {x_var}')
    ax.set_xlabel(x_var)
    ax.set_ylabel('Proportion')

def calculate_class_summary(df, class_col, count_cols):
    # Inizializza liste per raccogliere i dati
    data = {col: [] for col in [class_col] + count_cols}
    data['Total'] = []

    # Aggrega i dati per classe
    for i in df[class_col].unique():
        subset = df[df[class_col] == i]
        data[class_col].append(i)
        for col in count_cols:
            data[col].append(subset[col].sum())
        data['Total'].append(subset[count_cols].sum().sum())

    # Crea un DataFrame dai dati raccolti
    summary_df = pd.DataFrame(data)

    # Calcola le percentuali relative
    for col in count_cols:
        summary_df[f'%{col}'] = (summary_df[col] / summary_df['Total']) * 100

    # Calcola la somma delle colonne e delle righe se necessario
    # Aggiungi ulteriori operazioni qui...

    return summary_df


def plot_and_test(data, row_slice, col_slice, obs_col_index, plot_title, xlabel, ylabel, ymax=100, test=True):
    # Extract frequency matrix and column labels for the plot
    freq = data.iloc[row_slice, col_slice].values
    col_labels = data.columns[col_slice]

    # Create the bar plot
    colors = plt.get_cmap('Set2')
    fig, ax = plt.subplots(figsize=(12, 9))

    # Create a bar plot, grouped by 'class'
    x = np.arange(len(data.index[row_slice]))  # the label locations
    width = 0.2  # the width of the bars

    for i, category in enumerate(col_labels):
        ax.bar(x + i * width, data[category], width, label=category, color=colors(i))

    # Add some text for labels, title and custom x-axis tick labels, etc.
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.set_title(f"{ylabel} by class and category")
    ax.set_xticks(x + width + width / 2)
    ax.set_xticklabels(data[xlabel])
    ax.legend(title='Legend')
    plt.ylim(0, ymax)

    if test:
        # Correcting statistical tests approach
        for j in range(freq.shape[1]):
            print(f"\nTesting column: {col_labels[j]}")
            for i in range(freq.shape[0] - 1):
                for k in range(i + 1, freq.shape[0]):
                    # Performing z-test between each pair of groups
                    stat, p_value = proportions_ztest([freq[i, j], freq[k, j]],
                                                      [data.iloc[i, obs_col_index], data.iloc[k, obs_col_index]])
                    print(f"Comparison between {data.index[i]} and {data.index[k]}: p-value = {p_value}")
    plt.show()


def barplot_annotation(df, xlab, title):
    colors = plt.get_cmap('Set2')
    fig, ax = plt.subplots(figsize=(12, 9))
    x = np.arange(len(df.index))  # the label locations
    width = 0.8  # the width of the bars
    for i, category in enumerate(df.index):
        ax.bar(x[i], df.iloc[i, :], width, label=category, color=colors(i))
        plt.text(x[i], df.iloc[i, :] + 3, f"{round(df.iloc[i, :][0], 2)}", ha='center')

    ax.set_xlabel(xlab)
    ax.set_ylabel("%")
    ax.set_title(title)
    ax.set_xticks(x)
    ax.set_xticklabels(df.index.tolist())
    ax.legend(title='Legend')
    plt.ylim(0, 100)
    plt.show()

def process_rank_data(met_proleptic):
    # Initialize the DataFrame with the required structure
    class_rank = pd.DataFrame(columns=['rank_node', 'nb_shoots', 'sylleptic', 'v', 'm', 'b'])

    # Get unique, sorted ranks
    unique_ranks = sorted(met_proleptic['rank_node'].unique())

    # Loop through each unique rank and perform calculations
    for Q in unique_ranks:
        filtered_data = met_proleptic[met_proleptic['rank_node'] == Q]
        class_rank.loc[Q] = {
            'rank_node': Q,
            'nb_shoots': len(filtered_data['shoot_ID']),
            'sylleptic': filtered_data['sylleptic'].sum(),
            'v': filtered_data['v'].sum(),
            'm': filtered_data['m'].sum(),
            'b': filtered_data['b'].sum()
        }

    # Calculate column sums and add as a new row at the end
    class_rank.loc['sums'] = class_rank.sum(numeric_only=True)

    # Calculate sum of specific columns for each row and store in a new column
    class_rank['sum_obs'] = class_rank[['sylleptic', 'v', 'm', 'b']].sum(axis=1)

    return class_rank

def calculate_relative_frequencies(class_rank):
    # Assuming columns 3 to 6 in class_rank correspond to 'sylleptic', 'v', 'm', 'b' and the totals are in column 7 (as per your R code)
    # Check the exact DataFrame structure with print(class_rank.columns) and adjust indices accordingly
    indices_mapping = {2: 'sylleptic', 3: 'v', 4: 'm', 5: 'b', 6: 'sum_obs'}  # 0-based indexing in Python

    # Calculate relative frequencies and round to 2 decimal places
    for key, value in indices_mapping.items():
        if key < 6:  # Exclude sum_obs from division operation
            class_rank[f'%{value.upper()}'] = round((class_rank.iloc[:, key] / class_rank['sum_obs']) * 100, 2)

    return class_rank


def plot_rank_data(class_rank, start_row=0, end_row=16):
    # Work with a copy to avoid SettingWithCopyWarning when modifying the DataFrame
    class_rank = class_rank.copy()
    colors = plt.get_cmap('Set2')

    # Convert 'rank_node' to categorical for consistent plotting
    class_rank['rank_node'] = pd.Categorical(class_rank['rank_node'])

    plt.figure(figsize=(10, 8))  # Set the figure size

    # Define markers and colors for the plot
    markers = ['o', 5, 'x', '*']  # Define markers for each plot
    labels = ['%SYLLEPTIC', '%V', '%M', '%B']

    for i, label in enumerate(labels):
        if label in class_rank.columns:
            # Ensure the data slices are handled as arrays for plotting
            max_index = min(end_row, len(class_rank))
            y_data = class_rank[label][start_row:max_index].to_numpy()
            x_data = np.arange(start_row, max_index)  # Ensure x_data aligns with y_data length

            # Plot the data
            plt.plot(x_data, y_data, marker=markers[i], linestyle='-', label=label, markersize=6, color=colors(i))

    # Set x-ticks and labels correctly
    if 'rank_node' in class_rank:
        plt.xticks(np.arange(start_row, end_row), class_rank['rank_node'][start_row:end_row], rotation=45)
    plt.title("Frequency of Buds/Shoots in Proleptic <Own-Rooted> Parentals", fontsize=14)
    plt.xlabel("Rank Nodes", fontsize=12)
    plt.ylabel("%", fontsize=12)
    plt.ylim(0, 100)  # Set y-axis limits
    plt.legend(title="Legend", loc="upper left")
    plt.grid(False)
    plt.show()

def plot_stacked_barchart(data, colors, figsize=(12, 9), title="Number of Buds/Sylleptic per Rank in Proleptic Shoots", xlabel="Rank Nodes", ylabel="Number of Multiple Buds/Sylleptic"):

    # Create the bar chart
    plt.figure(figsize=figsize)
    ax = data.plot(kind='bar', stacked=True, color=colors, width=0.6)
    plt.title(title)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)

    # Adjust legend position
    ax.legend(loc='upper right', bbox_to_anchor=(1.0, 1.0), title="Types of Buds/Sylleptic")

    # Show the plot
    plt.tight_layout()
    plt.show()

def process_and_tabulate(df, columns, colum = "rank_node"):
    # Transform each specified column
    for col in columns:
        df[col] = df[col].apply(lambda x: '+'.join([col] * x) if x > 0 else np.nan)

    # Merge transformed columns into a single column
    df['merge'] = df[columns].apply(lambda x: '+'.join(x.dropna()), axis=1)

    # Create a frequency table
    frequency_table = pd.crosstab(df[colum], df['merge'])

    return frequency_table


def plot_bud_development(df, title="Proportion of Proleptic Buds Developed",
                         x_label="Rank Nodes", y_label="%", figsize=(10, 6)):
    # Define a custom color palette
    colors = ['#66c2a5', '#fc8d62']  # Example colors, adjust if necessary

    # Create the plot
    plt.figure(figsize=figsize)

    # Plotting vegetative buds
    plt.plot(df.index[:16].to_numpy(), df['V'][:16].to_numpy(), 'o-', color=colors[0],
             markersize=7, linewidth=2, label='from Vegetative buds')

    # Plotting mixed buds
    plt.plot(df.index[:16].to_numpy(), df['M'][:16].to_numpy(), 'o-', color=colors[1],
             markersize=7, linewidth=2, label='from Mixed buds')

    # Adding labels and title
    plt.xlabel(x_label)
    plt.ylabel(y_label)
    plt.title(title)
    plt.ylim(0, 101)
    plt.legend(loc='lower right', frameon=True, title='Bud Type')
    plt.show()

def generate_frequency_table(df, class_col='length_newshoots'):
    lne = df.columns.get_loc(class_col)
    new_data = pd.DataFrame(columns=['shoot_ID', 'rank_node', 'class'])
    for shoot_id in df['shoot_ID'].unique():
        for rank_node in df['rank_node'].unique():
            m = df[(df['shoot_ID'] == shoot_id) & (df['rank_node'] == rank_node)].iloc[:, lne]
            if not m.empty:
                class_combination = '+'.join(m.dropna())
                new_row = pd.DataFrame({'shoot_ID': [shoot_id], 'rank_node': [rank_node], 'class': [class_combination]})
                new_data = pd.concat([new_data, new_row], ignore_index=True)
    new_data['rank_node'] = pd.to_numeric(new_data['rank_node'])
    new_data = new_data[new_data['class'] != ""].sort_values(by='rank_node')
    return pd.crosstab(new_data['class'], new_data['rank_node'])

def plot_frequency_tables_side_by_side(freq_table1, title1, ylabel1, freq_table2, title2, ylabel2):
    fig, axs = plt.subplots(1, 2, figsize=(10, 6))
    colors = cm.get_cmap('Set3', len(freq_table1.columns)).colors

    # Plot first frequency table
    freq_table1.T.plot(kind='bar', stacked=True, ax=axs[0], color=colors)
    axs[0].set_title(title1)
    axs[0].set_xlabel("Rank nodes")
    axs[0].set_ylim([0,5])
    axs[0].set_ylabel(ylabel1)
    axs[0].legend(title="Class", bbox_to_anchor=(1.05, 1), loc='upper left')

    # Plot second frequency table
    freq_table2.T.plot(kind='bar', stacked=True, ax=axs[1], color=colors)
    axs[1].set_title(title2)
    axs[1].set_xlabel("Rank nodes")
    axs[1].set_ylabel(ylabel2)
    axs[1].set_ylim([0,5])
    axs[1].legend(title="Class", bbox_to_anchor=(1.05, 1), loc='upper left')

    plt.tight_layout()
    plt.show()


def developinprol(fromV_matrix, fromV_freq, budtype, xlim=(1, 23), ylim=(0, 40)):
    colors = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728']

    # Create a figure with two subplots side by side
    fig, axs = plt.subplots(1, 2, figsize=(10, 6))

    # Plot the number of lateral shoots from vegetative buds
    for idx, col in enumerate(fromV_matrix.columns):
        axs[0].plot(fromV_matrix.index.to_numpy(), fromV_matrix[col].to_numpy(), 'o-', color=colors[idx], markersize=7,
                    linewidth=2, label=col)
    axs[0].set_xlabel('Parental rank nodes')
    axs[0].set_ylim(ylim)
    axs[0].set_ylabel('Numbers of lateral shoots')
    axs[0].set_title(f'from {budtype} bud in proleptic shoots')
    axs[0].xaxis.set_major_locator(MultipleLocator(2))
    axs[0].xaxis.set_minor_locator(AutoMinorLocator(2))
    axs[0].set_xlim(xlim)
    axs[0].legend()

    # Plot the relative frequency of lateral shoots from vegetative buds
    for idx, col in enumerate(fromV_freq.columns):
        axs[1].plot(fromV_freq.index.to_numpy(), fromV_freq[col].to_numpy(), 'o-', color=colors[idx], markersize=7,
                    linewidth=2, label=col)
    axs[1].set_xlabel('Parental rank nodes')
    axs[1].set_ylabel('% of lateral shoots')
    axs[1].set_title(f'from {budtype} bud in proleptic shoots')
    axs[1].xaxis.set_major_locator(MultipleLocator(2))
    axs[1].xaxis.set_minor_locator(AutoMinorLocator(2))
    axs[1].set_xlim(xlim)
    axs[1].legend()

    # Adjust layout and display the plots
    plt.tight_layout()
    plt.show()


def lengthlat(new_class, new_class_freq, maxrank=16):
    # Plot combinations of laterals from proleptic buds and their relative frequency side by side
    fig, axs = plt.subplots(1, 2, figsize=(10, 8))
    colors = sns.color_palette("Set3", len(new_class.columns))

    # Plot combinations of laterals from proleptic buds
    new_class.T.iloc[:maxrank].plot(kind='bar', stacked=True, ax=axs[0], color=colors)
    axs[0].set_title("Combinations of laterals from proleptic buds")
    axs[0].set_xlabel("Rank nodes")
    axs[0].set_ylabel("# of child class")
    axs[0].legend(loc='upper right', bbox_to_anchor=(1.2, 1))

    # Plot relative frequency of combinations of laterals from proleptic buds
    new_class_freq.T.iloc[:maxrank].plot(kind='bar', stacked=True, ax=axs[1], color=colors)
    axs[1].set_title("Relative frequency of laterals from proleptic buds")
    axs[1].set_xlabel("Rank nodes")
    axs[1].set_ylabel("% of child class")
    axs[1].set_ylim([0, 100])
    axs[1].legend(loc='upper right', bbox_to_anchor=(1.2, 1))

    plt.tight_layout()
    plt.show()

def lenprol(new_class_freq_matrix, lengthtocheck, new_shoot_order = ['Sh', 'Me', 'Lo', 'VLo']):
    fig, ax = plt.subplots(figsize=(10, 6))
    colors = ['#90C3D4', '#FF6F61', '#6B5B95', '#88B04B']
    bars = new_class_freq_matrix.plot(kind='bar', stacked=False, ax=ax, color=colors)
    ax.set_title("Lateral child length from proleptic")
    ax.set_xlabel("Parent length (proleptic)")
    ax.set_ylabel("% child length (e.g., #child/totalchildSh)")
    ax.set_ylim([0, 110])
    ax.legend(loc='upper right', bbox_to_anchor=(1.2, 1))
    # Add percentage annotations
    for i, container in enumerate(bars.containers):
        if new_shoot_order[i] in lengthtocheck:
            ax.bar_label(container, fmt='%.2f%%', label_type='edge')