import plotly
import plotly.express as px
import numpy as np
import pandas as pd
import logging
import seaborn as sns

# ===== START LOGGER =====
logger = logging.getLogger(__name__)


root_logger = logging.getLogger()
root_logger.setLevel(logging.INFO)
sh = logging.StreamHandler()
formatter = logging.Formatter(
    '%(asctime)s - %(name)s - %(levelname)s - %(message)s')
sh.setFormatter(formatter)
root_logger.addHandler(sh)

# ==================================================
# ========== LOAD DATA =============================
# ==================================================

# weatherbuoy_df = pd.read_csv('https://erddap.marine.ie/erddap/tabledap/IWBNetwork.csvp?station_id%2Ctime%2CSeaTemperature')
weatherbuoy_df = pd.read_csv('data\IWBNetwork_archive.csv')
weatherbuoy_df['time (UTC)'] = pd.to_datetime(weatherbuoy_df['time (UTC)'])
weatherbuoy_df['year'] = weatherbuoy_df['time (UTC)'].dt.year
weatherbuoy_df['month'] = weatherbuoy_df['time (UTC)'].dt.month
weatherbuoy_df['day'] = weatherbuoy_df['time (UTC)'].dt.day
weatherbuoy_df['mean'] = weatherbuoy_df.groupby(['year', 'station_id'])[
    "SeaTemperature (degrees_C)"].transform('mean')
weatherbuoy_df['min'] = weatherbuoy_df.groupby(['year', 'station_id'])[
    "SeaTemperature (degrees_C)"].transform('min')
weatherbuoy_df['max'] = weatherbuoy_df.groupby(['year', 'station_id'])[
    "SeaTemperature (degrees_C)"].transform('max')
# weatherbuoy_df['key'] = pd.RangeIndex(stop=weatherbuoy_df.shape[0])

# print(weatherbuoy_df.head())
# weatherbuoy_df.to_csv('data\IWBNetwork_output.csv')

simple_df = weatherbuoy_df[(weatherbuoy_df.station_id == 'M2')]
fig = px.bar(simple_df, x='year', y='mean', color='SeaTemperature (degrees_C)')
fig.show()

# site_names = weatherbuoy_df.station_id.unique()
# fig = px.bar(simple_df, x='year', y='SeaTemperature (degrees_C)',
#  color='SeaTemperature (degrees_C)', facet_row='station_id', facet_col='station_id')
# fig.show()


site_names = weatherbuoy_df.station_id.unique()[:4]
# year_list = [yr for yr in weatherbuoy_df.year.unique(
# ) if yr in weatherbuoy_df.year.unique()]
# year_list.sort()

# fig = make_subplots(rows=2, cols=2)

# for i in range(4):
#     site_name = site_names[i]
#     temp_vals = list()
#     for yr in year_list:
#         filt_val = weatherbuoy_df[
#             (weatherbuoy_df.station_id == site_name)
#             & (weatherbuoy_df.year == yr)
#         ]
#         if len(filt_val) > 0:
#             temp_val = filt_val['mean'].values[0]
#         else:
#             logger.info(f'Filling value for {site_name} in {yr} to be 0')
#             temp_val = 0
#         temp_vals.append(temp_val)
#     fig.add_trace(
#         go.Bar(
#             x=year_list,
#             y=temp_vals,
#         ),
#         row=(i // 2) + 1, col=((i % 2) + 1),
#     )

# fig.update_layout(barmode='relative', height=500, width=700)
# fig.show(config={'displayModeBar': False})

sns.set_theme(style="darkgrid", palette="pastel")
sns.lineplot(data=simple_df, linewidth=2.5)
