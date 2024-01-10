
import pandas as pd
import numpy as np




df = pd.DataFrame({'age': [29, 55, 65, 18],
                   'gender': ['male', 'female', 'male', 'female'],
                   'hr1': [98.1, 78, 65, 64],
                   'hr2': [110, 120, 129, 141],
                   'hr3': [76, 87, 77, 59]},
                  index=['p1', 'p2', 'p3', 'p4'])
df

df['hr_mean'] = (df['hr1'] + df['hr2'] + df['hr3']) / 3
df['age_sqrt'] = df['age']**(1/2)

df




print(df['hr1'].skew())
print((df['hr1']**(1/2)).skew())
print((df['hr1']**(1/3)).skew())
print(np.log(df['hr1']).skew())

df['hr1_ln'] = np.log(df['hr1'])
df


df_new = df.copy()

df_new.loc[df_new['gender'] == 'female', 'gender'] = 0
df_new.loc[df_new['gender'] == 'male', 'gender'] = 1
df_new['gender'] = df_new['gender'].astype('int64')
df_new


from datetime import datetime as dt

df['signup_date'] = ['02/14/2019', '01/05/2018', '05/23/2020', '12/10/2019']
df['complete_date'] = ['02/27/2020', '10/22/2020', '09/11/2021', '07/05/2022']

# Convert to datetime
df['signup_date'] = pd.to_datetime(df['signup_date'])
df['complete_date'] = pd.to_datetime(df['complete_date'])

# Calculate days since signup
df['days_since_signup'] = (dt.strptime("2023-1-1", '%Y-%m-%d') - df['signup_date']).dt.days

df['days_to_completion'] = (df['complete_date'] - df['signup_date']).dt.days
df['years_to_completion'] = df['days_to_completion']/365
df['weeks_to_completion'] = df['days_to_completion']/7
df['months_to_completion'] = df['days_to_completion']/(365/12)

df.describe()

pd.to_datetime(df['signup_date']).dt.date

pd.to_datetime(df['signup_date']).dt






cost = pd.read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv", encoding = "ISO-8859-1")

cost2 = cost[['name', 'type', 'degree_length', 'in_state_tuition']]


income = pd.read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv")


income2 = (income.filter(["name", "total_price", "year"])
                 .groupby(["name", "year"])
                 .agg({"total_price": "mean"})
                 .reset_index()
                 .sort_values(["name", "year"])
                 .rename(columns = {"total_price": "avg_per_year"}))





# see which university has the most consistently increasing tuition
income3 = (income2.assign(
        years_of_data = lambda x: x.groupby("name")["name"].transform("count"),
        tuition_increase = lambda x: x.groupby("name")["avg_per_year"].transform(lambda x: np.append(np.nan, np.diff(x))))

    # filter out tuition where there has not been an increase in tuition each year
    .query("(tuition_increase >= 0) or tuition_increase != tuition_increase")
    .assign(years_with_tuition_increase = lambda x: x.groupby("name")["name"].transform("count"))

    # throw out rows where there has not been a consistent increase intuition over the years
    .query("(years_with_tuition_increase == years_of_data)"))

