import pandas as pd
import csv

OLDEST_YEAR = 10
MOST_RECENT_YEAR = 17

cols_df = pd.read_csv('B21001-columns.csv')
cols_df['Gender'] = cols_df['name'].apply(lambda s: s.split(':')[0])
cols_df['Age'] = cols_df['name'].apply(lambda s: s.split(':')[1])
cols_df['Veteran Status'] = cols_df['name'].apply(lambda s: s.split(':')[2])

final = [['Town', 'FIPS', 'Year', 'Age', 'Veteran Status', 'Gender', 'Measure Type', 'Variable', 'Value']]

def add_to_final(raw, r, year):
    for idx, town_data in raw.iterrows():
        age = r['Age']
        age_ = ' - ' + age + ':' if age != 'Total' else ''
        vet = r['Veteran Status']
        vet_ = ' - ' + vet if vet != 'Total' else ''
        sex = r['Gender']
        sex_ = ' - ' + sex + ':' if sex != 'Total' else ''

        year_full = str(1996 + year) + '-' + str(2000 + year)

        final.append([
            town_data['Geography'].split(' town,')[0],
            town_data['Id2'],
            year_full,
            age,
            vet,
            sex,
            'Number',
            'Veteran Status',
            int(town_data['Estimate; Total:' + sex_ + age_ + vet_]) if ('Estimate; Total:' + sex_ + age_ + vet_) in town_data else 'ILYA'
        ])

        final.append([
            town_data['Geography'].split(' town,')[0],
            town_data['Id2'],
            year_full,
            age,
            vet,
            sex,
            'Percent',
            'Veteran Status',
            float(int(town_data['Estimate; Total:' + sex_ + age_ + vet_]) / int(town_data['Estimate; Total:'])) if (('Estimate; Total:' + sex_ + age_ + vet_) in town_data & int(town_data['Estimate; Total:']) > 0) else 'ILYA'
        ])

        final.append([
            town_data['Geography'].split(' town,')[0],
            town_data['Id2'],
            year_full,
            age,
            vet,
            sex,
            'Percent',
            'Margins of Error',
            float(town_data['Margin of Error; Total:' + sex_ + age_ + vet_]) if ('Margin of Error; Total:' + sex_ + age_ + vet_) in town_data else 'ILYA'
        ])
    

for year in range(OLDEST_YEAR, MOST_RECENT_YEAR + 1):
    raw = pd.read_csv('../raw/ACS_' + str(year) + '_5YR_B21001_with_ann.csv', dtype='str', skiprows=1)

    for idx, r in cols_df.iterrows():
        add_to_final(raw, r, year)

pd.DataFrame(final).to_csv('test.csv', index=False, header=False, quoting=csv.QUOTE_NONNUMERIC)