import pandas as pd

df = pd.read_csv("C:/Users/jiyaw/Downloads/Rodent_Inspection.csv") #want to read this csv
cols = df.columns

#need to add a year column

df['Year'] = df['INSPECTION_DATE'].apply(lambda x: str(x)[6:10])
#-1 access the last element in the split (aka the year)
#the delimiter is a slash

for i in set(df.Year): # for classified by years files
    filename = 'C:/Users/jiyaw/Downloads/Rodent_Inspection' + i + '.csv'
    df.loc[df.Year == i].to_csv(filename,index=False,columns=cols) #we want the original cols in this new .csv file!


