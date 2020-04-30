#
# travel.py : calculate commission paid per product
#
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
plt.style.use('ggplot')

xlswb = "travel insurance.xlsx"
premium = pd.read_excel(xlswb, sheet_name="premium")
commission = pd.read_excel(xlswb, sheet_name="commission")

outp = (premium.query("channel=='Online'")
               .merge(commission)
               .assign(commission=lambda x:
                       x.pct_commission * x.premium)
               .groupby('agent')
               .apply(np.sum)
               .sort_values('commission')
               .plot(y='commission', kind='barh',
                     title='Provision per Agent',
                     legend=False, color='blue').set(xlabel="USD", ylabel="Agent"))

plt.show()
