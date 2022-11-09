import numpy as np
import libpysal as ps 
from mgwr.gwr import GWR, MGWR
from mgwr.sel_bw import Sel_BW
from mgwr.utils import shift_colormap, truncate_colormap
from mgwr.utils import compare_surfaces, truncate_colormap
import geopandas as gp
import matplotlib.pyplot as plt
import matplotlib as mpl
import pandas as pd
import pickle

vars_norm_data = gp.read_file("python_model.gpkg")
vars_norm_area = gp.read_file("vars_norm.gpkg")
fig, ax = plt.subplots(figsize=(10,10))
vars_norm_area.plot(ax=ax, **{'edgecolor':'black', 'facecolor':'white'})
vars_norm_data.plot(ax=ax, c='black')
plt.show()

g_y = vars_norm_data['n'].values.reshape(-1,1)
g_X = vars_norm_data[['estPop2019', 'blackPct', 'mltEthnPct', 'otherEthn','carOwn','IMDeduS','housePrice5yrsAvg','IMDtraffic', 'IMDair']].values
u = vars_norm_data['x']
v = vars_norm_data['y']
g_coords = list(zip(u, v))

gwr_selector = Sel_BW(g_coords, g_y, g_X)
gwr_bw = gwr_selector.search()
gwr_model = GWR(g_coords, g_y, g_X, gwr_bw)

pickle.dump(gwr_model, open("gwr_model.sav", "wb"))

#with open("gwr_model.sav","rb") as f:
#    gwr_model = pickle.load(f)
#print(gwr_model)

gwr_results = gwr_model.fit()

pickle.dump(gwr_results, open("gwr_results.sav", "wb"))

#with open("python_output/gwr_result.sav","rb") as f:
#    gwr_results = pickle.load(f)
#print(gwr_results)

gwr_results.summary()
gwr_filtered_t = gwr_results.filter_tvals(alpha=0.1)

gwr_collinearity=gwr_results.local_collinearity()
pickle.dump(gwr_collinearity, open("gwr_collinearity.sav","wb"))
np.savetxt("gwr_collinearity_v.csv",gwr_collinearity[0],delimiter=",")
np.savetxt("gwr_collinearity_p.csv",gwr_collinearity[1],delimiter=",")

gwr_bws_CI=gwr_results.get_bws_intervals(gwr_selector)
pickle.dump(gwr_bws_CI, open("gwr_bw_CI.sav","wb"))


###add gwr coefficients
vars_norm_area['gwr_intercept'] = gwr_results.params[:,0]
vars_norm_area['gwr_estPop2019'] = gwr_results.params[:,1]
vars_norm_area['gwr_blackPct'] = gwr_results.params[:,2]
vars_norm_area['gwr_mltEthnPct'] = gwr_results.params[:,3]
vars_norm_area['gwr_otherEthn'] = gwr_results.params[:,4]
vars_norm_area['gwr_carOwn'] = gwr_results.params[:,5]
vars_norm_area['gwr_IMDeduS'] = gwr_results.params[:,6]
vars_norm_area['gwr_housePrice5yrsAvg'] = gwr_results.params[:,7]
vars_norm_area['gwr_IMDtraffic'] = gwr_results.params[:,8]
vars_norm_area['gwr_IMDair'] = gwr_results.params[:,9]

###add corrected t test values 
###on gwr
vars_norm_area['gwr_t_intercept'] = gwr_filtered_t[:,0]
vars_norm_area['gwr_t_estPop2019'] = gwr_filtered_t[:,1]
vars_norm_area['gwr_t_blackPct'] = gwr_filtered_t[:,2]
vars_norm_area['gwr_t_mltEthnPct'] = gwr_filtered_t[:,3]
vars_norm_area['gwr_t_otherEthn'] = gwr_filtered_t[:,4]
vars_norm_area['gwr_t_carOwn'] = gwr_filtered_t[:,5]
vars_norm_area['gwr_t_IMDeduS'] = gwr_filtered_t[:,6]
vars_norm_area['gwr_t_housePrice5yrsAvg'] = gwr_filtered_t[:,7]
vars_norm_area['gwr_t_IMDtraffic'] = gwr_filtered_t[:,8]
vars_norm_area['gwr_t_IMDair'] = gwr_filtered_t[:,9]

###add residuals
vars_norm_area['gwr_std_resid'] = gwr_results.resid_response


######MGWR########

mgwr_selector = Sel_BW(g_coords, g_y, g_X, multi = True)
mgwr_bw = mgwr_selector.search(multi_bw_min = [2])
mgwr_results = MGWR(g_coords, g_y, g_X, mgwr_selector).fit()
pickle.dump(mgwr_results, open("mgwr_results.sav", "wb"))
pickle.dump(mgwr_bw, open("mgwr_results.sav", "wb"))
#with open("python_output/mgwr_results.sav","rb") as f:
#    mgwr_results = pickle.load(f)
#print(mgwr_results)

mgwr_filtered_t = mgwr_results.filter_tvals(alpha=0.1)

###add mgwr coefficients
vars_norm_area['mgwr_intercept'] = mgwr_results.params[:,0]
vars_norm_area['mgwr_estPop2019'] = mgwr_results.params[:,1]
vars_norm_area['mgwr_blackPct'] = mgwr_results.params[:,2]
vars_norm_area['mgwr_mltEthnPct'] = mgwr_results.params[:,3]
vars_norm_area['mgwr_otherEthn'] = mgwr_results.params[:,4]
vars_norm_area['mgwr_carOwn'] = mgwr_results.params[:,5]
vars_norm_area['mgwr_IMDeduS'] = mgwr_results.params[:,6]
vars_norm_area['mgwr_housePrice5yrsAvg'] = mgwr_results.params[:,7]
vars_norm_area['mgwr_IMDtraffic'] = mgwr_results.params[:,8]
vars_norm_area['mgwr_IMDair'] = mgwr_results.params[:,9]

###corrected t values
vars_norm_area['mgwr_t_intercept'] = mgwr_filtered_t[:,0]
vars_norm_area['mgwr_t_estPop2019'] = mgwr_filtered_t[:,1]
vars_norm_area['mgwr_t_blackPct'] = mgwr_filtered_t[:,2]
vars_norm_area['mgwr_t_mltEthnPct'] = mgwr_filtered_t[:,3]
vars_norm_area['mgwr_t_otherEthn'] = mgwr_filtered_t[:,4]
vars_norm_area['mgwr_t_carOwn'] = mgwr_filtered_t[:,5]
vars_norm_area['mgwr_t_IMDeduS'] = mgwr_filtered_t[:,6]
vars_norm_area['mgwr_t_housePrice5yrsAvg'] = mgwr_filtered_t[:,7]
vars_norm_area['mgwr_t_IMDtraffic'] = mgwr_filtered_t[:,8]
vars_norm_area['mgwr_t_IMDair'] = mgwr_filtered_t[:,9]

#add residuals
vars_norm_area['mgwr_std_resid'] = mgwr_results.resid_response

vars_norm_area.to_file("var_norm_modelled_mgwr_90.gpkg", layer='var_norm_area', driver="GPKG")

mgwr_collinearity=mgwr_results.local_collinearity()

pickle.dump(mgwr_collinearity, open("mgwr_collinearity.sav","wb"))
np.savetxt("mgwr_collinearity_v.csv",mgwr_collinearity[0],delimiter=",")
np.savetxt("mgwr_collinearity_p.csv",mgwr_collinearity[1],delimiter=",")

mgwr_bws_CI=mgwr_results.get_bws_intervals(mgwr_selector)

pickle.dump(mgwr_bws_CI, open("mgwr_bws_CI","wb"))

# with open("python_output/gwr_bw_CI.sav","rb") as f:
#     gwr_bws_CI = pickle.load(f)
# print(gwr_bws_CI)
# 
# with open("python_output/mgwr_bws_CI.sav","rb") as f:
#     mgwr_bws_CI = pickle.load(f)
# print(mgwr_bws_CI) 
