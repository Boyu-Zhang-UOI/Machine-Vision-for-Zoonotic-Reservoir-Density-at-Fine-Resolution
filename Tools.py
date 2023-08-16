#import numpy as np
#import rasterio
#from scipy import interpolate
#import pandas as pd

## Aggregate data according to box_name and return TS (response) and TotTraps (weight)
def aggregate_data(dataset, box_name, col_names):
    col_list = ['Site', 'House', 'Visit', 'Night'] + [box_name]

    agg_data = pd.DataFrame(dataset.groupby(col_list).apply(lambda x: x.loc[:,col_names].mean()))
    agg_data.pop('House')
    agg_data.pop('Night')
    agg_data.reset_index(inplace = True)
    mn_outcome = pd.DataFrame(dataset.groupby(col_list).apply(lambda x: x.loc[x.Mna==1,'Trap.weight'].sum()))
    mn_outcome.columns = ['Tot_Mn']
    mn_outcome.reset_index(inplace = True)
    tot_traps = pd.DataFrame(dataset.groupby(col_list).apply(lambda x: x.loc[:,'Trap.weight'].sum()))
    tot_traps.columns = ['TotTraps']
    tot_traps.reset_index(inplace = True)
    visit_date = pd.DataFrame(dataset.groupby(col_list).apply(lambda x: x.loc[:,'Date'].min()))
    visit_date.columns = ['Date']
    
    agg_data = agg_data.merge(mn_outcome, on = col_list)
    agg_data = agg_data.merge(tot_traps, on = col_list)
    agg_data = agg_data.merge(visit_date, on = col_list)
    
    agg_data.loc[:,'TS_Mn'] = agg_data.loc[:,'Tot_Mn'] / agg_data.loc[:,'TotTraps']

    return agg_data

## Return evaluation datasets for fitting procedure
def get_eval_sets(all_dat, col_names):    
    dat_set = list()
    weight_set = list()
    grid_i = all_dat.gridi.unique().tolist()
    for gi in grid_i:
        mask_gi = all_dat.gridi==gi
        dat = all_dat.loc[mask_gi,:]
        dat_xvals = dat.loc[:,col_names]
        dat_yvals = dat.loc[:,'TS_Mn']
        dat_wvals = dat.loc[:,'TotTraps']
        mask_dat_house = dat.House.values==1
        gi_dat_set = [dat_xvals, dat_yvals]
        gi_weight_set = [dat_wvals]
        dat_set = dat_set + [gi_dat_set]
        weight_set = weight_set + gi_weight_set
    return dat_set, weight_set

# Return a list of variable names. Flags control which variable names are 
# returned
def get_var_names(prec_flag = True, radii = [25,50,100,200,500,1000], sat_flag = True, 
                  house_flag = True, verbose = False, night_flag = True, start_prec = 1):

    add_names = []
    ## Store chosen variable names here
    var_names = []   

    if night_flag:
        add_names = ['Night']
    if house_flag:
        add_names += ['House']

    if sat_flag:
        lc_names = ["Frac_bare","Frac_grass", "Frac_tree","Frac_burn", "Frac_rice",
                       "Frac_cloud","Frac_water", "Frac_mound"]
        build_names = ["Density_Buildings","Density_Moderns",
                       "Density_Traditionals"]
        sc_build_names = ['sc_' + x for x in build_names]

        sat_var_names = []
        sat_var_names += lc_names
        sat_var_names += build_names
        sat_var_names += sc_build_names
        sat_var_names = get_features(radii, sat_var_names)
        var_names += sat_var_names 
        
    if prec_flag:
        prec_names = [('P' + str(i)) for i in range(start_prec,13)]
        var_names += prec_names

    var_names += add_names

    var_names = [name for name in var_names if ((name!='MODIS') and ('cloud' not in name))]

    if verbose:
        print('Number of predictors: ' + str(len(var_names)))
    return(var_names)
            



## Return feature names from satellite imagery
def get_features(rvec, names):
    feature_list = list()
    for name in names:
        for r in rvec:
            feature_list += [name + '.' + str(r)] 
    return feature_list

## Apply set of models to raster and average result. Returns a numpy array
def get_forecast(test_site, test_raster, cxmid, cymid, model_set, n_iter, 
                 nuke_preds, var_names,
                forecast_year, forecast_month, house,
                save_path = None, 
                build_path = None, 
                lc_path = None):
    forecast_date = datetime.date(year = forecast_year, month = forecast_month, day = 1)

    focal_dates = [(forecast_date - relativedelta(months = n)) for n in range(1,13)]
    focal_precip_dates = [(prec_data_path + 'chirps-v2.0.' + fd.strftime("%Y.%m.%d") + '.tif') for fd in focal_dates]
    prec_names = [('P' + str(i)) for i in range(1,13)]

    nx = len(cxmid)
    ny = len(cymid)

    focal_df = get_sparse_block(cxmid, cymid, test_site, var_names, focal_precip_dates, prec_names,
                                house, build_path = build_path, lc_path = lc_path)

    focal_df.columns = var_names
    
    ## Sever predictors
    focal_df.loc[:,nuke_preds] = np.nan
    
    out_prob_all = []
    for modi in range(len(model_set)):
        focal_model = model_set[modi]
        iters = int(n_iter[modi])
        if str(type(focal_model)) == "<class 'xgboost.sklearn.XGBClassifier'>":
            result = focal_model.predict_proba(focal_df, iteration_range = (0,iters))[:,1]
        elif str(type(focal_model)) == "<class 'xgboost.sklearn.XGBRegressor'>":
            result = focal_model.predict(focal_df,  iteration_range = (0,iters))
        else: 
            print('error in model type')
          
        out_prob = np.reshape(result, (ny,nx), order = 'F')
        out_prob = np.flip(out_prob,0)
        out_prob = out_prob[:,:,np.newaxis]
        out_prob_all.append(out_prob)
    out_prob_conc = np.concatenate(out_prob_all, axis = 2)
    out_prob = out_prob_conc.mean(axis = 2)
    
    ## SAVE to RASTER
    if save_path is not None: 
        dc = np.unique(np.diff(cxmid))[0]
        make_dir(save_path)

        min_x = cvecx.min() - dc
        max_y = cvecy.max() + dc
        pixel_size = dc
        transform = from_origin(min_x, max_y, dc, dc)

        new_tif = rasterio.open(save_path + test_raster + '.tif', 'w', driver='GTiff',
                        height = out_prob.shape[0], width = out_prob.shape[1],
                        count=1, dtype=str(out_prob.dtype),
                        crs=rasterio.crs.CRS.from_epsg(4326),
                        transform=transform)
        new_tif.write(out_prob, 1)
        new_tif.close()

    return out_prob

# Given a dataset, return a tuple that describes the grid-square number
# of each row; also returns the x and y spacing of the grid
def get_grid(dataset, drll, djx, djy):

    xminr = dataset.Longitude.min() - drll
    xmaxr = dataset.Longitude.max() + drll
    yminr = dataset.Latitude.min() - drll
    ymaxr = dataset.Latitude.max() + drll
    
    xmin = xminr + djx
    ymin = yminr + djy
    xmax = xmaxr + djx 
    ymax = ymaxr + djy

    cvecx = np.arange(xmin, xmax, drll)
    cvecy = np.arange(ymin, ymax, drll)
    nx = len(cvecx)
    ny = len(cvecy) 

    colx = np.searchsorted(cvecx, dataset.Longitude)
    rowy = np.searchsorted(cvecy, dataset.Latitude)
    nxy = (nx + 1)*rowy + colx
    return nxy, cvecx, cvecy


def get_predictors_at_xy_from_mask(Features, prefix, feature_dict, radius, mask_in, transform):
    """ 
    Extract values from a raster at specific Longitude / Latitude coordinates 
    
    Parameters: 
        -Features: pandas dataframe describing Longitude/Latitude locations at which to extract values 
        -feature_dict: dictionary that associates feature name and pixel number
        -mask_in: array with bands corresponding to features in feature_dict from which values are pulled
        -transform: index raster object that relates Longitude/Latitude to pixel coordinates 

    Return:
        -Features dataframe with extracted values added
    """ 
    Focal_Features = Features.copy()
    # Get coordinates of each Focal_Feature in terms of pixel coordinates
    rows,cols = rasterio.transform.rowcol(transform, Focal_Features.Longitude, Focal_Features.Latitude)   
    # Extract features from convolutional mask if provided
    if mask_in is not None:
        for val in feature_dict.keys():
            ival = int(val)
            feature_name = prefix + feature_dict[val] + '.' + str(radius)
            Focal_Features.loc[:, feature_name] = mask_in[rows,cols,ival]
    return Focal_Features.copy()   

# Loads in rasters and interpolates them at lat/lon coordinates described by
# template_x, template_y
def get_sparse_block(template_x, template_y, site, model_features, focal_precip, prec_names, house, 
                    build_path, lc_path):
    nx = len(template_x)
    ny = len(template_y)
    print(house)
    all_preds = []
    for ii in range(len(model_features)):
        var_name = model_features[ii]    
        build_flag = ('Density' in var_name)
        sc_build_flag = build_flag & ('sc' in var_name)
        lc_flag = ('Frac' in var_name)
        prec_flag = var_name in prec_names
        
        ## --Satellite predictors
        if build_flag | lc_flag | sc_build_flag:
            
            varparts = var_name.split('.')
            layer = varparts[0]
            radius = int(varparts[1])
            
            ## -- Buildings
            if build_flag | sc_build_flag:
                
                if sc_build_flag:
                    layer = layer.split('sc_')[1]
                
                bandi = list(dens_building_dict.values()).index(layer)
                name = site + '_' + str(radius) + '_building_counts.tif'
                with rasterio.open(build_path + name) as src:
                    temp = resample_and_resize(src_rast = src, band = bandi, 
                                               template_x = template_x, template_y = template_y)
                    
                if sc_build_flag:
                    max_val = temp.max()
                    min_val = temp.min()
                    temp = (temp - min_val)/(max_val - min_val)             
                    temp[np.isnan(temp)] = 0
                    
            ## -- Landcover
            elif lc_flag:
                suff = 1
                if radius > 100:
                    suff = 10
                name = site.lower() + '_lc_fea_' + str(radius) + 'm_res_' + str(suff) + 'm.tif'
                bandi = list(landcover_dict.values()).index(layer)
                with rasterio.open(lc_path + name) as src:       
                    temp = resample_and_resize(src_rast = src, band = bandi, 
                                               template_x = template_x, template_y = template_y)                    
        
        ## -- Rainfall 
        if prec_flag:
            lag_entry = int(var_name.split('P')[1]) - 1
            with rasterio.open(focal_precip[lag_entry]) as src:
                temp = resample_and_resize(src_rast = src, band = 0, template_x = template_x, template_y = template_y)

        ## -- House
        if var_name == 'House':
            temp = house*np.ones(shape = (ny, nx)) ## House    
        
        ## -- Night
        if var_name == 'Night':
            temp = np.ones(shape = (ny, nx)) ## Night
        
                
        all_preds.append(temp[np.newaxis, :,:])   

    ## Convert predictors into pandas dataframe
    X_stack = np.concatenate(all_preds, axis = 0)

    vals = X_stack[:,:,:].ravel(order='F').reshape((-1,len(model_features)))
    df = pd.DataFrame(vals)
    df.columns = model_features
    
    return df 



def get_trainval_data(dataset, test_sites, response = 'Mna', weight = 'Trap.weight'): 
    mask_test = dataset.Site.isin(test_sites)
    test_dat = dataset.loc[mask_test,:]
    trainval_dat = dataset.loc[~mask_test,:]
    trainval_dat.reset_index(inplace = True)    
    X_trainval, y_trainval, W_trainval = (trainval_dat.loc[:,get_var_names()], 
                                          trainval_dat.loc[:,response],
                                          trainval_dat.loc[:,weight])    
    return(X_trainval, y_trainval, W_trainval, trainval_dat, test_dat)

def make_dir(path):
    try: 
        os.mkdir(path) 
    except OSError as error: 
        print(error)



def resample_and_resize(src_rast, band, template_x, template_y):

    src_array = src_rast.read(band + 1) ## GDAL convention -- band starts at one
    src_array = np.nan_to_num(src_array, nan = 0.0)
    src_shape = src_rast.shape  
    
    min_coords = rasterio.transform.xy(src_rast.transform, 0,0)
    max_coords = rasterio.transform.xy(src_rast.transform, src_shape[0] - 1, src_shape[1] - 1)

    xvals = np.linspace(min_coords[0], max_coords[0], src_shape[1])
    yvals = np.linspace(min_coords[1], max_coords[1], src_shape[0])
    f = interpolate.interp2d(xvals, yvals, src_array, kind="linear")

    zz = f(template_x, template_y)
    return zz

def resample_and_resize_numpy(src_array, band, template_x, template_y):
    src_array = np.nan_to_num(src_array, nan = 0.0)
    src_shape = src_rast.shape  
    
    min_coords = rasterio.transform.xy(src_rast.transform, 0,0)
    max_coords = rasterio.transform.xy(src_rast.transform, src_shape[0] - 1, src_shape[1] - 1)

    xvals = np.linspace(min_coords[0], max_coords[0], src_shape[1])
    yvals = np.linspace(min_coords[1], max_coords[1], src_shape[0])
    f = interpolate.interp2d(xvals, yvals, src_array, kind="linear")

    zz = f(template_x, template_y)
    return zz

def train_test_split(data, test_site):
    mask_test = (data.Site.isin(test_site))
    trainval = data.loc[~mask_test,:].copy()
    trainval.reset_index(inplace = True, drop = True)
    
    test = data.loc[mask_test,:]
    test.reset_index(inplace = True)
    
    return (trainval, test)

#https://stackoverflow.com/questions/8090229/resize-with-averaging-or-rebin-a-numpy-2d-array
def bin_ndarray(ndarray, new_shape, operation='sum'):
    """
    Bins an ndarray in all axes based on the target shape, by summing or
        averaging.

    Number of output dimensions must match number of input dimensions and 
        new axes must divide old ones.

    Example
    -------
    >>> m = np.arange(0,100,1).reshape((10,10))
    >>> n = bin_ndarray(m, new_shape=(5,5), operation='sum')
    >>> print(n)

    [[ 22  30  38  46  54]
     [102 110 118 126 134]
     [182 190 198 206 214]
     [262 270 278 286 294]
     [342 350 358 366 374]]

    """
    operation = operation.lower()
    if not operation in ['sum', 'mean']:
        raise ValueError("Operation not supported.")
    if ndarray.ndim != len(new_shape):
        raise ValueError("Shape mismatch: {} -> {}".format(ndarray.shape,
                                                           new_shape))
    compression_pairs = [(d, c//d) for d,c in zip(new_shape,
                                                  ndarray.shape)]
    flattened = [l for p in compression_pairs for l in p]
    ndarray = ndarray.reshape(flattened)
    for i in range(len(new_shape)):
        op = getattr(ndarray, operation)
        ndarray = op(-1*(i+1))
    return ndarray


def make_custom_eval(trainval_dat, train_index, val_index):
        
        ## ---

    pos = trainval_dat.loc[train_index,:].groupby(['Track_ID']).apply(lambda x: x.loc[x.Mna==1,'Trap.weight'].sum()).to_frame()
    pos.columns = ['pos']
    tot = trainval_dat.loc[train_index,:].groupby(['Track_ID']).apply(lambda x: x.loc[:,'Trap.weight'].sum()).to_frame()
    tot.columns = ['traps']
    train_track_info = tot.merge(pos, right_index = True, left_index = True)
    train_track_info.loc[:,'TS'] = train_track_info.pos / train_track_info.traps

    train_labels, levels =  pd.factorize(trainval_dat.loc[train_index, 'Track_ID'])
    #track_info.loc['nTrack_ID'] = labels
    trainval_dat.loc[train_index,'nTrack_ID'] = train_labels

    train_track_info = train_track_info.merge(trainval_dat.loc[train_index,['nTrack_ID', 'Track_ID']].drop_duplicates(), left_index = True, right_on = 'Track_ID', how = 'left')
    true_traps = np.bincount(train_labels, weights = train_wvals)    
    true_pos = np.bincount(train_labels, weights = train_wvals*train_yvals)
    true_ts = true_pos / true_traps

    pos = trainval_dat.loc[val_index,:].groupby(['Track_ID']).apply(lambda x: x.loc[x.Mna==1,'Trap.weight'].sum()).to_frame()
    pos.columns = ['pos']
    tot = trainval_dat.loc[val_index,:].groupby(['Track_ID']).apply(lambda x: x.loc[:,'Trap.weight'].sum()).to_frame()
    tot.columns = ['traps']
    val_track_info = tot.merge(pos, right_index = True, left_index = True)
    val_track_info.loc[:,'TS'] = val_track_info.pos / val_track_info.traps

    val_labels, levels =  pd.factorize(trainval_dat.loc[val_index, 'Track_ID'])
    #track_info.loc['nTrack_ID'] = labels
    trainval_dat.loc[val_index,'nTrack_ID'] = val_labels

    val_track_info = val_track_info.merge(trainval_dat.loc[val_index,['nTrack_ID', 'Track_ID']].drop_duplicates(), left_index = True, right_on = 'Track_ID', how = 'left')
    val_true_traps = np.bincount(val_labels, weights = val_wvals)    
    val_true_pos = np.bincount(val_labels, weights = val_wvals*val_yvals)
    val_true_ts = val_true_pos / val_true_traps

    pos = trainval_dat.groupby(['Track_ID']).apply(lambda x: x.loc[x.Mna==1,'Trap.weight'].sum()).to_frame()
    pos.columns = ['pos']
    tot = trainval_dat.groupby(['Track_ID']).apply(lambda x: x.loc[:,'Trap.weight'].sum()).to_frame()
    tot.columns = ['traps']
    all_track_info = tot.merge(pos, right_index = True, left_index = True)
    all_track_info.loc[:,'TS'] = all_track_info.pos / all_track_info.traps

    def custom_obj(y_true, preds):
        yvals = y_true
        wvals = trainval_dat.loc[train_index,'Trap.weight']

        pred_pos = np.bincount(train_labels, weights = preds*wvals)
        pred_ts = pred_pos / true_traps
        hess = np.zeros((len(pred_ts), len(pred_ts))) + 2.0
        x = 2*(pred_ts - true_ts)
        true_grad = 2*x

        hess = 0*preds + 2.0
        soft_grad = 2*(preds - yvals)
        for ii in range(len(x)):
            mask = train_labels==ii#track_info.nTrack_ID.values
            soft_grad[mask] = x[ii]*wvals[mask]

        return soft_grad, hess

    
    #     def custom_obj(y_true, preds):
    #         yvals = y_true
    #         wvals = trainval_dat.loc[train_index,'Trap.weight']

    #         pred_pos = np.bincount(train_labels, weights = preds*wvals)
    #         pred_ts = pred_pos / true_traps
    #         hess = np.zeros((len(pred_ts), len(pred_ts))) + 2.0
    #         x = 2*(pred_ts - true_ts)
    #         true_grad = 2*x  
    #         return true_grad, hess
    
    def custom_eval(preds, dmatrix):

        if len(val_index) == len(preds):
            yvals = dmatrix.get_label()
            wvals = trainval_dat.loc[val_index, 'Trap.weight']

            pred_pos = np.bincount(val_labels, weights = preds*wvals)
            pred_ts = pred_pos / val_true_traps

            mae = np.average(np.abs(pred_ts - val_true_ts), weights = val_true_traps)
            return 'val_mae', mae 
        elif len(train_index)==len(preds):
            yvals = dmatrix.get_label()
            wvals = trainval_dat.loc[train_index, 'Trap.weight']

            pred_pos = np.bincount(train_labels, weights = preds*wvals)
            pred_ts = pred_pos / true_traps

            mae = np.average(np.abs(pred_ts - true_ts), weights = true_traps)
            return 'train_mae', mae             

    return custom_obj, custom_eval




## Not sure I need these

# def toTimestamp(d):
#     return calendar.timegm(d.timetuple())


# def timeseq(start, end, step):
#     dt = start
#     result = []

#     while dt < end:
#         result.append(dt)#.strftime('%Y-%m-%d'))
#         dt += step
#     return result

#####################

# def get_grid(dataset, xminr, xmaxr, yminr, ymaxr, dr):
#     xmin = xminr - dr
#     ymin = yminr - dr
#     xmax = xmaxr + dr
#     ymax = ymaxr + dr
    
#     cvecx = np.arange(xmin, xmax, dr)
#     cvecy = np.arange(ymin, ymax, dr)
#     nx = len(cvecx)
#     ny = len(cvecy) 

#     colx = np.searchsorted(cvecx, dataset.Longitude)
#     rowy = np.searchsorted(cvecy, dataset.Latitude)
#     nxy = (nx + 1)*rowy + colx
#     return nxy



# def get_forecast(test_site, test_raster, focal_model, n_iter, 
#                  dr_grid, nuke_preds, pc_var_names,
#                  scaler, pca, sig_directions, 
#                 forecast_year, forecast_month, trap_dat, house = 1,
#                 save_path = 'Predictions/'):
#     mask_town = trap_dat.Site==test_site
#     forecast_date = datetime.date(year = forecast_year, month = forecast_month, day = 1)

#     focal_dates = [(forecast_date - relativedelta(months = n)) for n in range(1,13)]
#     focal_precip_dates = [(prec_data_path + 'chirps-v2.0.' + fd.strftime("%Y.%m.%d") + '.tif') for fd in focal_dates]
#     prec_names = [('P' + str(i)) for i in range(1,13)]

#     fn = image_path + 'BHR_' + test_raster + '.tif'
#     src = rasterio.open(fn)
#     template_rast = src.read()
#     xmin, ymin, xmax, ymax = src.bounds
#     src.close()

#     ## Limit spatial grid to trap data extent, if traps are present
#     focal_traps = ((trap_dat.loc[:,'Longitude'] > xmin) & (trap_dat.loc[:,'Longitude'] < xmax) &
#     (trap_dat.loc[:,'Latitude'] > ymin) & (trap_dat.loc[:,'Latitude'] < ymax))
#     if np.sum(focal_traps):
#         trainval_dat = trap_dat.loc[focal_traps,:].copy()
#         long_town = trainval_dat.Longitude
#         lat_town = trainval_dat.Latitude
#         min_long, max_long = long_town.min(), long_town.max()
#         min_lat, max_lat = lat_town.min(), lat_town.max()
#     else:
#         min_long, max_long = xmin, xmax
#         min_lat, max_lat = ymin, ymax    

#     ## Overlay a grid on the raster to guide where prediction takes place
#     dc = dr_grid/111139
#     cvecx = np.arange(min_long, max_long, dc)
#     cvecy = np.arange(min_lat, max_lat, dc)
#     nx = len(cvecx)
#     ny = len(cvecy)

#     cx = np.array([cvecx.min() - dc] + cvecx.tolist() + [cvecx.max() + dc])
#     cy = np.array([cvecy.min() - dc] + cvecy.tolist() + [cvecy.max() + dc])
#     cxmid = (cx[0:-1] + cx[1:])/2
#     cymid = (cy[0:-1] + cy[1:])/2

#     nx = len(cxmid)
#     ny = len(cymid)

#     focal_df = get_sparse_block(cxmid, cymid, test_site, pc_var_names, focal_precip_dates, prec_names,
#                                 house, build_path = build_path, lc_path = lc_path)
#     #print('a')
#     #print(focal_df.loc[:,pc_var_names].std())
#     focal_df = pc_transform(focal_df.loc[:,pc_var_names], scaler = scaler, pca = pca, 
#                               npcs = sig_directions)
#     #print(focal_df.head())
#     focal_df.columns = ['PC' + str(x) for x in range(focal_df.shape[1])]

#     ## Sever predictors
#     focal_df.loc[:,nuke_preds] = np.nan
    
#     if str(type(focal_model)) == "<class 'xgboost.sklearn.XGBClassifier'>":
#         result = focal_model.predict_proba(focal_df, iteration_range = (0,n_iter))[:,1]
#     elif str(type(focal_model)) == "<class 'xgboost.sklearn.XGBRegressor'>":
#         result = focal_model.predict(focal_df,  iteration_range = (0,n_iter))
#     else: 
#         print('error in model type')
        
#     out_prob = np.reshape(result, (ny,nx), order = 'F')
#     out_prob = np.flip(out_prob,0)

#     ## SAVE to RASTER
#     if save_path is not None: 
#         pred_path = model_path + save_path

#         try: 
#             os.mkdir(pred_path) 
#         except OSError as error: 
#             pass    

#         min_x = cvecx.min() - dc
#         max_y = cvecy.max() + dc
#         pixel_size = dc
#         transform = from_origin(min_x, max_y, dc, dc)

#         new_tif = rasterio.open(pred_path + test_raster + '.tif', 'w', driver='GTiff',
#                         height = out_prob.shape[0], width = out_prob.shape[1],
#                         count=1, dtype=str(out_prob.dtype),
#                         crs=rasterio.crs.CRS.from_epsg(4326),
#                         transform=transform)
#         new_tif.write(out_prob, 1)
#         new_tif.close()

#     return out_prob, cvecx, cvecy, dc

# def get_forecast1(test_site, test_raster, focal_model, 
#                  dr_grid, nuke_preds, pc_var_names,
#                  scaler, pca, sig_directions, 
#                 forecast_year, forecast_month, trap_dat, house = 1,
#                 save_path = 'Predictions/'):

#     mask_town = trap_dat.Site==test_site
#     forecast_date = datetime.date(year = forecast_year, month = forecast_month, day = 1)

#     focal_dates = [(forecast_date - relativedelta(months = n)) for n in range(1,13)]
#     focal_precip_dates = [(prec_data_path + 'chirps-v2.0.' + fd.strftime("%Y.%m.%d") + '.tif') for fd in focal_dates]
#     prec_names = [('P' + str(i)) for i in range(1,13)]

#     fn = image_path + 'BHR_' + test_raster + '.tif'
#     src = rasterio.open(fn)
#     template_rast = src.read()
#     xmin, ymin, xmax, ymax = src.bounds
#     src.close()

#     ## Limit spatial grid to trap data extent, if traps are present
#     focal_traps = ((trap_dat.loc[:,'Longitude'] > xmin) & (trap_dat.loc[:,'Longitude'] < xmax) &
#     (trap_dat.loc[:,'Latitude'] > ymin) & (trap_dat.loc[:,'Latitude'] < ymax))
#     if np.sum(focal_traps):
#         trainval_dat = trap_dat.loc[focal_traps,:].copy()
#         long_town = trainval_dat.Longitude
#         lat_town = trainval_dat.Latitude
#         min_long, max_long = long_town.min(), long_town.max()
#         min_lat, max_lat = lat_town.min(), lat_town.max()
#     else:
#         min_long, max_long = xmin, xmax
#         min_lat, max_lat = ymin, ymax    

#     ## Overlay a grid on the raster to guide where prediction takes place
#     dc = dr_grid/111139
#     cvecx = np.arange(min_long, max_long, dc)
#     cvecy = np.arange(min_lat, max_lat, dc)
#     nx = len(cvecx)
#     ny = len(cvecy)

#     cx = np.array([cvecx.min() - dc] + cvecx.tolist() + [cvecx.max() + dc])
#     cy = np.array([cvecy.min() - dc] + cvecy.tolist() + [cvecy.max() + dc])
#     cxmid = (cx[0:-1] + cx[1:])/2
#     cymid = (cy[0:-1] + cy[1:])/2


#     nx = len(cxmid)
#     ny = len(cymid)

#     focal_df = get_sparse_block(cxmid, cymid, test_site, pc_var_names, focal_precip_dates, prec_names,
#                                 house, build_path = build_path, lc_path = lc_path)

#     return(focal_df)