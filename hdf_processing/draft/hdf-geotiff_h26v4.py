import os
import subprocess as sub
#code source: https://gis.stackexchange.com/a/276228
#code source: https://stackoverflow.com/questions/24306205/file-not-found-error-when-launching-a-subprocess-containing-piped-commands
# Using Python 2.7

os.chdir('D:\\Auguste\\UChicago\\2018-2019\\2019winter\\GIS2\\project\\h27v4')

path_hdf = r'D:\\Auguste\\UChicago\\2018-2019\\2019winter\\GIS2\\project\\h27v4'
path_tif = r'D:\\Auguste\\UChicago\\2018-2019\\2019winter\\GIS2\\project\\tif_h27v4'
files = [os.path.join(path_hdf, f) for f in os.listdir(path_hdf) if f.endswith('.hdf')]


for f in files:
    hdfname = os.path.basename(f)[:-4]
    tifname = os.path.join(path_tif, hdfname + '.tif')
    
    my_call = [r'C:\\Program Files\\QGIS 3.6\\OSGeo4W.bat',
    'gdal_translate', 
    '-of',
    'GTiff',
    '-co',
    'TFW=YES',
    'HDF4_EOS:EOS_GRID:"' + f + '":grid1km:Optical_Depth_047',
    tifname
    ]

    print 'Using gdal_translate on {0} to convert it to {1}'.format(f, tifname)
    p = sub.Popen(my_call, stdout=sub.PIPE, stderr=sub.PIPE, shell=True)
    stdout, stderr = p.communicate()
    if p.returncode != 0:
        print stdout
        print stderr

