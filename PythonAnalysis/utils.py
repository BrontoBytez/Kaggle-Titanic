import pandas as pd

def print_version(module_name, module):
    print(module_name + ': ', module.__version__)  
    
def get_data(filetype, path):
    if filetype == 'csv':
        return pd.read_csv(path)
    else:
        print('unsupported file type')


def partition_data(data, field, cut_point):
    testBatch = pd.DataFrame()
    trainingBatch = pd.DataFrame()
    
    trainingMask = data[field] % 10 < cut_point
    testingMask = data[field] % 10 > cut_point
    
    trainingData = data[trainingMask]
    testingData = data[testingMask]
    
    return trainingData, testingData