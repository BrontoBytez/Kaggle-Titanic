from sklearn.metrics import confusion_matrix, accuracy_score, roc_curve, roc_auc_score, auc
import matplotlib.pyplot as plt
import numpy as np

def print_model_information(model_name, formula, shape):
    print(model_name, formula,shape)
    
def model_accuracy(model_name, predictions, actual):
    print(model_name, accuracy_score(actual, predictions))
    
def print_roc_curve(model_name, predictions, actual):
    print('ROC Curve for model: ', model_name)
    false_positive_rate, true_positive_rate, thresholds = roc_curve(actual, predictions)
    roc_auc = auc(false_positive_rate, true_positive_rate)    
    print('ROC Area Under Curve Score: ', roc_auc_score(actual, predictions))
    plt.title('Receiver Operating Characteristic')
    plt.plot(false_positive_rate, true_positive_rate, 'b', label='AUC = %0.2f'% roc_auc)
    plt.legend(loc='lower right')
    plt.plot([0,1],[0,1],'r--')
    plt.xlim([-0.1,1.2])
    plt.ylim([-0.1,1.2])
    plt.ylabel('True Positive Rate')
    plt.xlabel('False Positive Rate')
    plt.show()
    
def print_confusion_matrix(model_name, predictions, actual, targetNames, title='Confusion Matrix', cmap=plt.cm.Blues):
    matrix = confusion_matrix(actual, predictions)
    plt.imshow(matrix, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(2)
    plt.xticks(tick_marks, targetNames, rotation=45)
    plt.yticks(tick_marks, targetNames)
    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label') 
    plt.show()