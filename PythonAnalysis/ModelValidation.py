from sklearn.metrics import confusion_matrix, accuracy_score, roc_curve, roc_auc_score, auc
import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import sem

def print_model_information(model_name, formula, shape):
    print(model_name, formula,shape)
    
def model_accuracy(model_name, predictions, actual):
    print(model_name, accuracy_score(actual, predictions))
    
def print_roc_curve(model_name, predictions, actual):
    print('ROC Curve for model: ', model_name)
    false_positive_rate, true_positive_rate, thresholds = roc_curve(actual, predictions)
    true_negative_rate = 1 - false_positive_rate
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
    print('Specificity (True Negative Rate): ', true_negative_rate)
    print('Sensitivity (True Positive Rate): ', true_positive_rate)
    
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
    
def bootstrap_ROC():
    y_pred = results['randomForest']
    y_true = randomForestTestingY['Survived']
    print("Original ROC area: {:0.3f}".format(roc_auc_score(y_true, y_pred)))
    n_bootstraps = 1000
    rng_seed = 42  # control reproducibility
    bootstrapped_scores = []
    
    rng = np.random.RandomState(rng_seed)
    for i in range(n_bootstraps):
        # bootstrap by sampling with replacement on the prediction indices
        indices = rng.random_integers(0, len(y_pred) - 1, len(y_pred))
        if len(np.unique(y_true[indices])) < 2:
            # We need at least one positive and one negative sample for ROC AUC
            # to be defined: reject the sample
            continue
        score = roc_auc_score(y_true, y_pred)
        bootstrapped_scores.append(score)
        print("Bootstrap #{} ROC area: {:0.3f}".format(i + 1, score))

