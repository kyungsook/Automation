import argparse
import numpy as np
import statistics as st
import sys

INPUT_MAX_LEN = 0
OUTPUT_MAX_LEN = 0
INPUT = 0
OUTPUT = 1
EMPTY = -100
TRUE = 1
FALSE = 0
TEST_SET_RATIO = 0.3
MIN_VALUE = None
MAX_VALUE = None

def read_file(filename):
    global INPUT_MAX_LEN, OUTPUT_MAX_LEN, INPUT, OUTPUT, TRUE, FALSE, MIN_VALUE, MAX_VALUE
    
    fp = open(filename, 'r')
    data_set = list()    
    stage = INPUT
    line = fp.readline().strip()

    while line:
        line = line.split(' ')
        if stage == INPUT and len(line) > INPUT_MAX_LEN:
            INPUT_MAX_LEN = len(line)
            #raise Exception('consider increasing INPUT_MAX_LEN >= {}.'.format(len(line)))
        if stage == OUTPUT and len(line) > OUTPUT_MAX_LEN:
            OUTPUT_MAX_LEN = len(line)
            #raise Exception('consider increasing OUTPUT_MAX_LEN >= {}.'.format(len(line)))    
        
        # convert a non-integer token to an integer        
        if len(line) == 1 and stage == OUTPUT:             
            if line[0].lower() == '\\null':
                line = []                       # \null is considered an empty list
            elif line[0].lower() == 'true':
                line = [TRUE]
            elif line[0].lower() == 'false':
                line = [FALSE]
            else:
                line = list(map(int, line))
        else:
            line = list(map(int, line))

        if len(line)>0:
            if MIN_VALUE==None:
                MIN_VALUE = min(line)
                MAX_VALUE = max(line)
            else:
                MIN_VALUE = min(MIN_VALUE,min(line))
                MAX_VALUE = max(MAX_VALUE,max(line))

        data_set.append(line)
        #if stage == OUTPUT:
        #    data_set.append([class_id])
        '''
        if stage == INPUT:            
            current_set = line
            current_set.extend([EMPTY] * max(0,INPUT_MAX_LEN - len(line)))
        else:            
            current_set.extend(line)
            current_set.extend([EMPTY] * max(0,OUTPUT_MAX_LEN - len(line)))
            data_set.append(current_set)
        '''
        
        stage = stage ^ 1  # xor 1 to flip
        line = fp.readline().strip()

    return data_set


def sigmoid(z):
    """
    Compute the sigmoid of z

    Arguments:
    z -- A scalar or numpy array of any size.

    Return:
    s -- sigmoid(z)
    """

    s = 1 / (1 + np.exp(-z))
    
    return s


def initialize_with_zeros(dim):
    """
    This function creates a vector of zeros of shape (dim, 1) for w and initializes b to 0.
    
    Argument:
    dim -- size of the w vector we want (or number of parameters in this case)
    
    Returns:
    w -- initialized vector of shape (dim, 1)
    b -- initialized scalar (corresponds to the bias)
    """
    
    w = np.zeros((dim,1))
    b = 0

    assert(w.shape == (dim, 1))
    assert(isinstance(b, float) or isinstance(b, int))
    
    return w, b


def propagate(w, b, X, Y):
    """
    Implement the cost function and its gradient for the propagation explained above

    Arguments:
    w -- weights, a numpy array of size (num_px * num_px * 3, 1)
    b -- bias, a scalar
    X -- data of size (num_px * num_px * 3, number of examples)
    Y -- true "label" vector (containing 0 if non-cat, 1 if cat) of size (1, number of examples)

    Return:
    cost -- negative log-likelihood cost for logistic regression
    dw -- gradient of the loss with respect to w, thus same shape as w
    db -- gradient of the loss with respect to b, thus same shape as b
    
    Tips:
    - Write your code step by step for the propagation. np.log(), np.dot()
    """
    
    m = X.shape[1]
    
    # FORWARD PROPAGATION (FROM X TO COST)
    A = sigmoid(np.dot(w.T,X) + b)                                    # compute activation
    cost = -(np.sum(Y * np.log(A)) + np.sum((1-Y) * np.log(1-A)))/m                                 # compute cost
    
    # BACKWARD PROPAGATION (TO FIND GRAD)
    dw = np.dot(X, (A-Y).T)/m
    db = np.sum(A-Y)/m

    assert(dw.shape == w.shape)
    assert(db.dtype == float)
    cost = np.squeeze(cost)
    assert(cost.shape == ())
    
    grads = {"dw": dw,
             "db": db}
    
    return grads, cost


def optimize(w, b, X, Y, num_iterations, learning_rate, print_cost = False):
    """
    This function optimizes w and b by running a gradient descent algorithm
    
    Arguments:
    w -- weights, a numpy array of size (num_px * num_px * 3, 1)
    b -- bias, a scalar
    X -- data of shape (num_px * num_px * 3, number of examples)
    Y -- true "label" vector (containing 0 if non-cat, 1 if cat), of shape (1, number of examples)
    num_iterations -- number of iterations of the optimization loop
    learning_rate -- learning rate of the gradient descent update rule
    print_cost -- True to print the loss every 100 steps
    
    Returns:
    params -- dictionary containing the weights w and bias b
    grads -- dictionary containing the gradients of the weights and bias with respect to the cost function
    costs -- list of all the costs computed during the optimization, this will be used to plot the learning curve.
    
    Tips:
    You basically need to write down two steps and iterate through them:
        1) Calculate the cost and the gradient for the current parameters. Use propagate().
        2) Update the parameters using gradient descent rule for w and b.
    """
    
    costs = []
    
    for i in range(num_iterations):    
        # Cost and gradient calculation
        grads, cost = propagate(w, b, X, Y)
        
        # Retrieve derivatives from grads
        dw = grads["dw"]
        db = grads["db"]
             
        # update rule  
        w = w - learning_rate * dw
        b = b - learning_rate * db       
        
        # Record the costs
        if i % 100 == 0:
            costs.append(cost)
        
        # Print the cost every 100 training iterations
        if print_cost and i % 100 == 0:
            print ("Cost after iteration %i: %f" %(i, cost))
    
    params = {"w": w,
              "b": b}
    
    grads = {"dw": dw,
             "db": db}
    
    return params, grads, costs   


def predict(w, b, X):
    '''
    Predict whether the label is 0 or 1 using learned logistic regression parameters (w, b)
    
    Arguments:
    w -- weights, a numpy array of size (num_px * num_px * 3, 1)
    b -- bias, a scalar
    X -- data of size (num_px * num_px * 3, number of examples)
    
    Returns:
    Y_prediction -- a numpy array (vector) containing all predictions (0/1) for the examples in X
    '''
    
    m = X.shape[1]
    Y_prediction = np.zeros((1,m))
    w = w.reshape(X.shape[0], 1)
    
    # Compute vector "A" predicting the probabilities of an i/o belonging to sortdec
    A = sigmoid(np.dot(w.T,X)+b)
    
    for i in range(A.shape[1]):        
        # Convert probabilities A[0,i] to actual predictions p[0,i]
        Y_prediction = np.where(A>0.5, 1, 0)
    
    assert(Y_prediction.shape == (1, m))
    
    return Y_prediction


def model(X_train, Y_train, X_test, Y_test, num_iterations = 2000, learning_rate = 0.5, print_cost = False):
    """
    Builds the logistic regression model by calling the function you've implemented previously
    
    Arguments:
    X_train -- training set represented by a numpy array of shape (num_px * num_px * 3, m_train)
    Y_train -- training labels represented by a numpy array (vector) of shape (1, m_train)
    X_test -- test set represented by a numpy array of shape (num_px * num_px * 3, m_test)
    Y_test -- test labels represented by a numpy array (vector) of shape (1, m_test)
    num_iterations -- hyperparameter representing the number of iterations to optimize the parameters
    learning_rate -- hyperparameter representing the learning rate used in the update rule of optimize()
    print_cost -- Set to true to print the cost every 100 iterations
    
    Returns:
    d -- dictionary containing information about the model.
    """
    
    # initialize parameters with zeros (≈ 1 line of code)
    w, b =  initialize_with_zeros(X_train.shape[0])

    # Gradient descent
    parameters, grads, costs = optimize(w, b, X_train, Y_train, num_iterations, learning_rate, print_cost)
    
    # Retrieve parameters w and b from dictionary "parameters"
    w = parameters["w"]
    b = parameters["b"]
    
    # Predict test/train set examples 
    Y_prediction_test = predict(w, b, X_test)
    Y_prediction_train = predict(w, b, X_train)
    
    # Print train/test Errors
    print("train accuracy: {} %".format(100 - np.mean(np.abs(Y_prediction_train - Y_train)) * 100))
    print("test accuracy: {} %".format(100 - np.mean(np.abs(Y_prediction_test - Y_test)) * 100))
    
    d = {"costs": costs,
         "Y_prediction_test": Y_prediction_test, 
         "Y_prediction_train" : Y_prediction_train, 
         "w" : w, 
         "b" : b,
         "learning_rate" : learning_rate,
         "num_iterations": num_iterations}
    
    return d


if __name__ == '__main__':
    # parse arguments - names of file to use as data set
    parser = argparse.ArgumentParser(description='get filenames to compare')
    parser.add_argument('filenames', action='store', nargs='*', help='file names to compare', default=['e-findmax.txt', 'e-sortdec.txt'])
    args = parser.parse_args()
    print(args)


    # read i/o files and determin min/max values and max # of i/o items    
    data_set_combined = list()    
    for idx, val in enumerate(args.filenames):
        data_set = read_file(val) 
        data_set_combined.append(data_set)
        
    '''
    print(data_set_combined)
    print(len(data_set), MIN_VALUE, MAX_VALUE, INPUT_MAX_LEN, OUTPUT_MAX_LEN)
    '''

    
    # extend data, such that every sample has same # of features and then divide data into training/test sets
    train_set_x_orig = list()
    train_set_y_orig = list()
    test_set_x_orig = list()
    test_set_y_orig = list()
    EMPTY = MIN_VALUE - (MAX_VALUE - MIN_VALUE)
    for class_id, data_set in enumerate(data_set_combined):
        #print(data_set)
        test_set_size = max(1, int(len(data_set)/2 * TEST_SET_RATIO))
        train_set_size = int(len(data_set)/2) - test_set_size
        assert(train_set_size > 0)
        #print(train_set_size, test_set_size)

        for idx, input_set in enumerate(data_set[0::2]):
            input_set.extend([EMPTY] * (INPUT_MAX_LEN - len(input_set)))            
            output_set = data_set[idx*2+1]
            output_set.extend([EMPTY] * (OUTPUT_MAX_LEN - len(output_set)))            
            input_set.extend(output_set)
            
            if idx < train_set_size:
                train_set_x_orig.append(input_set)
            else:
                test_set_x_orig.append(input_set)    

        train_set_y_orig.extend([class_id]*train_set_size)    # each file gets a unique class id    
        test_set_y_orig.extend([class_id]*test_set_size)
        
    '''
    print(train_set_x_orig)
    print(train_set_y_orig)

    print(test_set_x_orig)
    print(test_set_y_orig)

    sys.exit(0)
    '''

    m_train = len(train_set_x_orig)
    assert(len(train_set_x_orig) == len(train_set_y_orig))
    m_test = len(test_set_x_orig)
    assert(len(test_set_x_orig) == len(test_set_y_orig))
    print('# of training/test examples: {}/{}'.format(m_train, m_test))


    # convert lists into numpy matrices
    train_set_x = np.array(train_set_x_orig).T
    train_set_y = np.array(train_set_y_orig).reshape(1,m_train)
    test_set_x = np.array(test_set_x_orig).T
    test_set_y = np.array(test_set_y_orig).reshape(1,m_test)

    print("train_set_x shape:", train_set_x.shape)
    print("train_set_y shape:", train_set_y.shape)
    print("test_set_x shape:", test_set_x.shape)
    print("test_set_y shape:", test_set_y.shape)


    # center and standardize dataset
    '''
    - samples gather around 0 with small deviation,
        thus calculations required in learning/prediction will not result in extremely large/small values and lose precision
    - samples greater than mean become positive, while those less than mean become negative
        thus it is easy to learn parameters that differentiate the two cases
    '''
    mean = np.zeros(shape=(train_set_x.shape[0],1))
    stdev = np.full((train_set_x.shape[0],1),1.0)
    for i in range(train_set_x.shape[0]):
        # determine mean and stdev of non-empty values
        try:
            mean[i] = st.mean(x for x in train_set_x[i] if x!=EMPTY)
            stdev[i] = st.stdev(x for x in train_set_x[i] if x!=EMPTY)
        except st.StatisticsError: # all points are empty, when test cases have more i/o values than training cases
            mean[i] = EMPTY
            stdev[i] = 0
            
        if stdev[i]==0:
            stdev[i] = 1.0

    #print(train_set_x)

    train_set_x = (train_set_x - mean) / stdev
    test_set_x = (test_set_x - mean) / stdev

    '''
    print(mean)
    print(stdev)
    print(train_set_x)
    print(test_set_x)
    '''


    # create a prediction model by running gradient descent and by finding parameters w and b that fit the training samples
    d = model(train_set_x, train_set_y, test_set_x, test_set_y, num_iterations = 2000, learning_rate = 0.005, print_cost = True)

    #print(d['w'], d['b'])
