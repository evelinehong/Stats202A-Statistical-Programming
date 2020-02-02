from stats202a.layers import *


def fc_relu_forward(x, w, b):
    """
    Convenience layer that performs an affine transform followed by a ReLU

    Inputs:
    - x: Input to the affine layer
    - w, b: Weights for the affine layer

    Returns a tuple of:
    - out: Output from the ReLU
    - cache: Object to give to the backward pass
    """
    out, cache = None, None
    
    ###########################################################################
    # TODO: Implement fc-relu forward pass.                                   #
    ###########################################################################
    
    x2 = np.reshape(x, (x.shape[0],-1))
    out = x2.dot(w) + b
    out = np.maximum(0,out)
    cache = (x, w, b, out)
    ############################################################################
    #                             END OF YOUR CODE                             #
    ############################################################################
    return out, cache


def fc_relu_backward(dout, cache):
    """
    Backward pass for the affine-relu conveniences layer
    """
    dx, dw, db = None, None, None
    x, w, b, out = cache
    dout = (out > 0) * dout

    x2 = np.reshape(x, (x.shape[0],-1))
    dw = x2.T.dot(dout)
    db = np.sum(dout,axis=0)
    dx = dout.dot(w.T)
    dx = np.reshape(dx, (x.shape))

    ###########################################################################
    # TODO: Implement the fc-relu backward pass.                              #
    ###########################################################################
    
    ############################################################################
    #                             END OF YOUR CODE                             #
    ############################################################################
    return dx, dw, db
