# Interface of train function

## Input

- x
- y 
- other parameters (like `learning_rate`)

## Output 

Object of class "scrAMbodeL" with a subclass corresponding to implemented algorithm: "IWLS", "GD" "SGD"

# Interface of predict function

This function is a method. Generic: `predict`

## Input 

- object 
- newdata

## Output

data frame with columns : "prediction", "probability"

# Other common things to do:

- `loglikelihood`
- measures (inc. R2)
- some stopping rule
