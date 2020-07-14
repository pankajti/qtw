from sklearn.datasets import load_boston
from sklearn.linear_model import LinearRegression


X, y = load_boston(return_X_y=True)
print(X.shape)

regression = LinearRegression()

