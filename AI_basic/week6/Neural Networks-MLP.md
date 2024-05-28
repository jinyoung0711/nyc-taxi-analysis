# Neural Networks-MLP

수업일: 2024/05/28
비고: 강의 정리
주차: 6주차 학습내용

## Neural Networks & Multi-Layer Perceptron

### Neural Networks

> Neural networks are computing systems vanguely inspired by the biological neural networks that constitute animal brains.
> 

![Untitled](Neural%20Networks-MLP/Untitled.png)

> Neural networks are function approximators that stack affine transformations followed by nonlinear transformations.
> 

![Untitled](Neural%20Networks-MLP/Untitled%201.png)

### Linear Neural Networks

![Untitled](Neural%20Networks-MLP/Untitled%202.png)

$$
\frac{\partial loss}{\partial w} = \frac{\partial}{\partial w}\frac{1}{N}\sum_{i=1}^N(y_i-\hat{y_i})^2
\\ = \frac{\partial}{\partial w}\frac{1}{N}\sum_{i=1}^N(y_i-w{x_i}-b)^2
\\= -\frac{1}{N}\sum_{i=1}^N-2(y_i-w{x_i}-b)x_i
$$

$$
\frac{\partial loss}{\partial b} = \frac{\partial}{\partial b}\frac{1}{N}\sum_{i=1}^N(y_i-\hat{y_i})^2
\\ = \frac{\partial}{\partial b}\frac{1}{N}\sum_{i=1}^N(y_i-w{x_i}-b)^2
\\= -\frac{1}{N}\sum_{i=1}^N-2(y_i-w{x_i}-b)
$$

$$
w \gets w-\eta \frac{\partial loss}{\partial w}
\\ b \gets b-\eta \frac{\partial loss}{\partial b}
$$

← 는 update eta는 stepsize

![Untitled](Neural%20Networks-MLP/Untitled%203.png)

### Activation function

![Untitled](Neural%20Networks-MLP/Untitled%204.png)

![Untitled](Neural%20Networks-MLP/Untitled%205.png)

![Untitled](Neural%20Networks-MLP/Untitled%206.png)