# Gradient Descent Methods

수업일: 2024/05/30
비고: 강의 정리
주차: 6주차 학습내용

- Stochastic gradient desscent
    - Update with the gradient computed from a single sample.

- Mini-batch gradient escent
    - Update with the gradient computed from a subset of data.

- Batch gradient descent
    - Update with the gradient computed from the whole data.
    

### Batch-size Matters

> It has been observed in practice that when using a larger batch there is a degradation in the quality of the model, as measured bt its ability to generalize.
> 

> We … present numerical evidence that supports the view that large batch methods tend to converge to **sharp minimizers** of the training and testing functions. In contrast, small-batch methods consistently converge to **flat minimizers**… this is due to the inherent noise in the gradient estimation.
> 

![Untitled](Gradient%20Descent%20Methods/Untitled.png)

> On large-batch Training for Deep Learning: Generalization Gap and Sharp Minima, 2017
> 

Flat Minimum의 특징

Testing Function과는 거리가 조금 멀어져도 Testing Function에서 적당한 낮은 값이 나온다.

 → Flat Minimum이 Training Function에서 잘 나오면 Testing Function에서도 잘 나온다.

Batch size가 낮아지면 일반적으로 Generalize Performance가 좋아진다.

### Gradient Descent Methods

- Stochastic gradient descent
- Momentum
- Nestrerov accelerated gradient
- Adagrad
- Adadelta
- RMSprop
- Adam

**(Stochastic) Gradient descent**

$$
W_{t+1} \gets W_t - \eta g_t

$$

eta is Learning rate. g_t is Gradient.

learning rate를 적절히 선정하는 것이 어렵다.

**Momentum**

$$
\alpha_{t+1} \gets \beta\alpha_t + g_t \\ W_{t+1} \gets W_t - \eta\alpha_{t+1}
$$

beta is  momentum. alpha_t+1 is accumulation.

한번 흘러가기 시작한 gradient를 어느정도 유지한다는 특징을 가짐.

**Nesterov Accelerated Gradient (NAG)**

$$
\alpha_{t+1} \gets \beta\alpha_t + \nabla \pounds(W_t-\eta\beta\alpha_t) \\ W_{t+1} \gets W_t - \eta\alpha_{t+1}
$$

beta.alpha_t next formula is Lookahead gradient. 

![Untitled](Gradient%20Descent%20Methods/Untitled%201.png)

NAG의 장점은 local minimum(봉우리)에 좀 더 빠르게 도달할 수 있음. 

**Adagrad**

**Adagrad** adapts the learning rate, performing larger updates for infrequent and smaller updates for frequent parameters.

$$
W_{t+1} = W_t - \frac{\eta}{\sqrt{G_t + \epsilon}}g_t
$$

G_t는 Sum of gradient squares를 의미. epsilon은 for numerical stability (0이 되는 것을 방지).

neural network parameter가 지금까지 얼마나 변해왔는지, 안변해왔는지를 보기 시작.

많이 변한 parameter에 대해서는 적게 변화시키고, 안변한 parameter의 경우 많이 변화.

G가 점점 커지기 때문에 무한대가 infinity가 됨. → 시간이 지날 수록 학습이 멈추는 경향

**Adadelta**

**Adadelta** extends **adagrad** to reduce its monotonically decreasing the learning rate by restricting the accumulation window.

$$
G_t = \gamma G_{t-1} + (1 - \gamma)g_t^2 \\ W_{t+1} = W_t - \frac{\sqrt{H_{t-1}+\epsilon}}{G_t + \epsilon}g_t \\ H_t = \gamma H_{t-1} + (1-\gamma)(\Delta W_t)^2
$$

There is no learning rate in Adadelta. → 사실상 많이 사용되지 않음.

G_t is EMA of gradient squares. H_t is EMA of difference squares. 

**RMSprop**

**RMSprop** is an unpublished, adaptive learning rate method proposed bt Geoff Hinton in his lecture.

$$
G_t = \gamma G_{t-1} + (1 - \gamma)g_t^2 \\ W_{t+1} = W_t - \frac{\eta}{\sqrt{G_t + \epsilon}}g_t
$$

G_t is EMA of gradient squares. eta is stepsize.

**Adam**

Adaptive Moment Estimation (Adam) leverages both past gradients and squared gradients.

$$
m_t = \beta_1m_{t=1} + e(1-\beta_1)g_t \\ v_t = \beta_2v_{t-1} + (1-\beta_2)g_t^2 \\ W_{t+1} = W_t - \frac{\eta}{\sqrt{v_t + \epsilon}}\frac{\sqrt{1-\beta_2^t}}{1-\beta_1^t}m_t
$$

m_t is Momentum. v_t is EMA of gradient squares.

epsilon parameter를 잘 바꿔주는 것이 practical하게 중요하다.

Adam effectively combines momentum with adaptive learning rate approach.