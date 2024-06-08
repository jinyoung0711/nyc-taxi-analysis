# Generative Model

수업일: 2024/06/08
비고: 강의 정리
주차: 7주차 학습내용

## **Generative Models Part 1**

### Learning a Generative Model

![Untitled](Generative%20Model/Untitled.png)

- Bernoulli Distribution : 9biased) coin flip
    - D = {Heads, Tails}
    - Specify P(X = Heads) = p. Then P(X = Tails) = 1-p.
    - Write : X ~ Ber(p)

- Categorical distribution : (biased) m-sided dice
    - D = {1, …, m}
    - Specify
    
    $$
    P(Y=i) = p_i, such \; that \sum_{i=1}^mp_i = 1
    $$
    
    - Write : Y ~ Cat(p1, …, pm)
    
    Modeling an RGB joing distribution (of a single pixel)
    
    ![Untitled](Generative%20Model/Untitled%201.png)
    
    ![Untitled](Generative%20Model/Untitled%202.png)
    
    Structure Through Independence
    
    ![Untitled](Generative%20Model/Untitled%203.png)
    
    Independence를 가정하면 n개의 parameter만 필요
    
    ***Conditional Independence***
    
    Three important rules
    
    - Chain rule :
        
        $$
        p(x_1, ..., x_n) = p(x_1)p(x_2|x_1)p(x_3|x_1,x_2)...p(x_n|x_1,...,x_{n-1})
        $$
        
    - Bayes’ rule :
        
        $$
        p(x|y) = \frac{p(x,y)}{p(y)}= \frac{p(y|x)p(x)}{p(y)}
        $$
        
    - Conditional independence :
        
        $$
        if \; x \bot y|z, \; then \; p(x|y,z) = p(x|z)
        $$
        

Using the chain rule,

$$
p(x_1, ..., x_n) = p(x_1)p(x_2|x_1)p(x_3|x_1,x_2)...p(x_n|x_1,...,x_{n-1})
$$

- How many parameters?
    - 1 parameter
        
        $$
        p(x_1)
        $$
        
    - 2 parameters (one per p(x_2|x_1 = 0) and one per p(x_2|x_1 = 1)
    
    $$
    p(x_2|x_1)
    $$
    
    - 4 parameters
    
    $$
    p(x_3|x_1, x_2)
    $$
    
    Hence, 
    
    $$
    1 + 2 + 2^2 + ... + 2^{n-1} = 2^{n-1}
    $$
    
    Now, suppose Markov assuption
    
    $$
    X_{i+1} \bot X_1, ..., X_{i-1}|X_i
    $$
    
    $$
    p(x_1, ..., x_n) = p(x_1)p(x_2|x_1)p(x_3|x_1,x_2)...p(x_n|x_1,...,x_{n-1})
    $$
    
    - How many parameters ?
        
        2n-1
        
    
    Hence, by leveraging the Markov assuption, we get exponential reduction on the number of parameters.
    
    **Auto-regressive models** leverage this conditional independency.
    
    **Auto-regressive Model**
    
    ![Untitled](Generative%20Model/Untitled%204.png)
    
    **NADE : Neural Autoregressive Density Estimator**
    
    ![Untitled](Generative%20Model/Untitled%205.png)
    
    NADE is an explicit model that can compute the density of the given inputs.
    
    How can we compute the density of the given image?
    
    - Suppose we have a binary image with 784 binary pixels, {x_1, x_2, …, x_784}.
    - Then, the joint probability is computed by
    
    $$
    p(x_1, ...,x_{784}) = p(x_1)p(x_2|x_1)...p(x_{784}|x_{1:783})
    $$
    
    where each conditional probability p(x_i | x_{1 : i-1} is computed independently.
    
    In case of modeling continuous random variables, a mixture of Gaussian can be used.
    

Pixel RNN

![Untitled](Generative%20Model/Untitled%206.png)

![Untitled](Generative%20Model/Untitled%207.png)