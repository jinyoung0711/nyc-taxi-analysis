# Sequential Model

수업일: 2024/06/07
비고: 강의 정리
주차: 7주차 학습내용

### Sequential Model

![Untitled](Sequential%20Model/Untitled.png)

![Untitled](Sequential%20Model/Untitled%201.png)

![Untitled](Sequential%20Model/Untitled%202.png)

![Untitled](Sequential%20Model/Untitled%203.png)

Recurrent Neural Network

![Untitled](Sequential%20Model/Untitled%204.png)

![Untitled](Sequential%20Model/Untitled%205.png)

![Untitled](Sequential%20Model/Untitled%206.png)

![Untitled](Sequential%20Model/Untitled%207.png)

Long Short  Term Memory

![Untitled](Sequential%20Model/Untitled%208.png)

![Untitled](Sequential%20Model/Untitled%209.png)

![Untitled](Sequential%20Model/Untitled%2010.png)

![Untitled](Sequential%20Model/Untitled%2011.png)

![Untitled](Sequential%20Model/Untitled%2012.png)

**Transformer**

> Attention is All You Need, NIPS, 2017
> 

What makes sequential modeling a hard problem to handle?

![Untitled](Sequential%20Model/Untitled%2013.png)

![Untitled](Sequential%20Model/Untitled%2014.png)

Transformer is the first sequence transduction model based entirely on attention.

![Untitled](Sequential%20Model/Untitled%2015.png)

![Untitled](Sequential%20Model/Untitled%2016.png)

![Untitled](Sequential%20Model/Untitled%2017.png)

First, we represent each word with some embedding vectors.

![Untitled](Sequential%20Model/Untitled%2018.png)

Then, Transformer encodes each word to feature vectors with Self-Attention.

![Untitled](Sequential%20Model/Untitled%2019.png)

Suppose we are encoding two words:

- Thinking and Machines.

Self-Attention at a high level

The animal didn’t cross the street because it was too tired.

![Untitled](Sequential%20Model/Untitled%2020.png)

![Untitled](Sequential%20Model/Untitled%2021.png)

![Untitled](Sequential%20Model/Untitled%2022.png)

![Untitled](Sequential%20Model/Untitled%2023.png)

Calculating Q, K, and V from X in a matrix form

![Untitled](Sequential%20Model/Untitled%2024.png)

![Untitled](Sequential%20Model/Untitled%2025.png)

Multi-headed attention (MHA) allows Transformer to focus on different positions.

![Untitled](Sequential%20Model/Untitled%2026.png)

if eight heads are used, we end up getting eight different sets of encoded vectors (attention heads).

![Untitled](Sequential%20Model/Untitled%2027.png)

We siply pass them through additional (learnable) linear map.

![Untitled](Sequential%20Model/Untitled%2028.png)

![Untitled](Sequential%20Model/Untitled%2029.png)

Why do we need positional encoding?

![Untitled](Sequential%20Model/Untitled%2030.png)

This is the case for 4-dimensional encoding.

![Untitled](Sequential%20Model/Untitled%2031.png)

This is the case for 512-dimensional encoding.

![Untitled](Sequential%20Model/Untitled%2032.png)

Transformer transfer key (K) and value (V) of the topmost encoder to the decoder.

The output sequence is generated in an autoregressive manner.

In the decoder, the self-attention layer is only allowed to attend to earlier positions in the output sequence which is done by masking future positions before the softmax step.

**Vision Transformer**

![Untitled](Sequential%20Model/Untitled%2033.png)