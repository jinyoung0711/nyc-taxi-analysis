# Modern CNN

수업일: 2024/06/04
비고: 강의 정리
주차: 7주차 학습내용

### AlexNet

> Alex Krizhevsky, Ilya Sutskever, and Geoffery Hinton, “ImageNet Classification with Deep Convolutional Neural Networks,” NIPS, 2012
> 

### ILSVRC

**I**mageNet **L**arge-**S**cale **V**isual **R**ecognition **C**hallenge

- Classfication / Detection / Localization / Segmentation
- 1,000 different categories
- Over 1 million images
- Training set : 456,567 images

![Untitled](Modern%20CNN/Untitled.png)

5 convolutional layers + 3 dense layers

- Key ideas
    - Rectified Linear Unit (ReLU) activation
    - GPI implementations (2GPU)
    - Local response normalization, Overlapping pooling
    - Data augmentation
    - Dropout

- Relu Activation
    
    $$
    R(x) = max(0, z)
    $$
    
- Preserves properties of linear models
- Easy to optimize with gradient descent
- Good generalization
- Overcome the vanishing gradient problem

### VGGNet

> Karen Simonyan, Andrew Zisserman, “Very Deep Convolutional Networks for Large-Scale Image Recognition,” ICLR, 2015
> 
- Increasing depth with 3x3 convolution filters (with stride 1)
- 1x1 convolution for fully connected layers
- Dropout (p=0.5)
- VGG16, VGG19 (layer의 개수에 따름)

Why 3x3 convolution ?

3x3 → 5x5 → 7x7 → 11x11으로 크기가 커짐으로써 얻는 이점

하나의 convolution filter가 찍었을 때 고려되는 input의 크기가 커짐. 

→ Receptive field

한개의 convolution feature map 값을 얻기 위해서 고려할 수 있는 input의 special dimension

![Untitled](Modern%20CNN/Untitled%201.png)

### GoogLeNet

> Christian et al. “Going Deeper with Convolutions”, CVPR, 2015
> 

![Untitled](Modern%20CNN/Untitled%202.png)

Network In Network (NiN 구조)

비슷하게 생긴 network가 반복되는 현상

- GoogLeNet won the ILSVRC at 2014
    - It combined network-in-network (NiN) with inception blocks.
- Inception blocks

![Untitled](Modern%20CNN/Untitled%203.png)

- What are the benefits of the inception blcok?
    - Reduce the number of parameter.
- How ?
    - Recall how the number of parameters is computed.
    - 1x1 convolution can be seen as channel-wise dimension reduction.

![Untitled](Modern%20CNN/Untitled%204.png)

![Untitled](Modern%20CNN/Untitled%205.png)

### ResNet

> Kaiming He, Xiangyu Zhang, Shaoquing Ren, Jian Sun, “Deep Residual Learning for Image Recognition,”, CVPR, 2015
> 
- Deeper neural networks are hard to train.
    - Overfitting is usually caused by an excessive number of parameters.
    - But, not in this case.

![Untitled](Modern%20CNN/Untitled%206.png)

- Add an identity map (skip connection)

![Untitled](Modern%20CNN/Untitled%207.png)

![Untitled](Modern%20CNN/Untitled%208.png)

- Add an identity map after nonlinear activations :
- Batch normalization after convolutions :

![Untitled](Modern%20CNN/Untitled%209.png)

- Bottleneck architecture

![Untitled](Modern%20CNN/Untitled%2010.png)

### DenseNet

> Gao Huang, Zhuang Liu, Laurens van der Maaten, Kilian Weinberger, “Densely Connected Convolutional Networks,” CVPR, 2017
> 
- DenseNet uses concatenation instead of addition.

![Untitled](Modern%20CNN/Untitled%2011.png)

![Untitled](Modern%20CNN/Untitled%2012.png)

- Dense Block
    - Each layer concatenates the feature maps of all preceding layers.
    - The number of channels increases geometrically.
- Transition Block
    - BatchNorm → 1x1 Conv → 2x2 AvgPooling
    - Dimension reduction
    
- Key takeaways
    - VGG : repeated 3x3 blocks
    - GoogLeNet : 1x1 convolution
    - RestNet : skip-connection
    - DenseNet : concatenation