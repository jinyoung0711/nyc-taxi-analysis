# Computer Vision Applications

수업일: 2024/06/05
주차: 7주차 학습내용

### Semantic Segmentation

![Untitled](Computer%20Vision%20Applications/Untitled.png)

image의 모든 pixel이 어떤 label에 속하는지를 알고 싶음. 

dense classification, per pixel classification이라고도 불림.

자율주행, 운전보조장치 등에서 자주 사용.

**Fully Convolutional Network**

![Untitled](Computer%20Vision%20Applications/Untitled%201.png)

This is how an ordinary CNN looks like.

![Untitled](Computer%20Vision%20Applications/Untitled%202.png)

This is a fully convolutional network.

dense layer을 제거하는 과정 → convolutinalization

장점이 dense layer가 없어진 것.

![Untitled](Computer%20Vision%20Applications/Untitled%203.png)

![Untitled](Computer%20Vision%20Applications/Untitled%204.png)

- Transforming fully connected layers into convolution layers enables a classification net to output a heatmap.

![Untitled](Computer%20Vision%20Applications/Untitled%205.png)

- While FCN can run with inputs of any size, the output dimensions are typically reduced by subsampling. (input size, output size = special dimensions)
- So we need a way to connect the coarse output to the dense pixels.

---

### Deconvolution (conv transpose)

![Untitled](Computer%20Vision%20Applications/Untitled%206.png)

![제목 없는 동영상 - Clipchamp로 제작.gif](Computer%20Vision%20Applications/%25EC%25A0%259C%25EB%25AA%25A9_%25EC%2597%2586%25EB%258A%2594_%25EB%258F%2599%25EC%2598%2581%25EC%2583%2581_-_Clipchamp%25EB%25A1%259C_%25EC%25A0%259C%25EC%259E%2591.gif)

---

### Detection

**R-CNN**

![Untitled](Computer%20Vision%20Applications/Untitled%207.png)

**SPPNet**

![Untitled](Computer%20Vision%20Applications/Untitled%208.png)

**Fast R-CNN**

![Untitled](Computer%20Vision%20Applications/Untitled%209.png)

1. Takes an input and a set of bounding boxes.
2. Generated convolutional feature map
3. For each region, get a fixed length feature from ROI pooling
4. Two outputs: class and bounding-box regressor.

Faster R-CNN

![Untitled](Computer%20Vision%20Applications/Untitled%2010.png)

- Raster R-CNN = Region Proposal Network + Fast R-CNN

![Untitled](Computer%20Vision%20Applications/Untitled%2011.png)

Region Proposal Network (RPN)의 역할

image에서 특정 patch(영역)이 boundary box로의 의미가 있는지를 찾아줌.

![Untitled](Computer%20Vision%20Applications/Untitled%2012.png)

YOLO

- YOLO (v1) is an extremly fast object detection algorithm.
    - baseline :  45fps / smaller version : 155fps
- It simultaneously predicets multiple bounding boxes and class probabilities.
    - No explicit bounding box sampling (compared with Fast R-CNN)
    
    ![Untitled](Computer%20Vision%20Applications/Untitled%2013.png)
    
    ![Untitled](Computer%20Vision%20Applications/Untitled%2014.png)
    
    ![Untitled](Computer%20Vision%20Applications/Untitled%2015.png)
    
    ![Untitled](Computer%20Vision%20Applications/Untitled%2016.png)