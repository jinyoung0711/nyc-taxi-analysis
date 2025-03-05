# NYC Taxi Analysis

## 🚖 프로젝트 개요

뉴욕시 택시 데이터를 활용하여 다양한 데이터 분석을 수행하는 프로젝트입니다. 이 프로젝트에서는 R과 Hadoop을 이용하여 대용량 데이터를 처리하고, 택시 운행 패턴 및 요금과 관련된 인사이트를 도출합니다.

## 📊 데이터 설명

- **출처:** [NYC Taxi & Limousine Commission 공식 데이터](https://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml)
- **데이터 범위:** 특정 기간 동안의 택시 운행 기록
- **데이터 형식:** CSV 파일
- **주요 변수:**
  - `pickup_datetime`: 승차 시간
  - `dropoff_datetime`: 하차 시간
  - `passenger_count`: 승객 수
  - `trip_distance`: 이동 거리
  - `fare_amount`: 요금
  - `payment_type`: 결제 방식

## 🎯 분석 목표

✔ NYC 택시 운행 패턴 분석\
✔ 요금과 거리 간의 관계 탐색\
✔ 승객 수에 따른 이동 거리 및 요금 변화\
✔ 회귀 분석을 통한 요금 예측 모델 개발\
✔ 택시 기사의 효율적인 영업을 위한 데이터 기반 솔루션 제공

## 🛠 사용 기술

- **프로그래밍 언어:** R
- **데이터 처리:** Hadoop, MapReduce
- **시각화:** ggplot2, RgoogleMaps, Base R graphics
- **분석 기법:** 탐색적 데이터 분석(EDA), 회귀 분석, 데이터 전처리, K-Fold 검증, Stepwise, Best Subset

## 🏗 데이터 전처리 및 파생 변수 생성

### 📌 결측치 처리

- `drop_latitude`, `drop_longitude` 변수에서 188개의 NA 값이 발견됨
- NA 값이 있는 행은 CMT 회사 소속이며, 승차/하차 시간이 동일하여 삭제 결정

### 📌 이상치 제거

- `trip_distance`: 뉴욕주 최장 횡단 거리(60\~100마일)를 초과하는 경우 이상치로 제거
- `passenger_count`: 0명 또는 비정상적으로 높은 값(7, 9, 208명) 제거
- `trip_time_in_secs`: 4,000,000초(약 46일) 이상인 비정상적인 운행 시간 제거
- `rate_code`: 공항 및 협상 운임(2\~5) 제외, Standard 운임(1)만 분석에 포함
- `total_amount`: 음수 값 및 극단적인 최대값 제거
- `longitude`, `latitude`: 뉴욕주 위도/경도 범위를 벗어난 데이터 제거

### 📌 파생 변수 생성

- `pickup_day`, `dropoff_day`: 요일을 나타내는 변수
- `pickup_time_bin`, `dropoff_time_bin`: 4시간 단위로 나눈 시간대 변수 (러시아워 포함)
- `pickup1`\~`pickup6`: 시간대별 분류된 승차 데이터

## 🗺 Mapping 분석 및 시각화

- 주요 관광지(Hot Place) 및 호텔(Hotel Place) 위치를 시각화하여 택시 기사들의 효율적인 이동 경로 분석
- 수익 상위 0.1% 기사들의 `pickup` 및 `dropoff` 위치 비교 분석
- John F. Kennedy 공항과 LaGuardia 공항은 시간대와 관계없이 주요 승차 지점으로 확인됨
- `G2 (04h-08h)`: Manhattan 지역 내 관광지/호텔 밀집도가 낮음
- `G5-G6 (16h-24h)`: Manhattan 외곽 지역(ST. GEORGE, BROOKLYN)까지 이동 범위 확대

## 📂 프로젝트 구조

```
nyc-taxi-analysis/
├── scripts/             # 분석 및 모델링 코드
│   ├── EDA.R            # 탐색적 데이터 분석
│   ├── preprocessing.R  # 데이터 전처리
│   ├── regression.R     # 회귀 분석
│   ├── mapping.R        # 데이터 매핑
│   └── full_code.R      # 전체 코드
├── docs/                # 프로젝트 문서
│   ├── meeting-notes/   # 회의록 정리
│   │   ├── MEET1.md
│   │   ├── MEET2.md
│   │   ├── MEET3.md
│   │   ├── MEET4.md
│   │   └── MEET5.md
│   ├── MEET2_images     # 회의록 이미지
│   ├── MEET3_images     # 회의록 이미지
│   ├── MEET4_images     # 회의록 이미지
│   └── MEET5_images     # 회의록 이미지
├── .gitignore           # Git 무시 파일 목록
├── nyc-taxi-analysis.Rproj # R 프로젝트 파일
└── README.md            # 프로젝트 개요 및 실행 방법
```

## 🚀 실행 방법

1. **데이터 준비:**
   - 참고자료에 있는 원본 데이터를 다운로드하여 저장합니다.
2. **환경 설정:**
   - R 및 필요한 패키지(ggplot2, RgoogleMaps 등)를 설치합니다.
   - Hadoop 환경을 설정합니다.
3. **분석 실행:**
   - R 환경에서 `scripts/full_code.R` 파일을 실행하여 전체 분석을 수행합니다.

## 📌 참고 자료

- [NYC Taxi & Limousine Commission 공식 데이터](https://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml)

