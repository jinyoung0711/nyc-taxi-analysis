# MEET 1

날짜: 2022년 11월 20일 오후 9:00 (GMT+9)
분류: 정기회의
여부: 완료
참여자: 천진영, ***, ***, ***

- 진영
    - 변수 정리
        - Vender ID : A code indicating the TPEP provider that provided the record. →
        - pickup datetime : The date and time when the meter was engaged. → 미터기가 작동된 날짜 및 시간을 의미
        - dropoff datetime : The date and time when the meter was disengaged. → 미터기가 해제된 날짜 및 시간을 의미
        - Passenger count : The number of passengers in the vehicle. This is a driver-entered value. → 차량의 승객 수를 의미
        - Trip distance : The elapsed trip distance in miles reported by the taximeter. → 택시미터에 의한 주행 거리(마일기준)를 의미
        - Pickup longitude : Longitude where the meter was engaged. → 미터기가 활성화된 경도를 의미
        - Pickup latitude : Latitude where the meter was engaged. → 미터기가 활성화된 위도를 의미
        - RateCodeID : The final rate code in effect at the end of the trip. → 여행이 끝날 때 적용되는 최종 요금 코드 → 각 값들은 출발지를 의미
            - 1 = standard rate
            - 2 = JFK (JFK airport에서 출발)
            - 3 = Newark
            - 4 = Nassau or Westchester
            - 5 = Negotiated fare
            - 6 = Group ride
        - Store and fwd flag :
        - Dropoff longitude : Longitude where the meter was disengaged. → 미터기가 해제된 경도를 의미
        - Dropoff latitude : Latitude where the meter was disengaged. → 미터기가 해제된 위도를 의미
        - Payment type : A numeric code signifying how the passenger paid for the trip. → 지불방식을 의미
            - 1 = credit card
            - 2 = cash
            - 3 = no charge (외상을 의미하는건가?)
            - 4 = dispute (무슨 뜻이지?)
            - 5 = unknown
            - 6 = voided trip (무슨 뜻이지?)
        - Fare amount : The time-and-distance fare calculated by the meter. → 미터기로 계산한 시간 및 거리 요금
        
        - Extra : Miscellaneous extras and surcharges. Currently, this only includes the $0.50 and $1 rush hour and overnight charges. → 기타 추가 요금 (0.5달러의 러시아워와 1달러의 야간 요금만 포함)
        - MTA tax : $0.50 MTA tax that is automatically triggered based on the metered rate in use. → 미터링된 사용률에 따라 자동으로 트리거되는 MTA 세금
        - Improvement surcharge : $0.30 improvement surcharge assessed trips at the flag drop. The improvement surcharge began being levied in 2015. → 세금, 추가 요금 등을 의미하는듯
        - Tip amount : Tip amount – This field is automatically populated for credit card tips. Cash tips are not included. → 팁 금액 (신용 카드 팁에 자동으로 입력, 현금 팁은 포함X)
        - Tolls amount : Total amount of all tolls paid in trip. → 여행에서 지불된 모든 통행료의 총 금액.
        - Total amount : The total amount charged to passengers. Does not include cash tips. → 승객에게 청구된 총 금액 (현금 팁 포함 X)