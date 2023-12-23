# 서울특별시 행정동별 평균소득 데이터
## date: "**2023-12-23**"

----
[<img src="seoul_plot.png">](./girafe_map_int.html),
----

#### 서울특별시의 행정동별 평균 소득을 시각화하였습니다. 

#### 참고해주세요:
행정동별 소득 지출에 관한 데이터가 "raw_seoul" 객체입니다. "raw_seoul" 객체 중 "quarter" == 20231 조건을 만족하는 데이터를 "seoul_20231" 객체에 저장하였습니다.<br>

"seoul_20231" 객체는 *"개포3동", "상일제1동", "상일제2동"의 데이터를 포함하지 않고* 있는데, 이는 데이터가 행정동 분리와 명칭 변경 이전에 작성되었기 때문인 것으로 보입니다.

1. "seoul_20231"상의 "일원2동"은 2022년 12월 23일에 "개포3동"으로 명칭이 변경되었습니다. 이에 따라 "seoul_20231"상의 "dong_name"변수를 "일원2동"에서 "개포3동"으로 변경하고 해당 "dong_code"와 "EMD_CD" 변수 또한 변경하여 "seoul_20231_updated" 객체에 저장하였습니다. 변경된 부분에 관하여 다음의 코드를 보세요.

```r
seoul_20231_updated[seoul_20231_updated$dong_code == 11680740, 17] <- "1168067500" # 일원2동 -> 개포3동 renamed
seoul_20231_updated[seoul_20231_updated$dong_code == 11680740, 3] <-  "개포3동" # 일원2동 -> 개포3동 renamed
```

2. "seoul_20231"상의 "상일동"은 2021년 07월 01일에 "상일제1동"으로 명칭이 변경되었습니다. 이에 따라 "seoul_20231"상의 "dong_name"변수를 "상일동"에서 "상일제1동"으로 변경하고 해당 "dong_code"와 "EMD_CD" 변수 또한 변경하여 "seoul_20231_updated" 객체에 저장하였습니다. 변경된 부분에 관하여 다음의 코드를 보세요. 

```r
seoul_20231_updated[seoul_20231_updated$dong_code == 11740520, 17] <- "1174052500" # 상일동 -> 상일제1동 renamed
seoul_20231_updated[seoul_20231_updated$dong_code == 11740520, 3] <- "상일제1동" # 상일동 -> 상일제1동 renamed
```

3."seoul_20231"상의 "강일동"의 남쪽 지역은 2021년 07월 01일에 "상일제2동"으로 분리되었습니다. 이에 따라 "seoul_20231_updated" 객체에 "dong_name"을 "상일제2동"으로, 해당하는 "dong_code"와 "EMD_CD"를 넣어 새로운 행을 추가하였습니다. 이때 "상일제2동"의 소득-지출 데이터는 "강일동"의 데이터를 복사하여 그대로 붙여넣었음을 참고해주세요. 변경된 부분에 관하여 다음의 코드를 보세요. 

```r
seoul_20231_updated[nrow(seoul_20231_updated)+1, ] <- NA # New empty row added
seoul_20231_updated[nrow(seoul_20231_updated), 1:3] <- list(20231, 11740526,"상일제2동") # 상일제2동 row added (South region of 강일동 became 상일제2동)
seoul_20231_updated[nrow(seoul_20231_updated), 4:16] <- seoul_20231_updated[283, 4:16] # 상일제2동 data is the same as 강일동. 
seoul_20231_updated[nrow(seoul_20231_updated), 17] <- "1174052600"
```

#### Mean income per 행정동 (dong of administration) in Seoul was visualized. 

#### NOTE:
Income and expenditure data per 행정동 is in the "raw_seoul" object. I focused on the data of "quarter" == 20231, thus the data satisfying the conditon was saved to "seoul_20231" object.<br>

Note that *"seoul_20231" object does not contain a row of "개포3동", "상일제1동", and "상일제2동" in dong_name column*. This is because the "raw_seoul" dataset was made before 행정동 are renamed and separated.

1. "일원2동" in the "raw_seoul" object is renamed to "개포3동" in 2022-12-23. The update was made to rename "일원2동" to "개포3동" and change the corresponding "dong_code" variable and "EMD_CD" variable, which are saved to "seoul_20231_updated" object. See the following script.

```r
seoul_20231_updated[seoul_20231_updated$dong_code == 11680740, 17] <- "1168067500" # 일원2동 -> 개포3동 renamed
seoul_20231_updated[seoul_20231_updated$dong_code == 11680740, 3] <-  "개포3동" # 일원2동 -> 개포3동 renamed
```

2. "상일동" in the "raw_seoul" object is renamed to "상일제1동" in 2021-07-01. The update was made to rename "상일동" to "상일제1동" and change the corresponding "dong_code" variable and "EMD_CD" variable, which are saved to "seoul_20231_updated" object. See the following script.

```r
seoul_20231_updated[seoul_20231_updated$dong_code == 11740520, 17] <- "1174052500" # 상일동 -> 상일제1동 renamed
seoul_20231_updated[seoul_20231_updated$dong_code == 11740520, 3] <- "상일제1동" # 상일동 -> 상일제1동 renamed
```

3. South region of "강일동" in the "raw_seoul"was separated from "강일동" and named "상일제2동" in 2021-07-01. The update was made to create a new row that specify "상일제2동" and to add the corresponding "dong_code" variable and "EMD_CD" variable. Plus, the data of "상일제2동" was copied from that of "강일동". The changes are saved to "seoul_20231_updated" object. See the following script.

```r
seoul_20231_updated[nrow(seoul_20231_updated)+1, ] <- NA # New empty row added
seoul_20231_updated[nrow(seoul_20231_updated), 1:3] <- list(20231, 11740526,"상일제2동") # 상일제2동 row added (South region of 강일동 became 상일제2동)
seoul_20231_updated[nrow(seoul_20231_updated), 4:16] <- seoul_20231_updated[283, 4:16] # 상일제2동 data is the same as 강일동. 
seoul_20231_updated[nrow(seoul_20231_updated), 17] <- "1174052600"
```
