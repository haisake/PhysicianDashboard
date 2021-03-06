/*
Purpose: To comptue the patient volumes per period in CapPlan for the providers
Author: Hans Aisake
Date Created: June 6, 2019
Date Modified:
Comments:
*/


/* manualy have to add the table */
With dates AS (
	SELECT TOP 1 '2017-01' as 'FiscalPeriodLong','2016/4/1' as 'FiscalPeriodStartDate','2016/4/21' as 'FiscalPeriodEndDate' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2017-02','2016/4/22','2016/5/19' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2017-03','2016/5/20','2016/6/16' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2017-04','2016/6/17','2016/7/14' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2017-05','2016/7/15','2016/8/11' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2017-06','2016/8/12','2016/9/8' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2017-07','2016/9/9','2016/10/6' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2017-08','2016/10/7','2016/11/3' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2017-09','2016/11/4','2016/12/1' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2017-10','2016/12/2','2016/12/29' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2017-11','2016/12/30','2017/1/26' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2017-12','2017/1/27','2017/2/23' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2017-13','2017/2/24','2017/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-01','2017/4/1','2017/4/20' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-02','2017/4/21','2017/5/18' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-03','2017/5/19','2017/6/15' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-04','2017/6/16','2017/7/13' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-05','2017/7/14','2017/8/10' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-06','2017/8/11','2017/9/7' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-07','2017/9/8','2017/10/5' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-08','2017/10/6','2017/11/2' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-09','2017/11/3','2017/11/30' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-10','2017/12/1','2017/12/28' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-11','2017/12/29','2018/1/25' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-12','2018/1/26','2018/2/22' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2018-13','2018/2/23','2018/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-01','2018/4/1','2018/5/3' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-02','2018/5/4','2018/5/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-03','2018/6/1','2018/6/28' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-04','2018/6/29','2018/7/26' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-05','2018/7/27','2018/8/23' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-06','2018/8/24','2018/9/20' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-07','2018/9/21','2018/10/18' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-08','2018/10/19','2018/11/15' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-09','2018/11/16','2018/12/13' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-10','2018/12/14','2019/1/10' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-11','2019/1/11','2019/2/7' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-12','2019/2/8','2019/3/7' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2019-13','2019/3/8','2019/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-01','2019/4/1','2019/5/2' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-02','2019/5/3','2019/5/30' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-03','2019/5/31','2019/6/27' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-04','2019/6/28','2019/7/25' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-05','2019/7/26','2019/8/22' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-06','2019/8/23','2019/9/19' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-07','2019/9/20','2019/10/17' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-08','2019/10/18','2019/11/14' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-09','2019/11/15','2019/12/12' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-10','2019/12/13','2020/1/9' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-11','2020/1/10','2020/2/6' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-12','2020/2/7','2020/3/5' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2020-13','2020/3/6','2020/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-01','2020/4/1','2020/4/30' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-02','2020/5/1','2020/5/28' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-03','2020/5/29','2020/6/25' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-04','2020/6/26','2020/7/23' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-05','2020/7/24','2020/8/20' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-06','2020/8/21','2020/9/17' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-07','2020/9/18','2020/10/15' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-08','2020/10/16','2020/11/12' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-09','2020/11/13','2020/12/10' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-10','2020/12/11','2021/1/7' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-11','2021/1/8','2021/2/4' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-12','2021/2/5','2021/3/4' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2021-13','2021/3/5','2021/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-01','2021/4/1','2021/4/29' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-02','2021/4/30','2021/5/27' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-03','2021/5/28','2021/6/24' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-04','2021/6/25','2021/7/22' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-05','2021/7/23','2021/8/19' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-06','2021/8/20','2021/9/16' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-07','2021/9/17','2021/10/14' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-08','2021/10/15','2021/11/11' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-09','2021/11/12','2021/12/9' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-10','2021/12/10','2022/1/6' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-11','2022/1/7','2022/2/3' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-12','2022/2/4','2022/3/3' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2022-13','2022/3/4','2022/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-01','2022/4/1','2022/4/28' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-02','2022/4/29','2022/5/26' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-03','2022/5/27','2022/6/23' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-04','2022/6/24','2022/7/21' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-05','2022/7/22','2022/8/18' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-06','2022/8/19','2022/9/15' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-07','2022/9/16','2022/10/13' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-08','2022/10/14','2022/11/10' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-09','2022/11/11','2022/12/8' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-10','2022/12/9','2023/1/5' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-11','2023/1/6','2023/2/2' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-12','2023/2/3','2023/3/2' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2023-13','2023/3/3','2023/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-01','2023/4/1','2023/4/27' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-02','2023/4/28','2023/5/25' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-03','2023/5/26','2023/6/22' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-04','2023/6/23','2023/7/20' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-05','2023/7/21','2023/8/17' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-06','2023/8/18','2023/9/14' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-07','2023/9/15','2023/10/12' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-08','2023/10/13','2023/11/9' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-09','2023/11/10','2023/12/7' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-10','2023/12/8','2024/1/4' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-11','2024/1/5','2024/2/1' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-12','2024/2/2','2024/2/29' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2024-13','2024/3/1','2024/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-01','2024/4/1','2024/4/25' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-02','2024/4/26','2024/5/23' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-03','2024/5/24','2024/6/20' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-04','2024/6/21','2024/7/18' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-05','2024/7/19','2024/8/15' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-06','2024/8/16','2024/9/12' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-07','2024/9/13','2024/10/10' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-08','2024/10/11','2024/11/7' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-09','2024/11/8','2024/12/5' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-10','2024/12/6','2025/1/2' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-11','2025/1/3','2025/1/30' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-12','2025/1/31','2025/2/27' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2025-13','2025/2/28','2025/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-01','2025/4/1','2025/4/24' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-02','2025/4/25','2025/5/22' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-03','2025/5/23','2025/6/19' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-04','2025/6/20','2025/7/17' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-05','2025/7/18','2025/8/14' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-06','2025/8/15','2025/9/11' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-07','2025/9/12','2025/10/9' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-08','2025/10/10','2025/11/6' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-09','2025/11/7','2025/12/4' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-10','2025/12/5','2026/1/1' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-11','2026/1/2','2026/1/29' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-12','2026/1/30','2026/2/26' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2026-13','2026/2/27','2026/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-01','2026/4/1','2026/4/23' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-02','2026/4/24','2026/5/21' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-03','2026/5/22','2026/6/18' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-04','2026/6/19','2026/7/16' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-05','2026/7/17','2026/8/13' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-06','2026/8/14','2026/9/10' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-07','2026/9/11','2026/10/8' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-08','2026/10/9','2026/11/5' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-09','2026/11/6','2026/12/3' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-10','2026/12/4','2026/12/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-11','2027/1/1','2027/1/28' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-12','2027/1/29','2027/2/25' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2027-13','2027/2/26','2027/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-01','2027/4/1','2027/4/22' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-02','2027/4/23','2027/5/20' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-03','2027/5/21','2027/6/17' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-04','2027/6/18','2027/7/15' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-05','2027/7/16','2027/8/12' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-06','2027/8/13','2027/9/9' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-07','2027/9/10','2027/10/7' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-08','2027/10/8','2027/11/4' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-09','2027/11/5','2027/12/2' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-10','2027/12/3','2027/12/30' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-11','2027/12/31','2028/1/27' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-12','2028/1/28','2028/2/24' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2028-13','2028/2/25','2028/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-01','2028/4/1','2028/4/20' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-02','2028/4/21','2028/5/18' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-03','2028/5/19','2028/6/15' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-04','2028/6/16','2028/7/13' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-05','2028/7/14','2028/8/10' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-06','2028/8/11','2028/9/7' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-07','2028/9/8','2028/10/5' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-08','2028/10/6','2028/11/2' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-09','2028/11/3','2028/11/30' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-10','2028/12/1','2028/12/28' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-11','2028/12/29','2029/1/25' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-12','2029/1/26','2029/2/22' FROM [CapPlan_RHS].[dbo].[AccessLevels]
	 UNION SELECT TOP 1 '2029-13','2029/2/23','2029/3/31' FROM [CapPlan_RHS].[dbo].[AccessLevels]
)

/*find the census for each provider on the dates*/
SELECT D.FiscalPeriodLong
, [lu_HealthCareProfessionalID] as 'DrCode'
, CASE WHEN [lu_WardID] ='R4N' AND D.[FiscalPeriodLong]  >=' 2019-08' THEN 'ACE' ELSE 'NOTACE' END as 'ACE_Flag'
, COUNT(distinct [EncounterID]) as 'NumUniqueEncounters'
FROM [CapPlan_RHS].[dbo].[Assignments] as X
INNER JOIN dates as D
ON X.AssignmentDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate
where X.lu_wardid not like 'm[0-9]%'	/*ignore minoru*/
and X.lu_wardid not in ('rhbcb','ramb') /*ignore birth center and ambulatory care*/
GROUP BY D.FiscalPeriodLong
, [lu_HealthCareProfessionalID]
, CASE WHEN [lu_WardID] ='R4N' AND D.[FiscalPeriodLong] >=' 2019-08' THEN 'ACE' ELSE 'NOTACE' END
