/*
Purpose: To comptue the census by provider and nursing unit at Richmond hospital from CapPlan given a specified hour for a specified date range
Author: Hans Aisake
Date Created: June 6, 2019
Date Modified:
Comments:
	The base idea came from Peter's query. I removed the classification as Hospitalist, GIM, etc... from this query.
	Those linkages have to be found elsewhere in other databses like DSSI if anywhere at all.

	Taking about 7 minutes to run these days.

 */


/* we have admissions everyday so we can use the admissino table to get a list of dates without the CTE loop. This is easier for R to run. */
WITH dates AS(
	SELECT distinct CONVERT(date, [AdmissionDate]) as [date] 
	FROM [CapPlan_RHS].[dbo].[Admissions]
	WHERE CONVERT(date, [AdmissionDate])  BETWEEN DATEADD(year, -1, CONVERT(date, GETDATE())) AND GETDATE()
)

, dates2 AS (
	SELECT DATEADD(hour, 7, CONVERT(datetime, [date]) ) as 'Date_withHour' /* change the 7 to the hour of the day you want */
	, [date]
	FROM dates
)

/*find the census for each provider on the dates*/
SELECT Y.[Date]
, 7 as 'CensusHour'
, case when [lu_SpecialtyID]='ALC' then 'ALC' 
	   else 'Acute' 
END as ALCFlag
, [lu_HealthCareProfessionalID] as 'DrCode'
, [lu_SpecialtyID]
, CASE WHEN [lu_WardID] ='R4N' AND Y.[Date] >=' 2018-10-19' THEN 'ACE' ELSE 'NOTACE' END as 'ACE_Flag'
, COUNT(1) as 'Census'
FROM [CapPlan_RHS].[dbo].[Assignments] as X
INNER JOIN dates2 as Y
ON Y.[Date_withHour] BETWEEN AssignmentDate AND ISNULL(AssignmentEndDate,'2050-01-01')	/* filter to days relevant between @start and @end and assign a date for computing census */
where LEFT(X.lu_wardid,1)!='m'	/*ignore minoru*/
and X.lu_wardid not in ('rhbcb','ramb') /*ignore birth center and ambulatory care*/
GROUP BY Y.[Date]
,case when [lu_SpecialtyID]='ALC' then 'ALC' 
	 else 'Acute' 
END
, [lu_HealthCareProfessionalID]
, [lu_SpecialtyID]
, CASE WHEN [lu_WardID] ='R4N' AND Y.[Date] >=' 2018-10-19' THEN 'ACE' ELSE 'NOTACE' END
