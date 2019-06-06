/*
Purpose: To comptue the census by provider and nursing unit at Richmond hospital from CapPlan given a specified hour for a specified date range
Author: Hans Aisake
Date Created: June 6, 2019
Date Modified:
Comments:
	The base idea came from Peter's query. I removed the classification as Hospitalist, GIM, etc... from this query.
	Those linkages have to be found elsewhere in other databses like DSSI if anywhere at all.
	Hard coding a mapping into this query was not desired, but we will have sufficient foundations to do it later
 */

/*Parameters for the query*/
DECLARE @CensusHour int = 7;
DECLARE @start_date DATETIME = DATEADD(year, -1, DATEADD(hh, @CensusHour, DATEADD(dd, DATEDIFF(dd, 0, GETDATE()), 0))); /*get the current date, set time to @CensusHour, and then reduce the year by 1*/
DECLARE @end_date DATETIME = DATEADD(hh, @CensusHour, DATEADD(dd, DATEDIFF(dd, 0, GETDATE()), 0)); /*get the current date, set time to @CensusHour*/

/*create a list of dates based on the parameters*/
WITH AllDays AS 
( SELECT @start_date AS [Date]
  UNION ALL
  SELECT DATEADD(DAY, 1, [Date])
  FROM AllDays
  WHERE [Date] < @end_date 
)

SELECT [Date] INTO #dates FROM AllDays OPTION (MAXRECURSION 600);

/*find the census for each provider on the dates*/
SELECT @CensusHour as 'CensusHour'
, case when [lu_SpecialtyID]='ALC' then 'ALC' 
	   else 'Acute' 
END as ALCFlag
, [lu_HealthCareProfessionalID] as 'DrCode'
, [lu_SpecialtyID]
, [lu_WardID] as 'NursingUnitCode'
, COUNT(1) as 'Census'
/* these fields aren't required
, [AssignmentID]
, [EncounterID]
, [AssignmentDate]	/*start A*/
, [AssignmentEndDate] /*end E*/
, isnull([lu_BedID],'UNKNOWN') as 'Bed'
*/
FROM [CapPlan_RHS].[dbo].[Assignments] as X
INNER JOIN #dates as Y
ON Y.[Date] BETWEEN AssignmentDate AND ISNULL(AssignmentEndDate,'2050-01-01')	--filter to days relevant between @start and @end and assign a date for computing census
where X.lu_wardid not like 'm[0-9]%'	/*ignore minoru*/
and X.lu_wardid not in ('rhbcb','ramb') /*ignore birth center and ambulatory care*/
GROUP BY case when [lu_SpecialtyID]='ALC' then 'ALC' 
	   else 'Acute' 
END
, [lu_HealthCareProfessionalID]
, [lu_SpecialtyID]
, [lu_WardID]

