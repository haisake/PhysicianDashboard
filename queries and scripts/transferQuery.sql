﻿/*
Purpose: To build the indicator table for the physician dashboard
Author: Hans Aisake
Date Created: June 14, 2019
Date Modified:
Comments:

*/

-------------------------------------------
-- transfers between physicians; dbo.CP2
-------------------------------------------
--encounterId in (615375, 969797, 287650) are good example cases with many handoffs
--Question: What is the difference between a transfer and hand-off? same service?

--find the subset of the CapPlan assignments needed
IF OBJECT_ID('tempdb.dbo.#applicableEncounters') IS NOT NULL DROP TABLE #applicableEncounters;
GO

SELECT X.AssignmentID, X.EncounterID, X.AssignmentDate, X.AssignmentEndDate, X.lu_EncounterID, X.lu_HealthCareProfessionalID
INTO #applicableEncounters
FROM [CapPlan_RHS].[dbo].[Assignments] as X
INNER JOIN --only keep records with at least 2 distinct physicians otherwise there can't be any transfers of interest
(	SELECT encounterID, COUNT(distinct [lu_HealthCareProfessionalID]) as 'NumProviders'
	FROM [CapPlan_RHS].[dbo].[Assignments]
	GROUP BY EncounterID
	HAVING COUNT(distinct [lu_HealthCareProfessionalID]) >=2	--need at least 2 different providers in the encounter
) as Y
ON X.EncounterId=Y.EncounterID	--same encounterID
WHERE AssignmentEndDate >= DATEADD(month, -1, DATEADD(year,-3,GETDATE()))		--valid history range
OR AssignmentDate >= DATEADD(month, -1, DATEADD(year,-3,GETDATE()))				--valid history range
;
GO

--assign an ordered row number to the eligable records based on assignment startdate
IF OBJECT_ID('tempdb.dbo.#rowNum') is not null DROP TABLE #rowNum;
GO

SELECT ROW_NUMBER() OVER(Partition by EncounterID ORDER BY AssignmentDate ASC) as 'rn'
, *
INTO #rowNum
FROM #applicableEncounters
;

--identify transfer data set
IF OBJECT_ID('tempdb.dbo.#transfers') is not null DROP TABLE #transfers;
GO

SELECT X.EncounterID
, DATEPART(day, X.AssignmentDate) as 'TransferDate'
, DATEPART(hour, X.AssignmentDate) as 'TransferHour'
,  X.lu_HealthCareProfessionalID as 'OrigPhys'
, Y.lu_HealthCareProfessionalID as 'TransPhys'
INTO #transfers
FROM #rowNum as X
INNER JOIN #rowNum as Y
ON X.rn=(Y.rn-1) 
AND X.EncounterID=Y.EncounterID
AND X.lu_HealthCareProfessionalID!=Y.lu_HealthCareProfessionalID
;
GO