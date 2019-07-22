/*
Purpose: To build the indicator table for the physician dashboard
Author: Hans Aisake
Date Created: June 14, 2019
Date Modified:
Comments:

 transfers between physicians
 encounterId in (615375, 969797, 287650) are good example cases with many handoffs
 What is the difference between a transfer and hand-off same service

*/


/* find the subset of the CapPlan assignments needed */
WITH applicableEncounters AS (
	SELECT X.AssignmentID, X.EncounterID, X.AssignmentDate, X.AssignmentEndDate, X.lu_EncounterID, X.lu_HealthCareProfessionalID, X.lu_WardID
	FROM [CapPlan_RHS].[dbo].[Assignments] as X
	INNER JOIN /* only keep records with at least 2 distinct physicians otherwise there can't be any transfers of interest */
	(	SELECT encounterID, COUNT(distinct [lu_HealthCareProfessionalID]) as 'NumProviders'
		FROM [CapPlan_RHS].[dbo].[Assignments]
		GROUP BY EncounterID
		HAVING COUNT(distinct [lu_HealthCareProfessionalID]) >=2	/* need at least 2 different providers in the encounter */
	) as Y
	ON X.EncounterId=Y.EncounterID	/* same encounterID */
	WHERE AssignmentEndDate >= DATEADD(month, -1, DATEADD(year,-1,GETDATE()))		/* valid history range */
	OR AssignmentDate >= DATEADD(month, -1, DATEADD(year,-1,GETDATE()))				/* valid history range */
)

/*assign an ordered row number to the eligable records based on assignment startdate */

, rowNum AS (
	SELECT ROW_NUMBER() OVER(Partition by EncounterID ORDER BY AssignmentDate ASC) as 'rn'
	, *
	FROM applicableEncounters
)

/* identify transfer data set */
SELECT X.EncounterID
, CAST(CAST(X.AssignmentDate as date) as datetime) as 'TransferDate' /* need to set to 00:00:00 to facilitate joins to dim.Date */
, DATENAme(dw, X.AssignmentDate) as 'TransferDoW'
, DATEPART(hour, X.AssignmentDate) as 'TransferHour'
, X.lu_HealthCareProfessionalID as 'OrigPhys'
, Y.lu_HealthCareProfessionalID as 'TransPhys'
, CASE WHEN X.[lu_WardID] ='R4N' AND X.[AssignmentDate] >= '2018-10-19' THEN 'ACE' ELSE 'NOTACE' END as 'Orig_ACE_Flag'
, CASE WHEN Y.[lu_WardID] ='R4N' AND X.[AssignmentDate] >=' 2018-10-19' THEN 'ACE' ELSE 'NOTACE' END as 'Trans_ACE_Flag'
FROM rowNum as X
INNER JOIN rowNum as Y
ON X.rn=(Y.rn-1) 
AND X.EncounterID=Y.EncounterID
AND X.lu_HealthCareProfessionalID!=Y.lu_HealthCareProfessionalID
