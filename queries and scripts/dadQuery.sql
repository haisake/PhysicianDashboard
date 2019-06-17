/*
Purpose: To build the indicator table for the physician dashboard
Author: Hans Aisake
Date Created: June 14, 2019
Date Modified:
Comments:

List of Indicators
	- Percentage of days where census ≤ target(or funded level) –  by service (CAPPLAN)
	- Distribution of census by service is typical or atypical (CAPPLAN)
	- Distribution of census by TOD is typical or atypical (CAPPLAN)
	- # inpatient days (CAPPLAN)
	- # of ALC days or ALC rate % (CAPPLAN)
	- ALOS* days by service (CAPPLAN)
	- LOS days distribution typical or atypical (CAPPLAN)
	- ALOS/ELOS (ADRMart)
	- ALOS/ELOS top 5 most significant* worst CMGs (ADRMart)
	- Admission rate by service (EDMart)
	- Admission Volumes By ToD & Service Distribution typical or atypical (EDMart)
	- Discharge Volumes By ToD & Service Distribution typical or atypical (CAPPLAN)
	- # ED consults by service (EDMART)
	- LLOS days snapshot (CAPPLAN)
	- LLOS days snapshot ALC (CAPPLAN)
	- Service transfer volumes typical or atypical to other services (CAPPLAN)
	- 7 and 28 day raw readmission rates (or volumes ) by services (CAPPLAN)
*/

-----------------------------------
-- FP Table  dim.FPFY
-----------------------------------
	IF OBJECT_ID('tempdb.dbo.#physDB_reportFP') is not NULL DROP TABLE #physDB_reportFP;
	GO

	WITH TEMP AS (
		SELECT distinct TOP 39  fiscalyear, fiscalperiodlong, fiscalperiodstartdate, fiscalperiodenddate
	
		FROM ADTCMart.dim.[Date]
		WHERE FiscalPeriodEndDate < DATEADD(day, -1, GETDATE())	--FP over for at least a year
		ORDER BY fiscalperiodlong desc
	)

	--add on fiscal year dates
	SELECT X.*, Y.FiscalYearStartDate, Y.FiscalYearEndDate
	INTO #physDB_reportFP
	FROM TEMP as X
	LEFT JOIN (SELECT fiscalyear, min(fiscalperiodstartdate) as 'FiscalYearStartDate', max(fiscalperiodenddate) as 'FiscalYearEndDate' FROM TEMP GROUP BY fiscalyear) as Y
	ON X.FiscalYear=Y.FiscalYear
	;

-------------------------------------------
-- Pull DAD data; dbo.DAD
-------------------------------------------
IF OBJECT_ID('tempdb.dbo.#DAD') is not null DROP TABLE #DAD;
GO

SELECT D.FiscalPeriodLong
, D.FiscalYear
, ADR.MostRespProviderCode
, CMGPlusCode +'-' + LEFT(CMGPlusDesc,40) as 'CMG'
, SUM([ELOS]) as 'Sum_ELOS'
, SUM([LOS]) as 'Sum_LOS'
, SUM(1) as 'NumCases'
INTO #DAD
FROM ADRMart.dbo.vwAbstractFact as ADR
INNER JOIN #physDB_reportFP as D
ON ADR.DischargeDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate	--between FP
WHERE FacilityShortName='RHS'
AND  CMGStatusDesc  ='typical'
AND  CareType='A'
GROUP BY  D.FiscalPeriodLong
, D.FiscalYear
, CMGPlusCode +'-' + LEFT(CMGPlusDesc,40)
;
GO
