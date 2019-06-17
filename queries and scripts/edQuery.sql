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
-- Pull ED data; can't get consulting physician ; dbo.ED
-------------------------------------------
IF OBJECT_ID('tempdb.dbo.#ED') is not NULL DROP TABLE #ED;
GO

SELECT TOP 10 startdate
, visitID
, AdmittedFlag
, BedRequestDate as 'Admit_Date'
, DATENAME(dw, BedREquestDate) as 'Admit_DoW'
, DATEPART(hour, BedRequestTime) as 'Admit_Hour'
, ConsultationRequestDate
, DATENAME(dw, ConsultationRequestDate) as 'Consult_DoW'
, DATEPART(hour, ConsultationRequestTime) as 'Consult_Hour'
, CASE WHEN ConsultationServiceDescription ='Internal Medicine' THEN 'GIM'
	   WHEN ConsultationServiceDescription like '%Surgery%' THEN 'Surgeon'
	   WHEN ConsultationServiceDescription in ('Psychiatry') THEN 'Psych'
	   ELSE 'Other'
END as 'ConsultationServiceDesc'
, InpatientNursingUnitName
INTO #ED
FROM EDMart.dbo.vwEDVisitIdentifiedRegional as ED
INNER JOIN #physDB_reportFP as D
ON ED.StartDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate
WHERE ED.FacilityShortName='RHS'
;
