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

-----------------------------------
-- service table; dim.DrService
-----------------------------------
 IF OBJECT_ID('tempdb.dbo.#drService') is not NULL DROP TABLE #drService;
 GO
 
 WITH richmondDocs as(
	  SELECT AttendDoctorName
	  , AttendDoctorCode
	 , MIN(censusdate) as 'FirstCensusDate'
	 , MAX(CensusDate) as 'LastCensusDate'
	  FROM ADTCMart.ADTC.vwCensusFact 
	 WHERE FacilityLongName = 'richmond hospital'
	 AND AttendDoctorName not in ('Invalid')
	 GROUP BY AttendDoctorName
	 , AttendDoctorCode
 )

SELECT AttendDoctorName as 'DrName'
, 'P' + AttendDoctorCode as 'DrCode'
, CASE WHEN hosp.HospitalistName is not null THEN 'Hospitalist'
	   WHEN doc.drname IN ('AHMED, IQBAL', 'SAHIHOLNASAB, VAHID', 'SEHMER, BENJAMIN MICHAEL') THEN 'Internal Medicine' 
	   WHEN doc.drname IN ('HALJAN, GREGORY JOSEPH','LAU, EDGAR SENG TEONG','BRUCE, JENNIFER ELIZABETH','WONG, JUSTIN KAI FAI') THEN 'Critical Care Medicine'
	   ELSE serv.DADDescription
END as 'DoctorService'
, CASE WHEN hosp.StartDate is not null THEN hosp.StartDate
	   WHEN doc.drname IN ('AHMED, IQBAL', 'SAHIHOLNASAB, VAHID', 'SEHMER, BENJAMIN MICHAEL') THEN FirstCensusDate
	   WHEN doc.drname IN ('HALJAN, GREGORY JOSEPH','LAU, EDGAR SENG TEONG','BRUCE, JENNIFER ELIZABETH','WONG, JUSTIN KAI FAI') THEN FirstCensusDate
	   ELSE FirstCensusDate
END as 'DS_StartDate'
, CASE WHEN hosp.EndDate is not null THEN hosp.EndDate
	   WHEN doc.drname IN ('AHMED, IQBAL', 'SAHIHOLNASAB, VAHID', 'SEHMER, BENJAMIN MICHAEL') THEN LastCensusDate
	   WHEN doc.drname IN ('HALJAN, GREGORY JOSEPH','LAU, EDGAR SENG TEONG','BRUCE, JENNIFER ELIZABETH','WONG, JUSTIN KAI FAI') THEN LastCensusDate
	   ELSE LastCensusDate
END as 'DS_EndDate'
INTO #drService
FROM richmondDocs	/*for all richmond docs*/
LEFT JOIN ADTCMart.dim.Doctor as doc
ON richmondDocs.AttendDoctorName=doc.DrName	/*same name*/
LEFT JOIN DSSI.dbo.hospitalists as hosp
ON doc.drname = HospitalistName	/*name is the same*/
AND hosp.FacilityLongName='richmond hospital'	/*richmond only*/
LEFT JOIN ADTCMart.dim.[Service] as serv
ON serv.ServiceID = doc.DrServiceID
/* DoctorService in ('Internal Medicine','Hospitalist') */
;
GO

-------------------------------------------
-- Pull DAD data; dbo.DAD
-------------------------------------------
IF OBJECT_ID('tempdb.dbo.#DAD') is not null DROP TABLE #DAD;
GO

SELECT D.FiscalPeriodLong
, D.FiscalYear
, DrServ.DoctorService
, CMGPlusCode +'-' + LEFT(CMGPlusDesc,40) as 'CMG'
, SUM([ELOS]) as 'Sum_ELOS'
, SUM([LOS]) as 'Sum_LOS'
, SUM(1) as 'NumCases'
INTO #DAD
FROM ADRMart.dbo.vwAbstractFact as ADR
INNER JOIN #physDB_reportFP as D
ON ADR.DischargeDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate	--between FP
INNER JOIN #drService as DrServ
ON ADR.MostRespProviderCode=DrServ.DrCode	--same provider code
WHERE FacilityShortName='RHS'
AND  CMGStatusDesc  ='typical'
AND  CareType='A'
GROUP BY  D.FiscalPeriodLong
, D.FiscalYear
, DrServ.DoctorService
, CMGPlusCode +'-' + LEFT(CMGPlusDesc,40)
;
GO

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

-------------------------------------------
-- discharges and readmissions; dbo.ADTC1
-------------------------------------------
IF OBJECT_ID('tempdb.dbo.#temp') is not null DROP TABLE #temp;
GO

SELECT TOP 10 AccountNumber
, PatientID
, AdjustedAdmissionDate
, AdjustedDischargeDate
, DATENAME(dw, AdjustedDischargeDate) as 'Disch_DoW'
, DATEPART(hour, AdjustedDischargeTime ) as 'Disch_Hour'
, DrServ.DoctorService
INTO #temp
FROM ADTCMart.adtc.vwAdmissionDischargeFact as AD
INNER JOIN #physDB_reportFP as D
ON AD.AdjustedDischargeDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate
LEFT JOIN #drService as DrServ
ON AD.DischargeAttendingDrcode=DrServ.DrCode	--same doc
WHERE site='rmd'
AND AdjustedDischargeDate is not NULL
;

IF OBJECT_ID('tempdb.dbo.#ADTC1') is not NULL DROP TABLE #adtc1;
GO

SELECT *
INTO #adtc1
FROM (
	SELECT ROW_NUMBER() OVER(PARTITION BY PatientID ORDER BY Y.AdjustedAdmissionDate ASC) as 'rn'
	, X.*
	, DATEDIFF(day, X.AdjustedDischargeDate, Y.AdjustedAdmissionDate) as 'Readmission_any_days'
	FROM #temp as X
	LEFT JOIN #temp as Y
	ON X.AdjustedDischargeDate < Y.AdjustedAdmissionDate
) Z
WHERE rn=1
;

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

-------------------------------------------
-- CapPlan Census; dbo.CP
-------------------------------------------

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

SELECT [Date] INTO #dates FROM AllDays OPTION (MAXRECURSION 600); /*query fails if the date range has more than 599 dates*/

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

DROP TABLE #dates;	/*remove temp tables*/