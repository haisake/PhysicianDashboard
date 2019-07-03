/*
Purpose: To pull required ED visit data for the physician dashboard indicators.
Author: Hans Aisake
Date Created: June 14, 2019
Date Modified:
Comments:

Can't get consulting physician as it is not entered.
Instead services are requested.

*/

/* fiscal period and year table */
WITH fpDates AS (
	SELECT distinct TOP , fiscalperiodlong, fiscalperiodstartdate, fiscalperiodenddate
	FROM EDMart.dim.[Date]
	WHERE FiscalPeriodEndDate < DATEADD(day, -1, GETDATE())	--FP over for at least a year
	ORDER BY fiscalperiodlong desc
)

, fyDates AS (
	SELECT fiscalyear, min(fiscalperiodstartdate) as 'FiscalYearStartDate', max(fiscalperiodenddate) as 'FiscalYearEndDate' 
	FROM fpDates
	GROUP BY fiscalyear
)

/* add on fiscal year dates */
, fpFyDates AS (
	SELECT X.*, Y.FiscalYearStartDate, Y.FiscalYearEndDate
	FROM fpDates as X
	LEFT JOIN fyDates as Y
	ON X.FiscalYear=Y.FiscalYear
)

/* Pull ED visit data. */
SELECT [InpatientAdmittingDrCode]
, SUM(CASE WHEN [BedRequestDate] is not null THEN 1 ELSE 0 END) as 'Admits'
, SUM(1) 'TotalVisits'
FROM EDMart.dbo.vwEDVisitIdentifiedRegional as ED
INNER JOIN fpFyDates as D /* filter to fiscal periods of interest */
ON ED.StartDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate
WHERE ED.FacilityShortName='RHS'
AND ConsultationRequestDate is not NULL
GROUP BY StartDateFiscalPeriodLong
, DATENAME(dw, ConsultationRequestDate)
, DATEPART(hour, ConsultationRequestTime) 
, CASE WHEN ConsultationServiceDescription ='Internal Medicine' THEN 'Internal Medicine'
/*	   WHEN ConsultationServiceDescription like '%Surgery%' THEN 'Surgeon'
	   WHEN ConsultationServiceDescription in ('Psychiatry') THEN 'Psych' */
	   ELSE 'Other'
END



SELECT COUNT(1) 
FROM EDMart.dbo.vwEDVisitIdentifiedRegional
 WHERE DrName = InpatientAdmittingDoctorName
 AND FacilityShortName='RHS'
 AND AdmittedFlag=1;

 SELECT startdate, AdmittedFlag, DrName, InpatientAdmittingDoctorName
FROM EDMart.dbo.vwEDVisitIdentifiedRegional
 WHERE DrName != InpatientAdmittingDoctorName
 AND FacilityShortName='RHS'
 AND AdmittedFlag=1
 ;