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
	SELECT distinct TOP 39  fiscalyear, fiscalperiodlong, fiscalperiodstartdate, fiscalperiodenddate
	FROM ADTCMart.dim.[Date]
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
SELECT startdate
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
FROM EDMart.dbo.vwEDVisitIdentifiedRegional as ED
INNER JOIN fpFyDates as D /* filter to fiscal periods of interest */
ON ED.StartDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate
WHERE ED.FacilityShortName='RHS'


