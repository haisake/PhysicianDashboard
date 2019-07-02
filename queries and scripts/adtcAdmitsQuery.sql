/*
Purpose: Pull data from ADTCMart so that we can identify admits.
Want to compute volumes by day of week and time of day by physician services.
For the physician dashboard
Author: Hans Aisake
Comments:
*/

	/* build a table of fiscal period dates */
	WITH reportFP AS (
		SELECT distinct TOP 26  fiscalyear, fiscalperiodlong, fiscalperiodstartdate, fiscalperiodenddate
		FROM ADTCMart.dim.[Date]
		WHERE FiscalPeriodEndDate < DATEADD(day, -1, GETDATE())	/* FP over for at least a year */
		ORDER BY fiscalperiodlong desc
	)

	/* pull discharges from ADTC mart */
	SELECT D.FiscalPeriodLong as 'Admit_FP'
	, D.FiscalYear as 'Admit_FY'
	, [AdmissionNursingUnitCode]
	, DATENAME(dw, AdjustedAdmissionDate) as 'Admit_DoW'
	, DATEPART(hour, AdjustedAdmissionTime) as 'Admit_Hour'
	, 'P' + AD.AdmissionAttendingDoctorCode as 'DrCode'
	, COUNT(1) as 'NumAdmissions'
	FROM ADTCMart.[ADTC].[vwAdmissionDischargeFact] as AD
	INNER JOIN reportFP as D
	ON AD.AdjustedAdmissionDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate	/*filter to discharges within the time frame */
	WHERE AD.[site]='rmd'	/* richmond hospital only */
	AND LEFT(AdmissionNursingUnitCode,1)!='M'	/* excludes ('Minoru Main Floor East','Minoru Main Floor West','Minoru Second Floor East','Minoru Second Floor West','Minoru Third Floor') */
	GROUP BY D.FiscalPeriodLong
	, D.FiscalYear
	, [AdmissionNursingUnitCode]
	, DATENAME(dw, AdjustedAdmissionDate)
	, DATEPART(hour, AdjustedAdmissionTime)
	, 'P' + AD.AdmissionAttendingDoctorCode 
