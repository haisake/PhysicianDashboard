/*
Purpose: Pull data from ADTCMart so that we can identify readmissions.
Want data to compute per period rates by physician services.
7 and 28 days.
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
	, adRecords As (
		SELECT AccountNumber
		, PatientID
		, AdjustedAdmissionDate
		, AdjustedDischargeDate
		, D.FiscalPeriodLong as 'Disch_FP'
		, D.FiscalYear as 'Disch_FY'
		, 'P' + AD.DischargeAttendingDrcode as 'DrCode'
		, CASE WHEN AD.DischargeNursingUnitCode ='R4N' AND D.FiscalPeriodLong >='2019-08' THEN 'ACE' ELSE 'NOTACE' END as 'ACE_Flag'
		FROM ADTCMart.adtc.vwAdmissionDischargeFact as AD
		INNER JOIN reportFP as D
		ON AdjustedDischargeDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate
		WHERE AD.[site]='rmd'	/* richmond hospital only */
		AND AD.AdjustedDischargeDate is not NULL /* patient must have been discharged */
		AND LEFT(AdmissionNursingUnitCode,1)!='M'	/* excludes ('Minoru Main Floor East','Minoru Main Floor West','Minoru Second Floor East','Minoru Second Floor West','Minoru Third Floor') */
		AND AccountType ='A'
	)

	/* add readmission times to the discharge time */
	, addReadmissions AS (
		SELECT *
		FROM (
			SELECT ROW_NUMBER() OVER(PARTITION BY X.PatientID, X.AdjustedDischargeDate ORDER BY Y.AdjustedAdmissionDate ASC) 'rn'
			, X.*
			, ISNULL(DATEDIFF(day, X.AdjustedDischargeDate, Y.AdjustedAdmissionDate),9999) as 'Readmission_any_days'
			, Y.AdjustedAdmissionDate as 'ReadmissionDate'
			FROM adRecords as X
			LEFT JOIN adRecords as Y
			ON X.AdjustedDischargeDate < Y.AdjustedAdmissionDate
			AND X.PatientId=Y.PatientId
		) Z
		WHERE rn=1
	)

	--add on fiscal period for the readmission
	SELECT Disch_FP
	, Disch_FY
	, Drcode
	, ACE_Flag
	, SUM( IIF(Readmission_any_days BETWEEN 0 AND 7, 1, 0) ) as 'Seven_Day_Readmits'
	, SUM( IIF(Readmission_any_days BETWEEN 0 AND 28, 1, 0) ) as 'TwentyEight_Day_Readmits'
	, SUM(1) as 'TotalDischarges'
	FROM addReadmissions as X 
	GROUP BY Disch_FP
	, Disch_FY
	, Drcode
	, ACE_Flag




