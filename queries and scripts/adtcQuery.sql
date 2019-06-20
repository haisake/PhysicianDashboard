/*
Purpose: Pull data from ADTCMart so that we can identify discharges and identify time after discharge till a readmission.
Want to compute volumes by day of week and time of day for discharges by physician services.
Want to compute readmission rates by 7 and 28 days.
For the physician dashboard
Author: Hans Aisake
Comments:
*/

	/* build a table of fiscal period dates */
	WITH TEMP AS (
		SELECT distinct TOP 39  fiscalyear, fiscalperiodlong, fiscalperiodstartdate, fiscalperiodenddate
		FROM ADTCMart.dim.[Date]
		WHERE FiscalPeriodEndDate < DATEADD(day, -1, GETDATE())	/* FP over for at least a year */
		ORDER BY fiscalperiodlong desc
	)

	/* add on fiscal year dates */
	, reportFP AS(
		SELECT X.*, Y.FiscalYearStartDate, Y.FiscalYearEndDate
		FROM TEMP as X
		LEFT JOIN (SELECT fiscalyear, min(fiscalperiodstartdate) as 'FiscalYearStartDate', max(fiscalperiodenddate) as 'FiscalYearEndDate' FROM TEMP GROUP BY fiscalyear) as Y
		ON X.FiscalYear=Y.FiscalYear
	)


	/* pull discharges from ADTC mart */
	, Discharges As (
		SELECT AccountNumber
		, PatientID
		, AdjustedAdmissionDate
		, AdjustedDischargeDate
		, DATENAME(dw, AdjustedDischargeDate) as 'Disch_DoW'
		, DATEPART(hour, AdjustedDischargeTime ) as 'Disch_Hour'
		, AD.DischargeAttendingDrcode
		FROM ADTCMart.adtc.vwAdmissionDischargeFact as AD
		INNER JOIN reportFP as D
		ON AD.AdjustedDischargeDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate	/*filter to discharges within the time frame */ 
		WHERE site='rmd'	/* richmond hospital only */
		AND AdjustedDischargeDate is not NULL /* patient must have been discharged */
	)

	/* add readmission times to the discharge time */
	SELECT *
	FROM (
		SELECT ROW_NUMBER() OVER(PARTITION BY X.PatientID, X.AdjustedDischargeDate ORDER BY Y.AdjustedAdmissionDate ASC) 'rn'
		, X.*
		, ISNULL(DATEDIFF(day, X.AdjustedDischargeDate, Y.AdjustedAdmissionDate),9999) as 'Readmission_any_days'
		, Y.AdjustedAdmissionDate as 'ReadmissionDate'
		FROM Discharges as X
		LEFT JOIN Discharges as Y
		ON X.AdjustedDischargeDate < Y.AdjustedAdmissionDate
		AND X.PatientId=Y.PatientId
	) Z
	WHERE rn=1


