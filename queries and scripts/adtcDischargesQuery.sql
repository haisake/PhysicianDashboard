/*
Purpose: Pull data from ADTCMart so that we can identify discharges and identify time after discharge till a readmission.
Want to compute volumes by day of week and time of day for discharges by physician services.
Want to compute readmission rates by 7 and 28 days.
For the physician dashboard
Author: Hans Aisake
Comments:

--I think I should count admission and discharges and readmission rates or something instead of pulling the raw data.
The query takes too long

*/

	/* build a table of fiscal period dates */
	WITH reportFP AS (
		SELECT distinct TOP 39  fiscalyear, fiscalperiodlong, fiscalperiodstartdate, fiscalperiodenddate
		FROM ADTCMart.dim.[Date]
		WHERE FiscalPeriodEndDate < DATEADD(day, -1, GETDATE())	/* FP over for at least a year */
		ORDER BY fiscalperiodlong desc
	)

	/* pull discharges from ADTC mart */
	SELECT AdjustedDischargeDate
	, D.FiscalPeriodLong as 'Discharge_FP'
	, DATENAME(dw, AdjustedDischargeDate) as 'Disch_DoW'
	, DATEPART(hour, AdjustedDischargeTime ) as 'Disch_Hour'
	, 'P' + AD.DischargeAttendingDrcode as 'Drcode'
	, COUNT(1) as 'NumDischarges'
	FROM ADTCMart.adtc.vwAdmissionDischargeFact as AD
	INNER JOIN reportFP as D
	ON AD.AdjustedDischargeDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate	/*filter to discharges within the time frame */
	WHERE AD.[site]='rmd'	/* richmond hospital only */
	AND AD.AdjustedDischargeDate is not NULL /* patient must have been discharged */
	AND LEFT(DischargeNursingUnitCode,1)!='M'	/* excludes ('Minoru Main Floor East','Minoru Main Floor West','Minoru Second Floor East','Minoru Second Floor West','Minoru Third Floor') */
	GROUP BY AdjustedDischargeDate
	, D.FiscalPeriodLong
	, DATENAME(dw, AdjustedDischargeDate)
	, DATEPART(hour, AdjustedDischargeTime )
	, 'P' + AD.DischargeAttendingDrcode
