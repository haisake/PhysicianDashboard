/*
Purpose: Pull data from ADTCMart so that we have a date table with fiscal period and fiscal year for grouping CapPlan data in R.
For the physician dashboard
Author: Hans Aisake
Comments:
*/

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
	SELECT X.*, Y.FiscalYearStartDate, Y.FiscalYearEndDate
	FROM fpDates as X
	LEFT JOIN fyDates as Y
	ON X.FiscalYear=Y.FiscalYear
