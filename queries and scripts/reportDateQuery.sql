

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
