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
	, AD.DischargeAttendingDrcode
	INTO #temp
	FROM ADTCMart.adtc.vwAdmissionDischargeFact as AD
	INNER JOIN #physDB_reportFP as D
	ON AD.AdjustedDischargeDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate
	WHERE site='rmd'
	AND AdjustedDischargeDate is not NULL
	;

	IF OBJECT_ID('tempdb.dbo.#ADTC1') is not NULL DROP TABLE #adtc1;
	GO

	SELECT *
	INTO #adtc1
	FROM (
		SELECT ROW_NUMBER() OVER(PARTITION BY X.PatientID ORDER BY Y.AdjustedAdmissionDate ASC) as 'rn'
		, X.*
		, DATEDIFF(day, X.AdjustedDischargeDate, Y.AdjustedAdmissionDate) as 'Readmission_any_days'
		FROM #temp as X
		LEFT JOIN #temp as Y
		ON X.AdjustedDischargeDate < Y.AdjustedAdmissionDate
	) Z
	WHERE rn=1
	;