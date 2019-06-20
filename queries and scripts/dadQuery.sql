/*
Purpose: To comptue the pull required DAD records for the physician dashboar ALOS/ELOS computations
Author: Hans Aisake
Date Created: June 6, 2019
Date Modified:
Comments:
	The base idea came from Peter's query. I removed the classification as Hospitalist, GIM, etc... from this query.
	Those linkages have to be found elsewhere in other databses like DSSI if anywhere at all.
 */



	WITH TEMP AS (
		SELECT distinct TOP 39  fiscalyear, fiscalperiodlong, fiscalperiodstartdate, fiscalperiodenddate
		FROM ADTCMart.dim.[Date]
		WHERE FiscalPeriodEndDate < DATEADD(day, -1, GETDATE())	/* FP over for at least a year */
		ORDER BY fiscalperiodlong desc
	)

	, physDB_reportFP AS (
	/* add on fiscal year dates */
		SELECT X.*, Y.FiscalYearStartDate, Y.FiscalYearEndDate
		FROM TEMP as X
		LEFT JOIN (SELECT fiscalyear, min(fiscalperiodstartdate) as 'FiscalYearStartDate', max(fiscalperiodenddate) as 'FiscalYearEndDate' FROM TEMP GROUP BY fiscalyear) as Y
		ON X.FiscalYear=Y.FiscalYear
	)

	/*
		Pull DAD data; dbo.DAD
	*/
	SELECT D.FiscalPeriodLong
	, D.FiscalYear
	, 'P'+ADR.MostRespProviderCode as 'MostRespProviderCode'
	, CMGPlusCode +'-' + LEFT(CMGPlusDesc,40) as 'CMG'
	, SUM([ELOS]) as 'Sum_ELOS'
	, SUM([LOS]) as 'Sum_LOS'
	, SUM(1) as 'NumCases'
	FROM ADRMart.dbo.vwAbstractFact as ADR
	INNER JOIN physDB_reportFP as D
	ON CONVERT(datetime, CONVERT(varchar(8), ADR.DischargeDate),112) BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate	/* between FP */
	WHERE FacilityShortName='RHS'
	AND  CMGStatusDesc  ='typical'
	AND  CareType='A'
	GROUP BY  D.FiscalPeriodLong
	, D.FiscalYear
	, ADR.MostRespProviderCode
	, CMGPlusCode +'-' + LEFT(CMGPlusDesc,40)

