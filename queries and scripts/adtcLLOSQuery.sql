
	/*
	Purpose: To compute the LLOS census of currently waiting patients by fiscal period end.
	Fundamentally, It seams like we are trying our best to just focus on true inpatients in true inpatient units.
	It's not clear though what the expected definition is.
	For the physician dashboard

	The P4P query applies filters in a way that doesn't make this quite clear so I've switched it around here.

	Author: Hans Aisake
	Date Created: June 14, 2018
	Date Updated: June 14, 2018
	Inclusions/Exclusions:
		- true inpatient records only
		- excludes newborns
	Comments:
		I took the base query for indicator 471 for the BSC version FROM Emily, but modified it to be richmond specific. The numbers might not match perfectly.
		Targets are absed on the BSI fiscal period targets. If it is a snapshot it shouldn't be directly comprable to the thursday week end.
	
	*/

	/* build a table of fiscal period dates */
	WITH reportFP AS (
		SELECT distinct TOP 26  fiscalyear, fiscalperiodlong, fiscalperiodstartdate, fiscalperiodenddate
		FROM ADTCMart.dim.[Date]
		WHERE FiscalPeriodEndDate < DATEADD(day, -2, GETDATE())	/* FP over for at least a year */
		ORDER BY fiscalperiodlong desc
	)

	, LLOSData as (
		SELECT  -- Identify number of days greater than 30
		CASE WHEN ADTC.AdmittoCensusDays > 210 THEN 180
			 ELSE ADTC.AdmittoCensusDays-30 
		END as 'LLOSDays'
		, 'P' + AttendDoctorCode as 'DrCode'
		, D.fiscalPeriodLong
		, ADTC.FacilityLongName
		, ADTC.PatientId
		FROM ADTCMart.[ADTC].[vwCensusFact] as ADTC
		INNER JOIN reportFP as D 
		ON ADTC.CensusDate  BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate --pull census for the fiscal period
		WHERE ADTC.age>1				--P4P standard definition to exclude newborns.
		AND ADTC.AdmittoCensusDays > 30	--only need the LLOS patients, I'm not interested in proportion of all clients
		AND (ADTC.HealthAuthorityName = 'Vancouver Coastal' -- only include residents of Vancouver Coastal
		OR (ADTC.HealthAuthorityName = 'Unknown BC' AND (ADTC.IsHomeless = '1' OR ADTC.IsHomeless_PHC = '1'))) -- Include Unknown BC homeless population
		AND ADTC.[Site] ='rmd'									--only include census at Richmond
		AND ADTC.AccountType in ('I', 'Inpatient', '391')		--the code is different for each facility. Richmond is Inpatient
		AND ADTC.AccountSubtype in ('Acute')					--the true inpatient classification is different for each site. This is the best guess for Richmond
		--didn't have filters for patient service. that might be wrong on my part
	)

	SELECT 	DrCode
	, FiscalPeriodLong
	, SUM(LLOSDays) as 'LLOSDays'
	, COUNT(distinct PatientID) as 'NumLLOSPatients'
	FROM LLOSData
	GROUP BY DrCode
	, FiscalPeriodLong

	