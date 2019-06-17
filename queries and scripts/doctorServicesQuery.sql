/*
Purpose: To pull in doctor codes and doctor services identified as the following
- Hospitalist
- Internal Medicine
- ACE – Hospitalist
- ACE – GIM
These groupings must be mutulay exclusive.
Hospitalist are maintained in DSSI.dbo.
 
Author: Hans Aisake
Date Created: June 6, 2019
Date Modified:
Comments:
	The base idea came from Peter's query. I removed the classification as Hospitalist, GIM, etc... from this query.
	Those linkages have to be found elsewhere in other databses like DSSI if anywhere at all.
	Hard coding a mapping into this query was not desired, but we will have sufficient foundations to do it later
 */
 
 WITH richmondDocs as(
	  SELECT AttendDoctorName
	  , AttendDoctorCode
	 , MIN(censusdate) as 'FirstCensusDate'
	 , MAX(CensusDate) as 'LastCensusDate'
	  FROM ADTCMart.ADTC.vwCensusFact 
	 WHERE FacilityLongName = 'richmond hospital'
	 AND AttendDoctorName not in ('Invalid')
	 GROUP BY AttendDoctorName
	 , AttendDoctorCode
 )

SELECT AttendDoctorName as 'DrName'
, 'P' + AttendDoctorCode as 'DrCode'
, CASE WHEN hosp.HospitalistName is not null THEN 'Hospitalist'
	   WHEN doc.drname IN ('AHMED, IQBAL', 'SAHIHOLNASAB, VAHID', 'SEHMER, BENJAMIN MICHAEL') THEN 'Internal Medicine' 
	   WHEN doc.drname IN ('HALJAN, GREGORY JOSEPH','LAU, EDGAR SENG TEONG','BRUCE, JENNIFER ELIZABETH','WONG, JUSTIN KAI FAI') THEN 'Critical Care Medicine'
	   ELSE serv.DADDescription
END as 'DoctorService'
, CASE WHEN hosp.StartDate is not null THEN hosp.StartDate
	   WHEN doc.drname IN ('AHMED, IQBAL', 'SAHIHOLNASAB, VAHID', 'SEHMER, BENJAMIN MICHAEL') THEN FirstCensusDate
	   WHEN doc.drname IN ('HALJAN, GREGORY JOSEPH','LAU, EDGAR SENG TEONG','BRUCE, JENNIFER ELIZABETH','WONG, JUSTIN KAI FAI') THEN FirstCensusDate
	   ELSE FirstCensusDate
END as 'DS_StartDate'
, CASE WHEN hosp.EndDate is not null THEN hosp.EndDate
	   WHEN doc.drname IN ('AHMED, IQBAL', 'SAHIHOLNASAB, VAHID', 'SEHMER, BENJAMIN MICHAEL') THEN LastCensusDate
	   WHEN doc.drname IN ('HALJAN, GREGORY JOSEPH','LAU, EDGAR SENG TEONG','BRUCE, JENNIFER ELIZABETH','WONG, JUSTIN KAI FAI') THEN LastCensusDate
	   ELSE LastCensusDate
END as 'DS_EndDate'	  
FROM richmondDocs	/*for all richmond docs*/
LEFT JOIN ADTCMart.dim.Doctor as doc
ON richmondDocs.AttendDoctorName=doc.DrName	/*same name*/
LEFT JOIN DSSI.dbo.hospitalists as hosp
ON doc.drname = HospitalistName	/*name is the same*/
AND hosp.FacilityLongName='richmond hospital'	/*richmond only*/
LEFT JOIN ADTCMart.dim.[Service] as serv
ON serv.ServiceID = doc.DrServiceID
/* DoctorService in ('Internal Medicine','Hospitalist') */


						 