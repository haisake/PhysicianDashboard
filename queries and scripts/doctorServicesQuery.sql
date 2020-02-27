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

	Need a better IM identification. Made an attempt, but not sure it's right.
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
	   WHEN doc.drname IN ('AHMED, IQBAL', 'SAHIHOLNASAB, VAHID', 'SEHMER, BENJAMIN MICHAEL','SAHIHOLNASAB, SEYED VAHID') THEN 'InternalMedicine'   /* this list is not comprehensive*/
	   WHEN AttendDoctorCode in ('06717','08104','25466','28412','29848','60447','60469','60835','62745','62802','62830','62942','63959','64658','64691','66091','66141','66532','66891','67070','67106','67937','68304','68372','68562','68696','68870','82560','82943','82945') THEN 'InternalMedicine'   /* trying to complete the list; didn't look like it overlaped with hospitalists*/
	   WHEN AttendDoctorCode in ('06717','08104','25466','28412','29848','60447','60469','60835','62745','62802','62830','62942','63959','64658','64691','66091','66141','66532','66891','67070','67106','67937','68304','68372','68562','68696','68870','82560','82943','82945') THEN 'InternalMedicine'   /* trying to complete the list; didn't look like it overlaped with hospitalists; based on cap plan service not ADTC*/ 
	   ELSE 'Other'
END as 'DoctorService'
, CASE WHEN hosp.StartDate is not null THEN hosp.StartDate
	   ELSE FirstCensusDate
END as 'DS_StartDate'
, CASE WHEN hosp.EndDate is not null THEN hosp.EndDate
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
;


