declare @censusdate datetime,@counter int,@countermax int
set @censusdate=dateadd(dd,-(365+30+22),dateadd(mi,900,cast(cast(getdate() as date) as datetime)))
set @counter=1
set @countermax=365+30+22

while @counter<=@countermax
begin
insert into #census
SELECT @censusdate 
      ,'15:00' as censustime
      ,[AssignmentID]
      ,[EncounterID]
      ,[AssignmentDate] as AdmitDate
      ,[lu_WardID] as Unit
      ,case when [lu_SpecialtyID] ='ALC' then 'ALC' else 'Acute' end as ALCFlag
      ,d.DrService
      ,[lu_HealthCareProfessionalID] as DrCode
  ,isnull([lu_BedID],'UNKNOWN') as Bed
   , case when [lu_HealthCareProfessionalID] in (select drcode COLLATE DATABASE_DEFAULT from #drservice where drservice='hospitalist') and [lu_WardID]='R4N' then [lu_HealthCareProfessionalID] + '^^R4N' else [lu_HealthCareProfessionalID] end as LookupDrCodeUnit
FROM [CapPlan_RHS].[dbo].[Assignments] a
left outer join #drservice d 
on case when [lu_HealthCareProfessionalID] in (select drcode COLLATE DATABASE_DEFAULT from #drservice where drservice COLLATE DATABASE_DEFAULT ='hospitalist') and [lu_WardID] ='R4N' then [lu_HealthCareProfessionalID] + '^^R4N' else [lu_HealthCareProfessionalID] end=d.drcode COLLATE DATABASE_DEFAULT
where assignmentdate<@censusdate
and (assignmentenddate >= @censusdate or assignmentenddate is null)
and lu_wardid not like 'm[0-9]%'
and lu_wardid not in ('rhbcb','ramb')

set @censusdate=dateadd(dd,1,@censusdate)
set @counter=@counter+1
end

select * from #census

IF OBJECT_ID('tempdb..#census2') IS NOT NULL DROP TABLE #census2
select censusdate,count(*) as census 
into #census2
from #census
where drservice='Internal Medicine'
group by censusdate

select * from #census2
