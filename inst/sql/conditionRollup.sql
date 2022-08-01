with disease as ( -- define disease categories similar to ICD10 Chapters
  select 1 as precedence, 'Blood disease' as category_name, 440371 as category_id union
  select 1, 'Blood disease', 443723 union
  select 2, 'Injury and poisoning', 432795 union
  select 2, 'Injury and poisoning', 442562 union
  select 2, 'Injury and poisoning', 444363 union
  select 3, 'Congenital disease', 440508 union
  select 4, 'Pregnancy or childbirth disease', 435875 union
  select 4, 'Pregnancy or childbirth disease', 4088927 union
  select 4, 'Pregnancy or childbirth disease', 4154314 union
  select 4, 'Pregnancy or childbirth disease', 4136529 union
  select 5, 'Perinatal disease', 441406 union
  select 6, 'Infection', 432250 union
  select 7, 'Neoplasm', 438112 union
  select 8, 'Endocrine or metabolic disease', 31821 union
  select 8, 'Endocrine or metabolic disease', 4090739 union
  select 8, 'Endocrine or metabolic disease', 436670 union
  select 9, 'Mental disease', 432586 union
  select 10, 'Nerve disease and pain', 376337 union
  select 10, 'Nerve disease and pain', 4011630 union
  select 11, 'Eye disease', 4038502 union
  select 12, 'ENT disease', 4042836 union
  select 13, 'Cardiovascular disease', 134057 union
  select 14, 'Respiratory disease', 320136 union
  select 15, 'Digestive disease', 4302537 union
  select 16, 'Skin disease', 4028387 union
  select 17, 'Soft tissue or bone disease', 4244662 union
  select 17, 'Soft tissue or bone disease', 433595 union
  select 17, 'Soft tissue or bone disease', 4344497 union
  select 17, 'Soft tissue or bone disease', 40482430 union
  select 17, 'Soft tissue or bone disease', 4027384 union
  select 18, 'Genitourinary disease', 4041285 union
  select 19, 'Iatrogenic condition', 4105886 union
  select 19, 'Iatrogenic condition', 4053838
)
select distinct -- get the disease category with the lowest (best fitting) precedence, or assign 'Other Condition'
  concept_id as condition_id, concept_name as condition_name,
  first_value(coalesce(category_id, 0)) over (partition by concept_id order by precedence nulls last) as category_id, 
  first_value(coalesce(category_name, 'Other Condition')) over (partition by concept_id order by precedence nulls last) as category_name
from @cdmDatabaseSchema.concept
left join ( -- find the approprate disease category, if possible
  select descendant_concept_id, category_id, category_name, precedence
  from @cdmDatabaseSchema.concept_ancestor
  join disease on ancestor_concept_id=category_id
) d on descendant_concept_id=concept_id
where concept_id in (@conceptIds) -- place here the concept_ids you want to roll up (have to be standard SNOMED)
;
