with drug as ( -- define disease categories similar to ICD10 Chapters
  select row_number() over (order by concept_code) as precedence, concept_name as category_name, concept_id as category_id
  from @cdmDatabaseSchema.concept
  where vocabulary_id='ATC' and concept_class_id='ATC 2nd'
)
select distinct -- get the disease category with the lowest (best fitting) precedence, or assign 'Other Condition'
  concept_id as drug_id, concept_name as drug_name,
  first_value(coalesce(category_id, 0)) over (partition by concept_id order by precedence nulls last) as category_id,
  first_value(coalesce(category_name, 'Other Drug')) over (partition by concept_id order by precedence nulls last) as category_name
from @cdmDatabaseSchema.concept
left join ( -- find the approprate drug category, if possible
  select descendant_concept_id, category_id, category_name, precedence
  from @cdmDatabaseSchema.concept_ancestor
  join drug on ancestor_concept_id=category_id
) d on descendant_concept_id=concept_id
where concept_id in (@conceptIds) -- place here the concept_ids you want to roll up (have to be standard SNOMED)
;