# diff-correct notes

Objective: check a candidate asf for correctness w.r.t a target csf. Candidate is correct iff candidate and target entail no conflicting difference-making claims

## First pass

- Based on target syntax
- Ran into issue of ambiguities
- Should correctness/compatibility be based on 1) assuming target can be disambiguated, or 2), pure diff-making information?
- Choose 2)

## Second pass

- Based purely on data returned by the target
- First determine which factors are outcomes; this procedure is valid
- For each candidate disjunct, check that disjunct is free of redundant parts, and that the disjunct is a non-redundant part of the candidate asf. I believe the proceduce for this is valid.
- If candidate asf is non-redundant, generate all subsets of data that could contain a diff-making pair; this procedure may be overly complicated.
- Problem: other factors covary with candidate disjunct and outcome. How to determine which of these are confounders, based on only the data?
- Potential confounders are those covariates that are not themselves outcomes.
- One approach: For each potential confounder, check whether the potential confounder varies with the candidate outcome when the candidate disjunct is held fixed true. If yes, the covariate is a confounder.
- problem: unable to distinguish confounders from cofactors of the candidate disjunct.