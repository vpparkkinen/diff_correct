# diff-correct notes

Objective: check a candidate asf for correctness w.r.t a target csf. Candidate is correct iff candidate and target entail no conflicting difference-making claims

## First pass

- Based on target syntax
- Ran into issue of ambiguities
- Should correctness/compatibility be based on 1) assuming target can be disambiguated, or 2), pure diff-making information?
- Choose 2)

## Second pass

- Based on data returned by the target, not target syntax
- First determine which factors are outcomes; this procedure is valid
- For each candidate disjunct, check that disjunct is free of redundant parts, and that the disjunct is a non-redundant part of the candidate asf. I believe the proceduce for this is valid.
- If the candidate asf is non-redundant, for all candidate disjuncts, generate all subsets of data that could contain a diff-making pair; this procedure may be overly complicated.
- Problem: other factors covary with candidate disjunct and outcome. How to determine which of these are confounders, without actually finding all models that fit the data and comparing the candidate to those?
- Potential confounders are those covariates that are not themselves outcomes.
- For each potential confounder, check whether the potential confounder varies with the candidate outcome when the candidate disjunct is held fixed true. If yes, the covariate is a confounder.
- problem: unable to distinguish confounders from cofactors of the candidate disjunct.

## Third pass

- Based on target syntax
- Assume target is *the* true target, i.e. if ambiguous, assume it can be disambiguated
- Determine non-redundancy of the candidate as in earlier approach
- Based on target syntax, determine paths to the candidate outcome
- Check that there is a path from each candidate factor to the outcome
- For each candidate disjunct, for each factor (conjunct) in a disjunct, generate putative diff-making pairs by subsetting the data so that the candidate factor, the outcome, effects of the candidate factor, causes of the candidate factor and their (other) effects are allowed to vary.
- Problem: If at least the above are not allowed to vary in the putative diff-making pairs, there will be false negatives. But this already produces false positives. (Need to verify this with examples)