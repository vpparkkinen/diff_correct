# diff_correct

The aim of ```diff_correct()``` is to provide an alternative to ```cna::is.submodel()``` for checking whether a candidate model returned 
in an inverse search trial is correct with respect to the stipulated ground truth model that was used to generate the analyzed data, or more 
generally, to check whether two non-identical models are compatible with each other in a correctness preserving sense. The motivation for this is that 
two non-identical models may be compatible with each other in the sense that they do not disagree about any difference-making relations that they entail 
about factors included in both models, without being being related as sub- and supermodel. 
This is the case for example when a multi-step causal chain linking two factors in one model is represented as a direct causal relation in another model.

To cover such cases (and others where two models agree on all difference-making claims they entail), these functions test the compatibility between 
a "target" (ground truth) model m1 and a candidate model m2 by searching for appropriate difference-making evidence for all causal ascriptions made
by the candidate in an ideal data set simulated from the target.

The current implementation expects that the factors included in the candidate model are a subset of those included in the target, and hence only applies 
to pairs of models where the candidate is more coarse grained or lacks detail included in the target, not to pairs of models where the candidate also includes
additional factors omitted by the target.

