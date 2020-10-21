# diff_correct

The aim of ```diff_correct()``` is to provide an alternative to ```cna::is.submodel()``` for correctness checking models returned 
by CNA, or other configurational causal modeling (CCM) method such as QCA, in an inverse search trial with known search target, or more 
generally, to check whether two non-identical CCM models are compatible with each other in a correctness-preserving sense. The motivation for this is that 
two CCM models may be compatible with each other in the sense that neither model entails a difference-making relation between some factors A and B such that the other model includes A and B but does not entail a difference-making relationship between them, without being being related as sub- and supermodel. 
This is the case for example between models that differ only in the granularity at which they describe some causal relevance relations --
i.e. some pairs of factors linked by a causal chain in one model are represented as a directly causally related in another model. In such a case
all the difference-making relations entailed by the coarse grained model are also entailed by the fine grained model, the latter merely provides more detail.

To cover such cases (and others where two models agree on all difference-making claims they entail), these functions test the compatibility between 
a "target" (ground truth) model m1 and a candidate model m2 by searching for appropriate difference-making evidence for all causal ascriptions made
by the candidate in an ideal data set simulated from the target.

The current implementation expects that the factors included in the candidate model are a subset of those included in the target, and hence only applies 
to pairs of models where the candidate is more coarse grained or omits detail compared the target, not to pairs of models where the candidate in addition includes
some factors not included in the target.

