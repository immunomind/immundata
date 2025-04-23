# #
# # TODO: to turorials - it would makes sense to clearly outline what research questions each function/section answers to
# #
#
# # airr_subgroup_abundance
# # airr_abundance Abundance comparing - compare over-representation of immune repertoire subgroups, such as cell clusters or disease-specific receptors
# # Compare abundance of immune receptor subsets across samples
# #
# # TODO: composition of clusters per sample per cohort
# # link: https://www.researchgate.net/figure/Study-design-CDR3-diversity-and-cell-abundance-A-Number-of-samples-and-group_fig1_384211158
# # exploratory analysis / basic statistics. But this is abundance analysis too!!
#
# # receptor_discovery
# #
# # TODO: diff expr-like plots for receptor discovery?
# #
#
# # looks like some exploratory stuff with basic immundata
# # but could be a case for receptor_* as the focus is on the receptor-level features
# #
# # TODO: plots for the top represented receptors
# # link: https://www.nature.com/articles/s42003-023-04447-4
# #
#
# airr_subgroup_abundance <- function(idata, method = c("sum", "mean", "median")) {
#   "sum / mean / median proportion -> compare between groups"
#
#   "sum - how much space occupies the disease-specific receptors"
#
#   "mean / median - on average how overrepresented those are"
# }
#
# receptor_track <- function(idata, targets, seq_options = NULL) {
#
# }
#
# receptor_public <- function(idata) {
#   "
# # We are interested in specific receptors “What are the features or counts for each individual clonotype (or chain)?”
# # We are interested in how repertoires differ “How do clonotypes aggregate across samples or conditions?”
# # We are interested in finding discriminating factors “Which clonotype or repertoire features discriminate between groups?”
# # patient / cohort signatures
#
# -- TRACK --
# 1) [receptor_track] Find receptors of interest
# 1a) List of CDR3 - vector + col to match (screw that, just pass a data frame)
# 1b) List of CDR3 + other columns
# 1c) List of CDR3 alpha + CDR3 beta (+ other columns)
# 2a) Find abundance of found receptors in an ordered repertoires
# 2b) Find abundance of receptors close by distance in an ordered repertoires
# 3a) Visualise via alluvial
# 3b) Compute PCA on abundances and visualise
#
#
# airr_abundance <- function(idata, ...) {
#   filter data by ... - chain/receptor level metadata
#   compute how much abundance they have together
# }
#
# -- SEARCH+PUBLIC --
# 1) [airr_?] use receptors to compare abundance (receptor-level -> repertoire-level)
# 1-1) get a list of receptors
#   - all annotated receptors from a database
#   - some input receptors that important for us somehow (TCR-T)
#   - some selected public receptors, e.g., most abundant receptors from one group
# 1-2) compute abundance of those receptors between groups - mean / median / sum
#   - stat test on the full subrepertoire of those receptors
# 2) [receptor_discovery] use abundance to infer receptors (repertoire-level -> receptor-level)
# 2-1) find receptors which are more abundant in one group in contrast to other
#   - among the receptors of interest / all public receptors, find a way to find the most interesting ones
# 2-2a) Find those interesting ones in database
# 2-2b) Run the scenarii (1) with abundance analysis
#
#
# -- SEARCH --
# 1) Get a database
# 2a) Match CDR3 receptors exact or by distance and annotate
# 2b) Match CDR3-a+b receptors exact by distances and annotate
# 3) Compute some statistics like incidence or abundance per group / analyse inter-group difference
#       -> lools suspiciously similar to the public analysis... (sum up abundances and compare - instead of get receptor-level. So this comes from different directions)
#
# -- PUBLIC --
# done by design -> 1a) Discover receptors which are present in repertoires
# immungraph -> 1b) Discover receptors which are present in repertoires and similar by distance (== create graph and analyse public groups)
# search-like functionality -> 2a) (Somehow) analyse overrepresentation of one receptors comparing to other groups - looks like a biomarker
# 2b) Extract (!) receptors which are significantly overrepresented in some of the group in contrast to other groups
# "
# }
