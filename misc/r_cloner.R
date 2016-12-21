rm( list = ls())
library(git2r)
library(devtools)
source("worker_functions.R")
set.seed(1)
neuroc_packages = c("oro.nifti", "fslr", "neurobase", "ANTsR")
neuroc_table = data.frame(package = neuroc_packages,
                          stringsAsFactors = FALSE)
neuroc_table$commit = sample(letters, size = nrow(neuroc_table))


##########################################
# Clone stuff
##########################################
base_path = "/dcl01/smart/data/structural/neuroc/packages"
stub = "emsweene/SuBLIME_package"
# stub = "jfortin1/RAVEL"
# url = paste0(base_url, stub)

res = repo_puller(base_path = base_path, stub = stub)

pkg = res$package
local_path = res$local_path
repo = res$repo
  
pkg = get_pkg_name(stub)
local_path = file.path(base_path, pkg)

repo = repo_puller(local_path = local_path,
                   url = url)

if (!"neuroc" %in% remotes(repo)) {
  neuroc_url = paste0(gh_url, "neuroconductor", "pkg")
  remote_add(repo = repo, "neuroc", neuroc_url)
}

deps = dev_package_deps(pkg = local_path,
                        dependencies = TRUE)
dep_pack = deps$package
dcf = file.path(local_path, "DESCRIPTION")
orig_dcf = tempfile()
file.copy(dcf, orig_dcf)
rres = read_dcf(dcf)
fields = rres$fields
res = rres$dcf
nres = names(res)
if (!("biocViews" %in% nres)) {
  res$biocViews = ""
}
remotes = get_remotes(res)
remotes = split_remotes(remotes)
if (length(remotes) == 0) {
  remotes = ""
}
###################################################
# See if ANY packages are neuroc ones
# If not remote exists, add it
# If one exists, change user to neuroconductor and commit to that
# from the table
################################################
neuro_deps = neuroc_table[ 
  neuroc_table$package %in% dep_pack, , drop = FALSE
  ]

if (nrow(neuro_deps) > 0) {
  # no remotes
  if (length(remotes) == 1 &
      all(remotes == "")) {
    fixed = paste0("neuroconductor/",
                     neuro_deps$package,
                     "@", neuro_deps$commit
    )
  } else {
    parsed = lapply(remotes, parse_one_remote)
    pack_with_remote = sapply(parsed, function(x) {
      x$repo
    })
    names(remotes) = names(parsed) = pack_with_remote
    keep_these = setdiff(pack_with_remote, neuro_deps$package)
    remotes = remotes[keep_these]
    
    # need_to_add = setdiff(neuro_deps$package, pack_with_remote)
    # if (length(need_to_add) > 0) {
      adders = paste0("neuroconductor/",
                      neuro_deps$package,
                     "@", neuro_deps$commit)      
      names(adders) = neuro_deps$package
      remotes = c(remotes, adders)
    # }
    parsed = lapply(remotes, parse_one_remote)
    
    remote_repos = lapply(remotes, devtools:::parse_git_repo)
    
    fixed = sapply(parsed, function(x) {
      xx = paste(x$username, x$repo, x$subdir, sep = "/")
      xx = gsub("/$", "", xx)
      xx = gsub("//", "/", xx)
      xx = paste0(xx, "@", x$ref)
      xx = gsub("/$", "", xx)
      return(xx)
    })
  }
  fixed_remotes = paste(fixed, collapse = ", ")
  res$Remotes = fixed_remotes
} 
res = as.data.frame(res, stringsAsFactors = FALSE)

write.dcf(x = res, file = dcf, keep.white = fields)


# save(repo, dcf)
##############################
# R COMMAND CHECK
##############################

add(repo, path = dcf)
stat = status(repo)
staged = stat$staged$modified
if (length(staged) > 0) { 
  commit(repo, message = "neuroc_ready")
}

### add biocViews


check_stat = devtools::check(pkg = local_path)
errors = check_stat$errors
warns = check_stat$warnings
# checker = errors
checker = c(errors, warns)
if (length(checker) > 0) {
  file.copy(from = orig_dcf, to = dcf, overwrite = TRUE)
}
