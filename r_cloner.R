rm( list = ls())
library(git2r)
library(devtools)
set.seed(1)
neuroc_packages = c("oro.nifti", "fslr", "neurobase", "ANTsR")
neuroc_table = data.frame(package = neuroc_packages,
                          stringsAsFactors = FALSE)
neuroc_table$commit = sample(letters, size = nrow(neuroc_table))

read_dcf <- function(path) {
  fields <- colnames(read.dcf(path))
  dcf = as.list(read.dcf(path, keep.white = fields, all = TRUE)[1, ])
  return(list(fields = fields,
              dcf = dcf))
}

get_remotes = function(x){
  remotes = x$Remotes[[1]]
  remotes = trimws(remotes)
  if (is.null(remotes)) {
    remotes = ""
  }
  return(remotes)
}

split_remotes <- function(x) {
  trimws(unlist(strsplit(x, ",[[:space:]]*")))
}



parse_one_remote <- function(x) {
  pieces <- strsplit(x, "::", fixed = TRUE)[[1]]
  
  if (length(pieces) == 1) {
    type <- "github"
    repo <- pieces
  } else if (length(pieces) == 2) {
    type <- pieces[1]
    repo <- pieces[2]
  } else {
    stop("Malformed remote specification '", x, "'", call. = FALSE)
  }
  fun <- tryCatch(get(paste0(tolower(type), "_remote"),
                      envir = asNamespace("devtools"), mode = "function", inherits = FALSE),
                  error = function(e) stop("Unknown remote type: ", type, call. = FALSE))
  
  fun(repo)
}

##########################################
# Clone stuff
##########################################
base_path = "/dcl01/smart/data/structural/neuroc/packages"
stub = "muschellij2/extrantsr"
# stub = "jfortin1/RAVEL"
gh_url = base_url = "http://github.com/"
url = paste0(base_url, stub)

pkg = devtools:::parse_git_repo(stub)
pkg = pkg$repo

local_path = file.path(base_path, pkg)
if (!dir.exists(local_path)) {
  repo = clone(url, local_path = local_path)
} else {
  repo <- init(local_path)
  # upstream = 
  res = pull(repo)
  conf = res@conflicts
  if (length(conf) > 0) {
    if (conf) {
      cmd = paste0(paste0("cd ", local_path, "; "),
                  "git checkout --theirs DESCRIPTION")
      system(cmd)
    }
  }
  res = pull(repo)
  conf = res@conflicts
  if (length(conf) > 0) {
    if (conf) {
      stop("Conflicts are not merged correctly")
    }
  }
}

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
