
read_dcf <- function(path) {
  fields <- colnames(read.dcf(path))
  dcf = as.list(read.dcf(path, keep.white = fields, all = TRUE)[1, ])
  return(list(fields = fields,
              dcf = dcf))
}


get_pkg_name = function(stub){
  remote = devtools:::github_remote(stub)
  pkg = devtools:::remote_package_name(remote)  
  return(pkg)
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


repo_puller = function(base_path, stub) {
  url = repo_url_maker(stub)
  pkg = get_pkg_name(stub)
  
  local_path = file.path(base_path, stub)
  
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
  return(list(repo = repo,
              local_path = local_path,
              package = pkg)
  )
}


repo_url_maker = function(stub){
  gh_url = base_url = "http://github.com/"
  url = paste0(base_url, stub)
  return(url) 
}
