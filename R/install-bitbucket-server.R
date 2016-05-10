install_bitbucket_server <- function(repo, ref = "master", subdir = NULL,
  auth_token = bitbucket_server_pat(), host = bitbucket_server_host(),
  quiet = FALSE, ...) {

  remotes <- lapply(repo, bitbucket_server_remote, ref = ref, subdir = subdir,
    auth_token = auth_token, host = host)
  install_remotes(remotes, ...)

}


bitbucket_server_remote <- function(repo, ref = NULL, subdir = NULL,
  auth_token = bitbucket_server_pat(), host = bitbucket_server_host(),
  sha = NULL) {

  meta <- parse_bitbucket_server_repo(repo)
  meta <- bitbucket_server_resolve_ref(meta$ref %||% ref, meta)

  if (is.null(meta$username)) {
    stop("Unknown username.")
  }

  remote("bitbucket_server",
    host = host,
    repo = meta$repo,
    subdir = meta$subdir %||% subdir,
    username = meta$username,
    ref = meta$ref %||% ref,
    sha = sha,
    auth_token = auth_token
  )
}


