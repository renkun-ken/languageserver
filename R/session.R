#' Single R Session for Session Pool
#'
#' @examples
#' \dontrun{
#' # create a session and bind to a session pool
#' # this step is done in SessionPool$new()
#' session <- Session$new("session id", parent_pool_ptr, pool_name)
#'
#' # task acquire a session from session pool
#' # this step is done in SessionPool$acquire()
#'
#' # start task
#' session$start(target, args)
#'
#' # check session result with is_alive
#' session$is_alive()
#'
#' # when task is running
#' session$is_alive() == TRUE
#'
#' # when task is done
#' session$is_alive() == FALSE
#'
#' # get result when task is done
#' session$get_result()
#'
#' # session must be released when task is done
#' session$release()
#'
#' #' # use restart() to restart session
#' session$restart()
#'
#' # use kill() to restart and release session
#' session$kill()
#'
#' # session will be restarted on error
#' # you can manually “kill” the process in bash to test this behavior
#' }
#' @noRd
Session <- R6::R6Class("Session",
    private = list(
        # parent session poll - dependency injection
        parent_pool = NULL,
        pool_name = NULL,
        # session id - be released by parent
        id = NULL,
        session = NULL,
        # prev task is running
        is_running = FALSE,
        # r session is inited and will run call
        is_ready = FALSE,
        #  before r session is_ready, task is saved in init_task
        init_task = NULL,
        result = NULL,
        # safe call with try catch
        call = function(target, args) {
            ret <- tryCatch({
                private$session$call(target, args)
            }, error = function(e) e)
            if (inherits(ret, "error")) {
                private$result <- ret
                logger$error(private$pool_name, "session call error", private$id, ret)
                # on call error, skip current task and restart
                self$restart()
                private$is_running <- FALSE
            }
        }
    ),
    public = list(
        initialize = function(id, parent_pool, pool_name) {
            private$parent_pool <- parent_pool
            private$pool_name <- pool_name
            private$id <- id

            private$session <- callr::r_session$new(
                callr::r_session_options(
                    system_profile = TRUE, user_profile = TRUE, supervise = TRUE),
                # skip waiting
                wait = FALSE
            )
        },
        finalize = function() {
            private$session$close()
        },
        start = function(target, args) {
            if (private$is_running) {
                # session must stop running before release
                logger$error(private$pool_name, "session race condition: prev function call is running in session", private$id)
            }
            private$is_running <- TRUE
            private$result <- NULL

            if (private$is_ready) {
                private$call(target, args)
            } else {
                private$init_task <- list(target = target, args = args)
            }
        },
        # TRUE for running process, FALSE for compeletion
        is_alive = function() {
            if (!private$is_running) {
                # FALSE for compeletion
                return(FALSE)
            }

            # more about read status code
            # https://callr.r-lib.org/reference/r_session.html#method-read
            data <- private$session$read()
            if (!is.null(data)) {
                # session is not ready
                if (!private$is_ready) {
                    if (data$code == 201) {
                        logger$info(private$pool_name, "session ready", private$id, Sys.time())

                        private$is_ready <- TRUE
                        private$call(private$init_task$target, private$init_task$args)
                        private$init_task <- NULL
                    } else {
                        logger$error(private$pool_name, "session init error", private$id, data)
                        # restart on init error, task is perserved in init_task
                        self$restart()
                    }
                    # continue running
                    return(TRUE)
                }

                # session is ready
                if (data$code == 200) {
                    if (!is.null(data$error)) {
                        private$result <- data$error
                    } else {
                        private$result <- data$result
                    }
                } else {
                    # error data code 301, 500, 501, 502
                    logger$error(private$pool_name, "session read error", private$id, data)
                    # on read error, skip current task and restart
                    self$restart()
                }
                private$is_running <- FALSE
                # FALSE for compeletion
                return(FALSE)
            }

            # TRUE for running process
            return(private$is_running)
        },
        get_result = function() {
            private$result
        },
        close = function(grace = 100) {
            tryCatch({
                private$session$close(grace = grace)
            }, error = function(e) {
                logger$error(private$pool_name, "session close error", ret)
            })
        },
        restart = function(should_release = FALSE) {
            logger$info(private$pool_name, "session restart", private$id)
            self$close()

            private$session <- callr::r_session$new(
                callr::r_session_options(system_profile = TRUE, user_profile = TRUE),
                wait = FALSE
            )
            private$is_ready <- FALSE

            if (should_release) {
                private$is_running <- FALSE
                self$release()
            }
        },
        kill = function() {
            logger$info(private$pool_name, "session kill", private$id)
            self$restart(should_release = TRUE)
        },
        # release current session from session pool
        release = function() {
            private$parent_pool$release(private$id)
        }
    )
)

#' Session Pool with Many R Sessions
#'
#' @examples
#' \dontrun{
#' # create 3 cached r sessions in the pool
#' pool_size <- 3
#' pool_name <- "common"
#' pool <- SessionPool$new(pool_size, pool_name)
#'
#' # check idle_size before acquiring session
#' n <- pool$get_idle_size()
#'
#' # if there are idle sessions, acquire session
#' session <- pool$acquire()
#'
#' # check session is not null and then use session
#' is.null(session)
#'
#' # use session
#' # please read R6 Class `Session` documentation
#'
#' }
#' @noRd
SessionPool <- R6::R6Class("SessionPool",
    private = list(
        pool_name = NULL,
        idle_keys = NULL,
        sessions = NULL,
        pending_size = 0,
        idle_size = 0,
        size = 0,
        target_size = 0
    ),
    public = list(
        initialize = function(size, name) {
            private$pool_name <- name
            private$sessions <- collections::ordered_dict()
            private$idle_keys <- collections::queue()

            if (size > 0) {
                private$size <- size
                private$target_size <- size
                for (i in seq_len(size)) {
                    istr <- as.character(i)
                    private$sessions$set(istr, Session$new(istr, self, name))
                    private$idle_keys$push(istr)
                    private$idle_size <- private$idle_size + 1
                }
            }
        },
        get_idle_size = function() {
            private$idle_size
        },
        acquire = function() {
            if (private$idle_size > 0) {
                private$idle_size <- private$idle_size - 1
                session_id <- private$idle_keys$pop()
                if (!is.null(session_id)) {
                    logger$info(private$pool_name, "session acquired session_id =", session_id, "remain pool size =", private$idle_size)
                    return(private$sessions$get(session_id))
                }
            }
            return(NULL)
        },
        # called by child session
        release = function(id) {
            if (private$target_size < private$size) {
                private$sessions$get(id)$close()
                private$sessions$remove(id)
                private$size <- private$size - 1
            } else {
                private$idle_keys$push(id)
                private$idle_size <- private$idle_size + 1
            }
            logger$info(private$pool_name, "session released session_id =", id, "remain pool size =", private$idle_size, "total size =", private$size, "target size =", private$target_size)
        },
        resize = function(new_size) {
            if (private$target_size != new_size) {
                logger$info("resize", private$pool_name, "from", private$size, "to", new_size)
                private$target_size <- new_size
                if (new_size > private$size) {
                    # add new sessions
                    for (i in seq_len(new_size - private$size)) {
                        istr <- as.character(private$size + i)
                        private$sessions$set(istr, Session$new(istr, self, private$pool_name))
                        private$idle_keys$push(istr)
                        private$idle_size <- private$idle_size + 1
                        private$size <- private$size + 1
                    }
                } else {
                    # remove idle sessions
                    for (i in seq_len(private$size - new_size)) {
                        id <- private$idle_keys$pop()
                        if (!is.null(id)) {
                            logger$info("close session", id, "from", private$pool_name)
                            private$sessions$get(id)$close()
                            private$sessions$remove(id)
                            private$idle_size <- private$idle_size - 1
                            private$size <- private$size - 1
                        }
                    }
                    logger$info(private$pool_name, "current size", private$size, "target size", new_size)
                }
            }
        }
    )
)
