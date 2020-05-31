#include <sys/ioctl.h>
#include <errno.h>

#include "erl_nif.h"

static ERL_NIF_TERM make_atom(ErlNifEnv *env, const char *name) {
  ERL_NIF_TERM atom;
  if (enif_make_existing_atom(env, name, &atom, ERL_NIF_LATIN1)) {
    return atom;
  }
  return enif_make_atom(env, name);
}

static ERL_NIF_TERM nif_socket_out_queue(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  if (argc != 1) {
    return enif_make_badarg(env);
  }

  ERL_NIF_TERM fd_arg = argv[0];

  int fd = -1;
  if (!enif_get_int(env, fd_arg, &fd)) {
    return enif_make_tuple2(env, make_atom(env, "error"), make_atom(env, "bad_fd"));
  }

  int outq = -1;
  if (ioctl(fd, TIOCOUTQ, &outq) != 0) {
    return enif_make_tuple2(env, make_atom(env, "error"), enif_make_int(env, errno));
  }

  return enif_make_tuple2(env, make_atom(env, "ok"), enif_make_int(env, outq));
}

static ErlNifFunc nif_funcs[] = {
  {"nif_socket_out_queue", 1, nif_socket_out_queue},
};

ERL_NIF_INIT(eradio_server_stream_h, nif_funcs, NULL, NULL, NULL, NULL);
