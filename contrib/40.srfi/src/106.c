#include "picrin.h"

#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <unistd.h>

#ifndef EWOULDBLOCK
#define EWOULDBLOCK EAGAIN
#endif

struct pic_socket_t {
  int fd;
};

PIC_INLINE void
socket_close(struct pic_socket_t *sock)
{
  if (sock != NULL && sock->fd != -1) {
    close(sock->fd);
    sock->fd = -1;
  }
}

PIC_INLINE void
ensure_socket_is_open(pic_state *pic, struct pic_socket_t *sock)
{
  if (sock != NULL && sock->fd == -1) {
    pic_errorf(pic, "the socket is already closed");
  }
}

static void
socket_dtor(pic_state *pic, void *data)
{
  struct pic_socket_t *sock;

  sock = data;
  socket_close(sock);
  pic_free(pic, data);
}

static const pic_data_type socket_type = { "socket", socket_dtor, NULL };

#define pic_socket_p(o) (pic_data_type_p((o), &socket_type))
#define pic_socket_data_ptr(o) ((struct pic_socket_t *)pic_data_ptr(o)->data)

PIC_INLINE void
validate_socket_object(pic_state *pic, pic_value v)
{
  if (! pic_socket_p(v)) {
    pic_errorf(pic, "~s is not a socket object", v);
  }
}

static pic_value
pic_socket_socket_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);
  return pic_bool_value(pic_socket_p(obj));
}

static pic_value
pic_socket_make_socket(pic_state *pic)
{
  pic_value n, s;
  const char *node, *service;
  int family, socktype, flags, protocol;
  int result;
  struct addrinfo hints, *ai, *it;
  struct pic_socket_t *sock;

  pic_get_args(pic, "ooiiii", &n, &s, &family, &socktype, &flags, &protocol);

  node = service = NULL;
  if (pic_str_p(n)) {
    node = pic_str_cstr(pic, pic_str_ptr(n));
  }
  if (pic_str_p(s)) {
    service = pic_str_cstr(pic, pic_str_ptr(s));
  }

  sock = pic_malloc(pic, sizeof(struct pic_socket_t));
  sock->fd = -1;

  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = family;
  hints.ai_socktype = socktype;
  hints.ai_flags = flags;
  hints.ai_protocol = protocol;

  errno = 0;

  do {
    result = getaddrinfo(node, service, &hints, &ai);
  } while (result == EAI_AGAIN);
  if (result) {
    if (result == EAI_SYSTEM) {
      pic_errorf(pic, "%s", strerror(errno));
    }
    pic_errorf(pic, "%s", gai_strerror(result));
  }

  for (it = ai; it != NULL; it = it->ai_next) {
    int fd;

    fd = socket(it->ai_family, it->ai_socktype, it->ai_protocol);
    if (fd == -1) {
      continue;
    }

    if (it->ai_flags & AI_PASSIVE) {
      int yes = 1;
      if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == 0 &&
          bind(fd, it->ai_addr, it->ai_addrlen) == 0) {
        if (it->ai_socktype == SOCK_STREAM ||
            it->ai_socktype == SOCK_SEQPACKET) {
          /* TODO: Backlog should be configurable. */
          if (listen(fd, 8) == 0) {
              sock->fd = fd;
              break;
          }
        } else {
          sock->fd = fd;
          break;
        }
      }
    } else {
      if (connect(fd, it->ai_addr, it->ai_addrlen) == 0) {
        sock->fd = fd;
        break;
      }
    }

    close(fd);
  }

  freeaddrinfo(ai);

  if (sock->fd == -1) {
    pic_errorf(pic, "%s", strerror(errno));
  }

  return pic_obj_value(pic_data_alloc(pic, &socket_type, sock));
}

static pic_value
pic_socket_socket_accept(pic_state *pic)
{
  pic_value obj;
  int fd = -1;
  struct pic_socket_t *sock, *new_sock;

  pic_get_args(pic, "o", &obj);
  validate_socket_object(pic, obj);

  sock = pic_socket_data_ptr(obj);
  ensure_socket_is_open(pic, sock);

  errno = 0;
  while (1) {
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(struct sockaddr_storage);

    fd = accept(sock->fd, (struct sockaddr *)&addr, &addrlen);

    if (fd < 0) {
      if (errno == EINTR) {
        continue;
      } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
        continue;
      } else {
        pic_errorf(pic, "%s", strerror(errno));
      }
    } else {
      break;
    }
  }

  new_sock = pic_malloc(pic, sizeof(struct pic_socket_t));
  new_sock->fd = fd;
  return pic_obj_value(pic_data_alloc(pic, &socket_type, new_sock));
}

static pic_value
pic_socket_socket_send(pic_state *pic)
{
  pic_value obj;
  struct pic_blob *bv;
  const unsigned char *cursor;
  int flags = 0;
  size_t remain, written;
  struct pic_socket_t *sock;

  pic_get_args(pic, "ob|i", &obj, &bv, &flags);
  validate_socket_object(pic, obj);

  sock = pic_socket_data_ptr(obj);
  ensure_socket_is_open(pic, sock);

  cursor = bv->data;
  remain = bv->len;
  written = 0;
  errno = 0;
  while (remain > 0) {
    ssize_t len = send(sock->fd, cursor, remain, flags);
    if (len < 0) {
      if (errno == EINTR) {
        continue;
      } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
        break;
      } else {
        pic_errorf(pic, "%s", strerror(errno));
      }
    }
    cursor += len;
    remain -= len;
    written += len;
  }

  return pic_int_value(written);
}

static pic_value
pic_socket_socket_recv(pic_state *pic)
{
  pic_value obj;
  struct pic_blob *bv;
  void *buf;
  int size;
  int flags = 0;
  ssize_t len;
  struct pic_socket_t *sock;

  pic_get_args(pic, "oi|i", &obj, &size, &flags);
  validate_socket_object(pic, obj);
  if (size < 0) {
    pic_errorf(pic, "size must not be negative");
  }

  sock = pic_socket_data_ptr(obj);
  ensure_socket_is_open(pic, sock);

  buf = malloc(size);
  if (buf == NULL && size > 0) {
    /* XXX: Is it really OK? */
    pic_panic(pic, "memory exhausted");
  }

  errno = 0;
  do {
    len = recv(sock->fd, buf, size, flags);
  } while (len < 0 && (errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK));

  if (len < 0) {
    free(buf);
    pic_errorf(pic, "%s", strerror(errno));
  }

  bv = pic_make_blob(pic, len);
  memcpy(bv->data, buf, len);
  free(buf);

  return pic_obj_value(bv);
}

static pic_value
pic_socket_socket_shutdown(pic_state *pic)
{
  pic_value obj;
  int how;
  struct pic_socket_t *sock;

  pic_get_args(pic, "oi", &obj, &how);
  validate_socket_object(pic, obj);

  sock = pic_socket_data_ptr(obj);
  if (sock->fd != -1) {
    shutdown(sock->fd, how);
    sock->fd = -1;
  }

  return pic_undef_value();
}

static pic_value
pic_socket_socket_close(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);
  validate_socket_object(pic, obj);

  socket_close(pic_socket_data_ptr(obj));

  return pic_undef_value();
}

static int
xf_socket_read(pic_state PIC_UNUSED(*pic), void *cookie, char *ptr, int size)
{
  struct pic_socket_t *sock;

  sock = (struct pic_socket_t *)cookie;

  return recv(sock->fd, ptr, size, 0);
}

static int
xf_socket_write(pic_state PIC_UNUSED(*pic), void *cookie, const char *ptr, int size)
{
  struct pic_socket_t *sock;

  sock = (struct pic_socket_t *)cookie;

  return send(sock->fd, ptr, size, 0);
}

static long
xf_socket_seek(pic_state PIC_UNUSED(*pic), void PIC_UNUSED(*cookie), long PIC_UNUSED(pos), int PIC_UNUSED(whence))
{
  errno = EBADF;
  return -1;
}

static int
xf_socket_close(pic_state PIC_UNUSED(*pic), void PIC_UNUSED(*cookie))
{
  return 0;
}

static struct pic_port *
make_socket_port(pic_state *pic, struct pic_socket_t *sock, short dir)
{
  struct pic_port *port;

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port), PIC_TT_PORT);
  port->file = xfunopen(pic, sock, xf_socket_read, xf_socket_write, xf_socket_seek, xf_socket_close);
  port->flags = dir | PIC_PORT_BINARY | PIC_PORT_OPEN;
  return port;
}

static pic_value
pic_socket_socket_input_port(pic_state *pic)
{
  pic_value obj;
  struct pic_socket_t *sock;

  pic_get_args(pic, "o", &obj);
  validate_socket_object(pic, obj);

  sock = pic_socket_data_ptr(obj);
  ensure_socket_is_open(pic, sock);

  return pic_obj_value(make_socket_port(pic, sock, PIC_PORT_IN));
}

static pic_value
pic_socket_socket_output_port(pic_state *pic)
{
  pic_value obj;
  struct pic_socket_t *sock;

  pic_get_args(pic, "o", &obj);
  validate_socket_object(pic, obj);

  sock = pic_socket_data_ptr(obj);
  ensure_socket_is_open(pic, sock);

  return pic_obj_value(make_socket_port(pic, sock, PIC_PORT_OUT));
}

static pic_value
pic_socket_call_with_socket(pic_state *pic)
{
  pic_value obj, result;
  struct pic_proc *proc;
  struct pic_socket_t *sock;

  pic_get_args(pic, "ol", &obj, &proc);
  validate_socket_object(pic, obj);

  sock = pic_socket_data_ptr(obj);
  ensure_socket_is_open(pic, sock);

  result = pic_apply1(pic, proc, obj);

  socket_close(sock);

  return result;
}

void
pic_init_socket(pic_state *pic)
{
  pic_deflibrary (pic, "(srfi 106)") {
    pic_defun(pic, "socket?", pic_socket_socket_p);
    pic_defun(pic, "make-socket", pic_socket_make_socket);
    pic_defun(pic, "socket-accept", pic_socket_socket_accept);
    pic_defun(pic, "socket-send", pic_socket_socket_send);
    pic_defun(pic, "socket-recv", pic_socket_socket_recv);
    pic_defun(pic, "socket-shutdown", pic_socket_socket_shutdown);
    pic_defun(pic, "socket-close", pic_socket_socket_close);
    pic_defun(pic, "socket-input-port", pic_socket_socket_input_port);
    pic_defun(pic, "socket-output-port", pic_socket_socket_output_port);
    pic_defun(pic, "call-with-socket", pic_socket_call_with_socket);

#ifdef AF_INET
    pic_define(pic, "*af-inet*", pic_int_value(AF_INET));
#else
    pic_define(pic, "*af-inet*", pic_false_value());
#endif
#ifdef AF_INET6
    pic_define(pic, "*af-inet6*", pic_int_value(AF_INET6));
#else
    pic_define(pic, "*af-inet6*", pic_false_value());
#endif
#ifdef AF_UNSPEC
    pic_define(pic, "*af-unspec*", pic_int_value(AF_UNSPEC));
#else
    pic_define(pic, "*af-unspec*", pic_false_value());
#endif

#ifdef SOCK_STREAM
    pic_define(pic, "*sock-stream*", pic_int_value(SOCK_STREAM));
#else
    pic_define(pic, "*sock-stream*", pic_false_value());
#endif
#ifdef SOCK_DGRAM
    pic_define(pic, "*sock-dgram*", pic_int_value(SOCK_DGRAM));
#else
    pic_define(pic, "*sock-dgram*", pic_false_value());
#endif

#ifdef AI_CANONNAME
    pic_define(pic, "*ai-canonname*", pic_int_value(AI_CANONNAME));
#else
    pic_define(pic, "*ai-canonname*", pic_false_value());
#endif
#ifdef AI_NUMERICHOST
    pic_define(pic, "*ai-numerichost*", pic_int_value(AI_NUMERICHOST));
#else
    pic_define(pic, "*ai-numerichost*", pic_false_value());
#endif
/* AI_V4MAPPED and AI_ALL are not supported by *BSDs, even though they are defined in netdb.h. */
#if defined(AI_V4MAPPED) && !defined(BSD)
    pic_define(pic, "*ai-v4mapped*", pic_int_value(AI_V4MAPPED));
#else
    pic_define(pic, "*ai-v4mapped*", pic_false_value());
#endif
#if defined(AI_ALL) && !defined(BSD)
    pic_define(pic, "*ai-all*", pic_int_value(AI_ALL));
#else
    pic_define(pic, "*ai-all*", pic_false_value());
#endif
#ifdef AI_ADDRCONFIG
    pic_define(pic, "*ai-addrconfig*", pic_int_value(AI_ADDRCONFIG));
#else
    pic_define(pic, "*ai-addrconfig*", pic_false_value());
#endif
#ifdef AI_PASSIVE
    pic_define(pic, "*ai-passive*", pic_int_value(AI_PASSIVE));
#else
    pic_define(pic, "*ai-passive*", pic_false_value());
#endif

#ifdef IPPROTO_IP
    pic_define(pic, "*ipproto-ip*", pic_int_value(IPPROTO_IP));
#else
    pic_define(pic, "*ipproto-ip*", pic_false_value());
#endif
#ifdef IPPROTO_TCP
    pic_define(pic, "*ipproto-tcp*", pic_int_value(IPPROTO_TCP));
#else
    pic_define(pic, "*ipproto-tcp*", pic_false_value());
#endif
#ifdef IPPROTO_UDP
    pic_define(pic, "*ipproto-udp*", pic_int_value(IPPROTO_UDP));
#else
    pic_define(pic, "*ipproto-udp*", pic_false_value());
#endif

#ifdef MSG_PEEK
    pic_define(pic, "*msg-peek*", pic_int_value(MSG_PEEK));
#else
    pic_define(pic, "*msg-peek*", pic_false_value());
#endif
#ifdef MSG_OOB
    pic_define(pic, "*msg-oob*", pic_int_value(MSG_OOB));
#else
    pic_define(pic, "*msg-oob*", pic_false_value());
#endif
#ifdef MSG_WAITALL
    pic_define(pic, "*msg-waitall*", pic_int_value(MSG_WAITALL));
#else
    pic_define(pic, "*msg-waitall*", pic_false_value());
#endif

#ifdef SHUT_RD
    pic_define(pic, "*shut-rd*", pic_int_value(SHUT_RD));
#else
    pic_define(pic, "*shut-rd*", pic_false_value());
#endif
#ifdef SHUT_WR
    pic_define(pic, "*shut-wr*", pic_int_value(SHUT_WR));
#else
    pic_define(pic, "*shut-wr*", pic_false_value());
#endif
#ifdef SHUT_RDWR
    pic_define(pic, "*shut-rdwr*", pic_int_value(SHUT_RDWR));
#else
    pic_define(pic, "*shut-rdwr*", pic_false_value());
#endif
  }
}
