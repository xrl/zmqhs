#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <zmq.h>
#include <assert.h>

#define MSG "DEADBEEF"
void free_msg(void *data, void *hint);

/**
 * This sample program will setup
 * the full ZMQ API, send out a message
 * with the content DEADBEEF and exit
 * with EXIT_SUCCESS.
**/
int main(int argc, char **args){
  int retval;

  void *ctx = zmq_init(1);
  assert(ctx != NULL);

  void *sock = zmq_socket(ctx,ZMQ_PAIR);
  assert(sock != NULL);

  int linger_interval = 1;
  retval = zmq_setsockopt(sock,ZMQ_LINGER,(void*)&linger_interval,sizeof(linger_interval));
  assert(retval == 0);

  retval = zmq_connect(sock,"tcp://localhost:8765");
  // Switch statement because surprisingly ZMQ_PAIR does not
  // support UDP
  if(retval != 0){
    switch(errno){
      case EINVAL:
        puts("zmq_connect EINVAL");
        break;
      case EPROTONOSUPPORT:
        puts("zmq_connect EPROTONOSUPPORT");
        break;
      case ENOCOMPATPROTO:
        puts("zmq_connect ENOCOMPATPROTO");
        break;
      case ETERM:
        puts("zmq_connect ETERM");
        break;
      case ENOTSOCK:
        puts("zmq_connect ENOTSOCK");
        break;
      case EMTHREAD:
        puts("zmq_connect EMTHREAD");
        break;
      default:
        puts("zmq_connect errno default");
    }
    exit(EXIT_FAILURE);
  }

  // Prep the message from preset value. ZMQ takes ownership.
  void *data = calloc(strlen(MSG),1);
  assert(data != NULL);
  memcpy(data,MSG,strlen(MSG));

  zmq_msg_t msg;
  retval = zmq_msg_init_data(&msg, data, strlen(MSG), free_msg, NULL);
  assert(retval == 0);

  retval = zmq_send(sock,&msg,0);
  if(retval != 0){
    switch(errno){
      case EAGAIN:
        puts("zmq_send EAGAIN");
        break;
      case ENOTSUP:
        puts("zmq_send EAGAIN");
        break;
      case EFSM:
        puts("zmq_send EAGAIN");
        break;
      case ETERM:
        puts("zmq_send EAGAIN");
        break;
      case ENOTSOCK:
        puts("zmq_send EAGAIN");
        break;
      case EINTR:
        puts("zmq_send EAGAIN");
        break;
      case EFAULT:
        puts("zmq_send EAGAIN");
        break;
      default:
        puts("zmq_send: errno default");
    }
    exit(EXIT_FAILURE);
  } else {
    puts("sent the message");
  }

  // Cleanup
  printf("cleaning up msg...");
  zmq_msg_close(&msg);
  puts(" done!");

  printf("cleaning up sock...");
  retval = zmq_close(sock);
  assert(retval == 0);
  puts(" done!");

  printf("shutting down zmq...");
  zmq_term(ctx);
  puts(" done!");

  exit(EXIT_SUCCESS); 
}

void free_msg(void *data, void *hint){
  free(data);
}
