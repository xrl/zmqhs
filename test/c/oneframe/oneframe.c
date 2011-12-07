#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <unistd.h>

#include <zmq.h>
#include <assert.h>

#define MSG "DEADBEEFDEADBEEFDEADBEEFDEADBEEFDEADBEEFDEADBEEFDEADBEEFDEADBEEF"
void free_msg(void *data, void *hint);
void send_msg(void* sock);
void recv_msg(void* sock);

/**
 * This sample program will setup
 * the full ZMQ API, send out a message
 * with the content DEADBEEF and exit
 * with EXIT_SUCCESS.
**/
int main(int argc, char **args){
  if(argc != 2){
    puts("Usage: oneframe send OR oneframe recv");
    exit(EXIT_FAILURE);
  }
  int retval;

  void *ctx = zmq_init(1);
  assert(ctx != NULL);

  void *sock = zmq_socket(ctx,ZMQ_PAIR);
  assert(sock != NULL);

  int linger_interval = 1;
  retval = zmq_setsockopt(sock,ZMQ_LINGER,
                          (void*)&linger_interval,
                          sizeof(linger_interval));
  assert(retval == 0);

  if(strcmp(args[1],"send") == 0){
    send_msg(sock);
  } else if(strcmp(args[1],"recv") == 0){
    recv_msg(sock);
  } else {
    puts("oneframe send OR oneframe recv");
    exit(EXIT_FAILURE);
  }

  printf("cleaning up sock...");
  retval = zmq_close(sock);
  assert(retval == 0);
  puts(" done!");

  printf("shutting down zmq...");
  zmq_term(ctx);
  puts(" done!");

  sleep(1);
  exit(EXIT_SUCCESS); 
}

void free_msg(void *data, void *hint){
  free(data);
}

void recv_msg(void* sock){
  int retval = 0;
  zmq_msg_t msg;
  zmq_msg_init(&msg);

  retval = zmq_bind(sock,"tcp://127.0.0.1:8765");
  if(retval != 0){
    switch(errno){
      case EINVAL:
        puts("zmq_bind EINVAL");
        break;
      case EPROTONOSUPPORT:
        puts("zmq_bind EPROTONOSUPPORT");
        break;
      case ENOCOMPATPROTO:
        puts("zmq_bind ENOCOMPATPROTO");
        break;
      case EADDRINUSE:
        puts("zmq_bind EADDRINUSE");
        break;
      case EADDRNOTAVAIL:
        puts("zmq_bind EADDRNOTAVAIL");
        break;
      case ENODEV:
        puts("zmq_bind ENODEV");
        break;
      case ETERM:
        puts("zmq_bind ETERM");
        break;
      case ENOTSOCK:
        puts("zmq_bind ENOTSOCK");
        break;
      case EMTHREAD:
        puts("zmq_bind EMTHREAD");
        break;
      default:
        puts("zmq_bind errno default");
    }
  }

  puts("Server up. Waiting for message.");

  retval = zmq_recv(sock,&msg,0);
  if(retval != 0){
    switch(errno){
      case EAGAIN:
        puts("zmq_connect EAGAIN");
        break;
      case ENOTSUP:
        puts("zmq_connect ENOTSUP");
        break;
      case EFSM:
        puts("zmq_connect EFSM");
        break;
      case ETERM:
        puts("zmq_connect ETERM");
        break;
      case ENOTSOCK:
        puts("zmq_connect ENOTSOCK");
        break;
      case EINTR:
        puts("zmq_connect EINTR");
        break;
      case EFAULT:
      default:
        puts("zmq_recv errno default");
    }
  }

  printf("oneframe receiver got %.*s\n",(int)zmq_msg_size(&msg),zmq_msg_data(&msg));
  zmq_msg_close(&msg);
}

void send_msg(void* sock){
  int retval = 0;
  retval = zmq_connect(sock,"tcp://127.0.0.1:8765");
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

  printf("sending %s\n",zmq_msg_data(&msg));
  retval = zmq_send(sock,&msg,0);
  if(retval != 0){
    switch(errno){
      case EAGAIN:
        puts("zmq_send EAGAIN");
        break;
      case ENOTSUP:
        puts("zmq_send ENOTSUP");
        break;
      case EFSM:
        puts("zmq_send EFSM");
        break;
      case ETERM:
        puts("zmq_send ETERM");
        break;
      case ENOTSOCK:
        puts("zmq_send ENOTSOCK");
        break;
      case EINTR:
        puts("zmq_send EINTR");
        break;
      case EFAULT:
        puts("zmq_send EFAULT");
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
}