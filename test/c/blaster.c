/**
 * ZMQ Blaster
 * Author: Xavier Lange <xrlange@gmail.com>
 *
 * Description: A utility for banging on any
 * zeromq implementation. Provides options
 * for sending varying sized messages with
 * user-defined number of frames.
**/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <unistd.h>

#include <zmq.h>
#include <assert.h>

#include <getopt.h>

static char* version = "0.1.0";

void free_msg(void *data, void *hint);
void send_msg(void* sock, int parts, int size);
void recv_msg(void* sock);

struct operation {
  enum {MODE_SEND, MODE_RECV, MODE_UNSET} mode;
  char* destination;
  int interval;
  int socket_type;
  int parts;
  int size;
} operation;
static void set_default_operation_values(){
  operation.mode        = MODE_UNSET;
  operation.socket_type = ZMQ_PAIR;
  operation.destination = "tcp://0.0.0.0:7890";
  operation.parts = 1;
  operation.size  = 5;
  operation.interval = 0;
}

void parse_arguments(int argc, char**args);
void connect_socket(void* sock, char* target);

int main(int argc, char **args){
  parse_arguments(argc,args);

  int retval;

  void *ctx = zmq_init(1);
  assert(ctx != NULL);

  void *sock = zmq_socket(ctx,operation.socket_type);
  assert(sock != NULL);

  int linger_interval = 1;
  retval = zmq_setsockopt(sock,ZMQ_LINGER,
                          (void*)&linger_interval,
                          sizeof(linger_interval));
  assert(retval == 0);

  switch(operation.mode){
    case MODE_SEND:
      connect_socket(sock,operation.destination);
      send_msg(sock,operation.parts,operation.size);
      break;
    case MODE_RECV:
      recv_msg(sock);
      break;
    case MODE_UNSET:
      printf("bad mode!");
      exit(EXIT_FAILURE);
  }

  sleep(1);

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

void recv_msg(void* sock){
  int retval = 0;
  zmq_msg_t msg;
  zmq_msg_init(&msg);

  retval = zmq_bind(sock,operation.destination);
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

void connect_socket(void* sock, char* target){
  int retval = 0;
  retval = zmq_connect(sock,target);
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
}

void deliver_message(void* sock, int size, int flags);
void send_msg(void* sock, int parts, int size){
  for(int i=0; i < (parts-1); i++){
    deliver_message(sock,size,ZMQ_SNDMORE);
    sleep(operation.interval);
  }
  deliver_message(sock,size,0);

  puts(" done!");
}

void deliver_message(void* sock, int size, int flags){
  zmq_msg_t msg;
  int retval;

  // Prep the message from preset value. ZMQ takes ownership.
  void *data = calloc(size,1);
  assert(data != NULL);
  for(int i=0; i<size; i++){
    ((char*) data)[i] = 'A';
  }

  retval = zmq_msg_init_data(&msg, data, size, free_msg, NULL);
  assert(retval == 0);

  // printf("sending %s\n",zmq_msg_data(&msg));
  retval = zmq_send(sock,&msg,flags);
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
}

void display_usage(){
  printf("The ZMQ Blaster, version %s\n",version);
  puts("usage");
  puts("--mode {pub,sub,pair}");
  puts("--parts numparts : Send a message with numparts frames");
  puts("--destination dst: Connect to destination");
  puts("--interval time  : How long between frames (milliseconds)");
  puts("--size size      : Size of a frame");
  puts("--help           : This usage information");
}
void parse_arguments(int argc, char **args){
  int retval = 0;
  int option_index = 0;
  int c = 0;
  set_default_operation_values();
  static struct option long_options[] = {
    {"socket-type", required_argument, 0, 't'},
    {"mode",  required_argument,       0, 'm'},
    {"parts", required_argument,       0, 'p'},
    {"destination",required_argument,  0, 'd'},
    {"interval",required_argument,     0, 'i'},
    {"size",  required_argument,       0, 's'},
    {"help",  no_argument,             0, 'h'},
    {0, 0, 0, 0}
  };
  while(1){
    c = getopt_long(argc, args, "t:m:p:s:d:i:h?",
                    long_options, &option_index);
    if(c == -1)
      break;
    switch(c){
      case '?':
      case 'h':
        display_usage();
        exit(EXIT_SUCCESS);
        break;
      case 'm':
        if(operation.mode != MODE_UNSET){
          display_usage();
          fprintf(stderr,"ERROR: You can only specify mode once. Exiting.\n");
          display_usage();
          exit(EXIT_FAILURE);
        } else {
          if(strncmp("send",optarg,4) == 0){
            operation.mode = MODE_SEND;
          } else if (strncmp("recv",optarg,4) == 0){
            operation.mode = MODE_RECV;
          } else {
            fprintf(stderr,"ERROR: '%s' is an unsupported mode\n",optarg);
            display_usage();
            exit(EXIT_FAILURE);
          }
        }
        break;
      case 'p':
        retval = sscanf(optarg,"%d",&(operation.parts));
        if(retval < 1){
          fprintf(stderr,"ERROR: Could not parse parts. Must be an unsigned decimal\n");
          display_usage();
          exit(EXIT_FAILURE);
        } else if(operation.parts < 1){
          fprintf(stderr,"ERROR: %d is invalid. Parts must be greater than 0\n",operation.parts);
          display_usage();
          exit(EXIT_FAILURE);
        }
        break;
      case 's':
        retval = sscanf(optarg,"%d",&(operation.size));
        if(retval < 1){
          fprintf(stderr,"ERROR: Could not parse size argument\n");
          exit(EXIT_FAILURE);
        } else if(operation.size < 1){
          fprintf(stderr, "ERROR: %d is invalid. Size must be greater than 0\n",operation.size);
          exit(EXIT_FAILURE);
        }
        break;
      case 't':
        if(strncmp(optarg,"pair",4) == 0){
          operation.socket_type = ZMQ_PAIR;
        } else if(strncmp(optarg,"pub",3) == 0){
          operation.socket_type = ZMQ_PUB;
        } else if(strncmp(optarg,"sub",3) == 0){
          operation.socket_type = ZMQ_SUB;
        } else if(strncmp(optarg,"req",3) == 0){
          operation.socket_type = ZMQ_REQ;
        } else if(strncmp(optarg,"rep",3) == 0){
          operation.socket_type = ZMQ_REP;
        } else if(strncmp(optarg,"dealer",6) == 0){
          operation.socket_type = ZMQ_DEALER;
        } else if(strncmp(optarg,"router",6) == 0){
          operation.socket_type = ZMQ_ROUTER;
        } else if(strncmp(optarg,"push",4) == 0){
          operation.socket_type = ZMQ_PUSH;
        } else if(strncmp(optarg,"pull",4) == 0){
          operation.socket_type = ZMQ_PULL;
        } else if(strncmp(optarg,"xpub",4) == 0){
          operation.socket_type = ZMQ_XPUB;
        } else if(strncmp(optarg,"xsub",4) == 0){
          operation.socket_type = ZMQ_XSUB;
        } else {
          fprintf(stderr,"ERROR: Unknown socket type '%s'\n",optarg);
          exit(EXIT_FAILURE);
        }
        break;
      case 'i':
        retval = sscanf(optarg,"%d",&(operation.interval));
        if(retval < 1){
          fprintf(stderr,"ERROR: Could not parse interval argument\n");
          exit(EXIT_FAILURE);
        } else if(operation.interval < 0){
          fprintf(stderr,"ERROR: '%d' is not a valid interval",operation.interval);
          exit(EXIT_FAILURE);
        }
        break;
      case 'd':
        operation.destination = optarg;
        break;
      default:
        puts("Unknown option");
    }
  }
}