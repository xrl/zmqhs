#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <unistd.h>

#include <zmq.h>
#include <assert.h>

#include <getopt.h>

#define MSG "ASDFASDFASDFASDFASDFASDFASDF"
static char* version = "0.1.0";
void free_msg(void *data, void *hint);
void send_msg(void* sock);
void recv_msg(void* sock);

struct operation {
  enum {MODE_SEND, MODE_RECV, MODE_UNSET} mode;
  int parts;
  int size;
} operation;
static void set_default_operation_values(){
  operation.mode  = MODE_UNSET;
  operation.parts = 1;
  operation.size  = 5;
}

void parse_arguments(int argc, char**args);

int main(int argc, char **args){
  parse_arguments(argc,args);
  exit(EXIT_FAILURE);

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

  if(strncmp(args[1],"send",4) == 0){
    send_msg(sock);
  } else if(strncmp(args[1],"recv",4) == 0){
    recv_msg(sock);
  } else {
    puts("oneframe send OR oneframe recv");
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

  retval = zmq_bind(sock,"tcp://0.0.0.0:7890");
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
  retval = zmq_connect(sock,"tcp://0.0.0.0:7890");
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

void display_usage(){
  printf("The ZMQ Blaster, version %s\n",version);
  puts("usage");
  puts(" --mode send       : Send data");
  puts(" --mode recv       : Receive data");
  puts(" --parts numparts  : Send a message with numparts frames");
  puts(" --size size       : Size of a frame");
  puts(" --help            : This usage information");
}
void parse_arguments(int argc, char **args){
  int retval = 0;
  int option_index = 0;
  int c = 0;
  set_default_operation_values();
  static struct option long_options[] = {
    {"mode",  required_argument,    0, 'm'},
    {"parts", required_argument,    0, 'p'},
    {"size",  required_argument,    0, 's'},
    {"help",  no_argument,          0, 'h'},
    {0, 0, 0, 0}
  };
  while(1){
    c = getopt_long(argc, args, "m:p:s:h?",
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
          fprintf(stderr,"ERROR: Could not parse size. Must be an unsigned decimal\n");
          exit(EXIT_FAILURE);
        } else if(operation.size < 1){
          fprintf(stderr,"ERROR: %d is invalid. Size must be greater than 0\n",operation.size);
          exit(EXIT_FAILURE);
        }
        break;
      default:
        puts("Unknown option");
    }
  }
}