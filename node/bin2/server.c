/*
 *    server.c
 *
 *      Potential C version of the server. Will
 *      likely /not/ be used, in favor of C++
 *      version in same directory.
 *
 *    Compile with:
 *      
 *      gcc -ldtrace -lzmq server.c -o server
 *
 *
 *    CREATED:   8 JULY 2013
 *    UPDATED:  16 JULY 2013
 *
 *
 */

#include <stdio.h>
#include <time.h>
#include <zmq.h>

/* Prints a string to stdout and colorizes it */
void printfc (char * s, int color)
{
  printf ("\033[00;%dm%s\033[00m", color, s);
}

/* Returns well-formatted date and/or time */
char * formatted_time ()
{
  time_t now;
  time (&now);
  return ctime (&now);
}

/////////////////
//* Main loop *//
/////////////////
int main (int argc, char * argv[])
{

  /* Init server */
  printfc ("\nServer starting...", 31);

  void * context = zmq_ctx_new ();
  void * responder = zmq_socket (context, ZMQ_PUSH);
  void * push_socket = zmq_socket (context, ZMQ_PUSH);
  int rc2 = zmq_bind (responder, "tcp://*12341");
  int rc = zmq_bind (push_socket, "tcp://*:7211");
  if (rc != 0) return 1;


  printfc ("\n ... online!\n", 31);

  /* Useful information for the user */
  printfc ("\n Start time was: ", 31);
  printfc (formatted_time(), 31);
  printf ("\n\n");

  /* Begin talking to anyone who will listen */

  while (1)
  {
    char buffer[5] = "Hello"; 
    int ierr = zmq_send (push_socket, "Hello", 5, ZMQ_DONTWAIT); 
    
    printf("Error %d\n", ierr);
    printf("Sent %s\n", buffer); 
    sleep(1);
  }

  return 0;
}
