/*
 *    server.c
 *
 *      the server
 *
 *
 *
 *
 *
 *
 *
 *
 *    Compile with:
 *      
 *      gcc -ldtrace -lzmq server.c -o server
 *
 *
 *    CREATED:  8 July 2013
 *
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
  void * responder = zmq_socket (context, ZMQ_REP);
  int rc = zmq_bind (responder, "tcp://*:4200");
  if (rc != 0) return 1;


  printfc ("\n ... online!\n", 31);

  /* Useful information for the user */
  printfc ("\n Start time was: ", 31);
  printfc (formatted_time(), 31);
  printf ("\n\n");



  /* Listen on ports */


  while (1)
  {


    char buffer[10];
    zmq_recv (responder, buffer, 5, 0);
    printf ("Received %s\n", buffer);
    sleep(1);






  }


  /* Get connection */
    // determine type of request


    // run dtrace script

    // run kstat script

    // run cmdline parse


  /* Handle errant disconnects */




  return 0;

}












