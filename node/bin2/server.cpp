/*
 *  server.cpp
 *
 *    Pushes kstat & dtrace statistics
 *    via ZMQ & protocol buffers to
 *    anyone who will listen.
 *
 *    CREATED:  16 JULY 2013
 *    UPDATED:  16 JULY 2013
 */

#include <zmq.hpp>
#include <string>
#include <iostream>
#include <unistd.h>
#include <kstat.h>

#include "entry.pb.h"    // protocol buffer header

/*
 * Note the existence of:
 */

int send_message();
int retrieve_dtrace();
int retrieve_kstat();

/*
 * Globally-existing variables &c.
 */
zmq::context_t context (1);
zmq::socket_t socket (context, ZMQ_PUSH);

kstat_ctl_t   * kc;


/*
 * "int main"
 */
int main () {
    /* Set up zmq socket */
    socket.bind ("tcp://*:7211");

    /* Protobuf stuff */
    GOOGLE_PROTOBUF_VERIFY_VERSION;

    /* Set up kstat */
    kstat_t       * ksp;
    kstat_io_t      kio;
    kstat_named_t * knp;
    kc = kstat_open();

    /* Send data in loop */
    while (true) {
        send_message();
        zmq::message_t request;
        
        sleep (1);

        zmq::message_t reply (5);
        memcpy ((void *) reply.data (), "World", 5);
        socket.send (reply);
    }
    return 0;
}


/*
 * Find the associated dtrace statistic
 * using libdtrace methods 
 */
int retreive_dtrace() {

  return -1;
}

/*
 * Find the associated kstat using the
 * libkstat methods
 */
int retreive_kstat() {
  kstat_t       * ksp;
  kstat_named_t * knp;

  ksp = kstat_lookup (kc, "cpu_info", -1, NULL);
  kstat_read (kc, ksp, NULL);
  
  knp = kstat_data_lookup(ksp, "clock_MHz");   
  printf("CPU speed of system is ");  
  //my_named_display(ksp->ks_name, ksp->ks_class, knp);  
  printf("n");  

  return -1;
}


/*
 * This function will send the message (prepacked)
 * over the established zmq socket
 */
int send_message() {
  try {
    zmq::message_t reply (5);
    memcpy ((void *) reply.data (), "Hello", 5);
    socket.send( reply );
    return 0;
  } catch (int e) {
    printf("Error number %d\n", e);
    return -1;
  }
}


