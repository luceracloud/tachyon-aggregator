/*
 *  server.cpp
 *
 *    Pushes kstat & dtrace statistics
 *    via ZMQ & protocol buffers to
 *    anyone who will listen.
 *
 *    CREATED:  16 JULY 2013
 *    UPDATED:  17 JULY 2013
 */

#include <string>
#include <iostream>
#include <unistd.h>
#include <cstring>

#include <zmq.hpp>       // for zmq sockets
#include <kstat.h>       // for kstats
#include "entry.pb.h"    // protocol buffer header
#include "packet.pb.h"   // protocol buffer header
#include "scripts.h"     // script repository


/*
 * Note the existence of:
 */
int send_message ();
int send_message ( const char *c );
int send_message ( PB_MSG::Packet pckt );

int print_message ( PB_MSG::Packet pckt );

int retreive_kstat ( std::string module, std::string name, std::string statistic, int64_t *value );
int retreive_dtrace ();

void usage ();
void printfc ( const char *c, int color );


/*
 * Globally-existing variables &c.
 */
bool VERBOSE = 0; 
zmq::context_t context (1); 
zmq::socket_t socket (context, ZMQ_PUSH);
PB_MSG::Packet msg_packet;
kstat_ctl_t   * kc;

/*
 * Main loop
 *
 */
int main ( int argc, char **argv ) {

    /* Parse command line */
    int i=0;
    for (i=0; i<argc; i++) {
      if ( VERBOSE ) printf ( "Argument %s\n", argv[i] );
      if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "-V")) {
        printf ( "running in verbose mode\n" );
        VERBOSE = 1;
      }
    }
 
    /* Set up zmq socket */
    socket.bind ("tcp://*:7211");
    send_message ( (const char *)"Init" ); 

    /* Protobuf stuff */
    GOOGLE_PROTOBUF_VERIFY_VERSION;

    /* Dtrace init */
    // TODO

    /* Kstat init */
    kc = kstat_open();
    int64_t value;

    /* Send data in loop */
    while (true) {
      
      msg_packet.Clear(); // Clear the message (no need to reallocate)
      
      msg_packet.set_time ( "25h61m61s" );  // Sample
      msg_packet.set_ticks ( 123 );         // code
      if ( VERBOSE ) {
        print_message ( msg_packet );
      }

      /* Do kstat look-ups */
      int i=0;
      for (i=0; i<S::MEM::number; i++) {
        if ( retreive_kstat ( S::MEM::module[i], S::MEM::name[i], S::MEM::statistic[i], &value ) ) {
          printf("Failure in function \"retreive_kstat\" \n");
        } else {
          printf ( "Received: %d for %s\n", value, S::MEM::statistic[i].c_str() );
        } 
      }

      send_message();
      send_message ( (const char *)"World" );

      sleep (1);
    }
    return 0;
}


/* Print contents of packet message for user */
int print_message( PB_MSG::Packet pckt ) {
  printf ( "String representation of packet:\n" );
  pckt.PrintDebugString(); 
  return 0;
}  


/*
 * Find the associated dtrace statistic
 * using libdtrace methods 
 */
int retreive_dtrace() {

  // TODO

  return -1;
}


/*
 * Find the associated kstat using libkstat
 *   methods. Note the necessity to cast 
 *   strings to raw char *'s.
 */
int retreive_kstat ( std::string module, std::string name, std::string statistic, int64_t *value ) {

  // The profile of kstat_* is as follows:
  //     kstat_lookup ( kc, module, instance, name );
  //     kstat_data_lookup ( ksp, statistic );
  //
  //     module || name can be NULL to ignore
  //     instance can be -1 to ignore

  /* Local variable declarations */ 
  kstat_t         *ksp;
  kstat_named_t   *knp;

  if (strcmp(name.c_str(), "NULL")) {
    ksp = kstat_lookup ( kc, (char *)module.c_str(), -1, (char *)name.c_str() );
  } else {
    ksp = kstat_lookup ( kc, (char *)module.c_str(), -1, NULL );
  }

  kstat_read ( kc, ksp, NULL );
  knp = (kstat_named_t *)kstat_data_lookup ( ksp, (char *)statistic.c_str() ); 

  if (knp == NULL) {
    printf ( "\033[00;31m > libkstat lookup failed\033[00m\n", 33 );
    return -1;
  }

  /* Cast data depending on type */
  switch ( knp->data_type ) {
    case KSTAT_DATA_CHAR:
      // knp->value.c
      printf ( "Not yet implemented : 1\n" );
      break;
    case KSTAT_DATA_INT32:
      *value = knp->value.i32;
      printf ( "Not yet tested : 2\n" );
      break;
    case KSTAT_DATA_UINT32:
      *value = knp->value.ui32;
      printf ( "Not yet tested : 3\n" );
      break;
    case KSTAT_DATA_INT64:
      *value = knp->value.i64;
      break;
    case KSTAT_DATA_UINT64:
      *value = knp->value.ui64;
    break;
  default:
    // We should never end up in here
    printf ( "Something rather peculiar happened.\n" );
    break;
  }

  return 0;
}


/*
 * This function sends a message 
 * over the established zmq socket
 */
int send_message () {
  send_message ( (const char *)"Hello" );
  return 0;
}

int send_message ( const char *c ) {
  try {
    zmq::message_t msg ( strlen(c) );
    memcpy ( (void *) msg.data(), c, strlen(c) );
    socket.send ( msg );
    return 0;
  } catch ( int e ) {
    printf ( "Error number %d\n", e );
    return -1;
  }
}

int send_message ( PB_MSG::Packet pckt ) {
  try {
    std::string message;
    pckt.SerializeToString ( &message );
    zmq::message_t msg ( message.length() );
    memcpy ( (void *) msg.data(), message.c_str(), message.length() ); 
  } catch ( int e ) {
    printf ( "Error number %d\n", e );
    return -1;
  }
  return 0;
}


