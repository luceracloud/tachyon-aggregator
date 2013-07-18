/*
 *  server.cpp
 *
 *    Pushes kstat & dtrace statistics
 *    via ZMQ & protocol buffers to
 *    anyone who will listen.
 *
 *    CREATED:  16 JULY 2013
 *    UPDATED:  18 JULY 2013
 */

#include <string>
#include <iostream>
#include <unistd.h>
#include <cstring>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>


#include<dtrace.h>
#include <zmq.hpp>       // for zmq sockets
#include <kstat.h>       // for kstats
#include "entry.pb.h"    // protocol buffer header
#include "packet.pb.h"   // protocol buffer header
#include "scripts.h"     // script repository


/*
 * Note the existence of:
 */
void sigint_handler ( int s );

int send_message ();
int send_message ( const char *c );
int send_message ( PB_MSG::Packet pckt );

int pfc ( const char *c, int color );
int print_message ( PB_MSG::Packet pckt );

int retreive_kstat ( std::string module, std::string name, std::string statistic, uint64_t *value );
int retreive_dtrace ();

void usage ();
void printfc ( const char *c, int color );

// Dtrace routines
int dtrace_setopts ( dtrace_hdl_t **this_g_dtp );
int dtrace_init ( dtrace_hdl_t **this_g_dtp, std::string s );
static int dtrace_aggwalk(const dtrace_aggdata_t *data, void *arg);


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

    /* Handle signals (for cleanup) */
    signal(SIGINT, sigint_handler);

    /* Splash */
    pfc ( "\n server.cpp\n", 35 );
    pfc ( "\n  Reports dtrace and kstat statistics\n", 36 );
    pfc ("  using Google Protocol Buffers and\n", 37 );
    pfc ("  ZMQ (0MQ) sockets.\n", 33 );

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

  dtrace_hdl_t *g_dtp;

    /* Dtrace init */
    int NUM_SCRIPTS = 1; // this should change
    for (i=0; i<NUM_SCRIPTS; i++) {
      dtrace_init ( &g_dtp, S::DTRACE::trial );
      dtrace_setopts ( &g_dtp );
      if (dtrace_go ( g_dtp )) {
        pfc ( "ERROR: Unable to start dtrace script\n", 31 );
      }
      (void) dtrace_status ( g_dtp );
    }

    /* Kstat init */
    kc = kstat_open();
    uint64_t value;

    pfc ( " ... online!\n", 31 );
    pfc ( "\n Start time was: dd/mm/yyyy | hh:mm:ss\n", 31 );

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

      /* Memory */
      for (i=0; i<S::MEM::number; i++) {
        if (retreive_kstat ( S::MEM::module[i], S::MEM::name[i], S::MEM::statistic[i], &value ) ) {
          printf("Failure in function \"retreive_kstat\" \n");
        } else {
          if (VERBOSE) {
            printf ( "Received: %u for %s\n", value, S::MEM::statistic[i].c_str() );
          }
        } 
      }

      /* Network */
      for (i=0; i<S::NET::number; i++) {
        if (retreive_kstat ( S::NET::module[i], S::NET::name[i], S::NET::statistic[i], &value )) {
          pfc ( "WARN: Failure in function \"retreive_kstat\" \n", 33 );
        } else {
          if (VERBOSE) {
            printf ( "Received: %u for %s\n", value, S::NET::statistic[i].c_str() );
          }
        }
      }
        
      /* Do dtrace look-ups */
      for (i=0; i<1; i++) {

        (void) dtrace_status(g_dtp);
        if (dtrace_aggregate_snap(g_dtp) != 0) {
          pfc ( "WARN: Failed to snap aggregate\n", 33 );
        }
          
        if (dtrace_aggregate_walk_valsorted ( g_dtp, dtrace_aggwalk, NULL ) != 0) {
          pfc ( "WARN: Failed to walk aggregate\n", 33 );
        }
      }

      if (VERBOSE) {
        print_message ( msg_packet );
      }
      send_message ( msg_packet );
      
      sleep (1);
    }

    // Clean stuff up here. Call dtrace close (must execute on being killed (even via sigint))
    return 0;
}


/* Initialize dtrace program (passed by handle and string) */
int dtrace_init ( dtrace_hdl_t **this_dtp, std::string this_prog ) {

  int done = 0;
  int err;
  dtrace_prog_t *prog;
  dtrace_proginfo_t info;

  if ((*this_dtp = dtrace_open(DTRACE_VERSION, 0, &err)) == NULL) {
    pfc ( "ERROR: Failed to init dtrace\n", 31 );
    return -1;  
  }

  if ((prog = dtrace_program_strcompile(*this_dtp, (const char *)this_prog.c_str(),
    DTRACE_PROBESPEC_NAME, 0, 0, NULL)) == NULL) {
    pfc ( "ERROR: Failed to compile program\n", 31 );
    return -1;  
  }

  if (dtrace_program_exec(*this_dtp, prog, &info) == -1) {
    pfc ( "ERROR: Failed to init probes\n", 31 );
    return ( -1 );
  }

  return 0;
}


/* Walk through aggregate and pass data out */
static int dtrace_aggwalk(const dtrace_aggdata_t *data, void *arg)
{
  dtrace_aggdesc_t *aggdesc = data->dtada_desc;
  dtrace_recdesc_t *nrec, *irec;
  char *name;
  int32_t *instance;
  static const dtrace_aggdata_t *count;

  if (count == NULL) {
    count = data;
    return (DTRACE_AGGWALK_NEXT);
  }

  nrec = &aggdesc->dtagd_rec[1];
  irec = &aggdesc->dtagd_rec[2];

  name = data->dtada_data + nrec->dtrd_offset;
  instance = (int32_t *)(data->dtada_data + irec->dtrd_offset);

  printf("%-60s %-20d\n", name, *instance);

  return (DTRACE_AGGWALK_NEXT);
}


/* For the dtrace command line */
int dtrace_setopts ( dtrace_hdl_t **this_g_dtp ) {

  if (dtrace_setopt(*this_g_dtp, "strsize", "32") == -1) {
    pfc ( "failed to set 'strsize'", 31 );
  }
  if (dtrace_setopt(*this_g_dtp, "bufsize", "1k") == -1) {
    pfc ( "failed to set 'bufsize'", 31 );
  }
  if (dtrace_setopt(*this_g_dtp, "aggsortrev", NULL) == -1) {
    pfc ( "failed to set 'aggsortrev'", 31 );
  }
  if (dtrace_setopt(*this_g_dtp, "aggsize", "1M") == -1) {
    pfc ( "failed to set 'aggsize'", 31 );
  }
  if (dtrace_setopt(*this_g_dtp, "aggrate", "2msec") == -1) {
    pfc ( "failed to set 'aggrate'", 31 );
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
int retreive_kstat ( std::string module, std::string name, std::string statistic, uint64_t *value ) {

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
      pfc ( "Not yet implemented : 1\n", 33 );
      break;
    case KSTAT_DATA_INT32:
      *value = knp->value.i32;
      pfc ( "Not yet tested : 2\n", 33 );
      break;
    case KSTAT_DATA_UINT32:
      *value = (uint64_t)knp->value.ui32;
      pfc ( "Not yet tested : 3\n", 33 );
      break;
    case KSTAT_DATA_INT64:
      *value = (uint64_t)knp->value.i64;
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
    memcpy ( (void *)msg.data(), c, strlen(c) );
    socket.send ( msg );
    return 0;
  } catch ( int e ) {
    printf ( "Error number %d\n", e );
    return -1;
  }
}

int send_message ( PB_MSG::Packet pckt ) {
  try {
    std::string pckt_serialized;
    pckt.SerializeToString ( &pckt_serialized );
    zmq::message_t msg ( pckt_serialized.size() );
    memcpy ( (void *)msg.data(), pckt_serialized.c_str(), pckt_serialized.size() );
    socket.send ( msg );
  } catch ( int e ) {
    printf ( "Error number %d\n", e );
    return -1;
  }
  return 0;
}


/*
 * Prints a colorized version of input text
 */
int pfc ( const char *c, int color ) {

  printf ( "\033[00;%dm%s\033[00m", color, c );
  return 0;
}


void sigint_handler ( int s ) {
  pfc ( "\nWARN: dtrace script instances were not cleaned!\nKilling program...\n", 33 );
  exit(1);

}


