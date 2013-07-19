/*
 *  server.cpp
 *
 *    Pushes kstat & dtrace statistics
 *    via ZMQ & protocol buffers to
 *    anyone who will listen.
 *
 *    CREATED:  16 JULY 2013
 *    UPDATED:  19 JULY 2013
 */

#include <string>
#include <iostream>
#include <unistd.h>
#include <cstring>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <sstream>

#include <dtrace.h>
#include <zmq.hpp>       // for zmq sockets
#include <kstat.h>       // for kstats
#include "entry.pb.h"    // protocol buffer header
#include "packet.pb.h"   // protocol buffer header
#include "scripts.h"     // script repository

typedef void (* SetFunctions) (uint64_t value);
SetFunctions setFunc[] = {
};

/*
 * Note the existence of:
 */
void sigint_handler ( int s );
int pfc ( const char *c, int color );
int print_message ( PB_MSG::Packet pckt );
void usage ();
void printfc ( const char *c, int color );

int send_message ();
int send_message ( const char *c );
int send_message ( PB_MSG::Packet pckt );

int retreive_kstat ( std::string module, std::string name, std::string statistic, int instance, uint64_t *value );
int formatted_print ( const dtrace_recdesc_t *rec, caddr_t addr ); 
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
kstat_ctl_t *kc;

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
    
    /* Dtrace init */
    dtrace_hdl_t *g_dtp[S::DTRACE::number]; 
    for (i=0; i<S::DTRACE::number; i++) {
      dtrace_init ( &g_dtp[i], S::dtrace[i] );
      dtrace_setopts ( &g_dtp[i] );
      if (dtrace_go ( g_dtp[i] )) {
        pfc ( "ERROR: Unable to start dtrace script\n", 31 );
      }
      (void) dtrace_status ( g_dtp[i] );
    }

    /* Kstat init */
    kc = kstat_open();
    uint64_t value;

    pfc ( " ... online!\n", 31 );
    pfc ( "\n Start time was: dd/mm/yyyy | hh:mm:ss\n", 31 );

    /* Send data in loop */
    while (true) {
      
      msg_packet.Clear(); // Clear the message (no need to reallocate)
      
      /* Do kstat look-ups */
      int i=0;
      
      // The chain should be updated occasionally

      if (kstat_chain_update ( kc )) {
        pfc (" here\n", 36);
      }
      
      /* General statistics */

      /* Memory */
      PB_MSG::Packet_Mem *msg_mem = msg_packet.add_mem();
      for (i=0; i<S::MEM::number; i++) {
        if (retreive_kstat ( S::MEM::module[i], S::MEM::name[i], S::MEM::statistic[i], -1, &value )) {
          printf("Failure in function \"retreive_kstat\" @memory \n");
        } else {
          if (S::MEM::statistic[i]=="physmem") msg_mem->set_physmem ( value );
          else if (S::MEM::statistic[i]=="rss") msg_mem->set_rss ( value );
          else if (S::MEM::statistic[i]=="pp_kernel") msg_mem->set_pp_kernel ( value );
          else if (S::MEM::statistic[i]=="freemem") msg_mem->set_freemem ( value );
          else if (S::MEM::statistic[i]=="physcap") msg_mem->set_physcap ( value );
          else if (S::MEM::statistic[i]=="swap") msg_mem->set_swap ( value );
          else if (S::MEM::statistic[i]=="swapcap") msg_mem->set_swapcap ( value );

          if (VERBOSE) printf ( "Received: %u for %s\n", value, S::MEM::statistic[i].c_str() );
        } 
      }

      /* Network */
      int instance;
      /* Allow for instance loop for future dev */
      for (instance=0; instance<1; instance++) {
        PB_MSG::Packet_Net *msg_net = msg_packet.add_net();
        msg_net->set_instance ( "net0" );
        for (i=0; i<S::NET::number; i++) {
          if (retreive_kstat ( S::NET::module[i], S::NET::name[i], S::NET::statistic[i], -1, &value )) {
            pfc ( "WARN: Failure in function \"retreive_kstat\" @network \n", 33 );
          } else {
            /* This is an ugly way to do this...                                  */
            /* You should put everything here in a function array (typedef above) */
            if (S::NET::statistic[i]=="obytes64") msg_net->set_obytes64 ( value );
            else if (S::NET::statistic[i]=="rbytes64") msg_net->set_rbytes64 ( value );
            else if (S::NET::statistic[i]=="opackets") msg_net->set_opackets ( value );
            else if (S::NET::statistic[i]=="ipackets") msg_net->set_ipackets ( value );
            else pfc ( "WARN: Unexpected statistic returned @net\n", 33 ); 

            if (VERBOSE) printf ( "Received: %u for %s\n", value, S::NET::statistic[i].c_str() );
          }
        }
      }

      /* Disk */   // Fails due to class errors
/*    instance = -1;
      while (++instance < 12) {
        ++instance;
        PB_MSG::Packet_Disk *msg_disk = msg_packet.add_disk();
        msg_disk->set_instance ( instance );
        for (i=0; i<S::DISK::number; i++) {
          std::ostringstream name;
          name << S::DISK::module[i] << instance;
          //if (retreive_kstat ( S::DISK::module[i], S::DISK::name.str(), S::DISK::statistic[i], instance, &value )) {
          if (retreive_kstat ( (std::string)"sd", (std::string)"sd11", (std::string)"nread", 11, &value )) {
            pfc ( "WARN: Failure in function \"retreive_kstat\" @disk \n", 33 );
          } else {
          // It's ugly, I get it
            
            if (VERBOSE) printf ( "Received: %u for %s\n", value, S::DISK::statistic[i].c_str() );
          }
        }
      }
*/
 
      /* Do dtrace look-ups */
      for (i=0; i<S::DTRACE::number; i++) {

        if (i==2) { printf ("ici\n"); }
  
        (void) dtrace_status(g_dtp[i]);
        if (dtrace_aggregate_snap(g_dtp[i]) != 0) {
          pfc ( "WARN: Failed to snap aggregate\n", 33 );
        }
          
        if (dtrace_aggregate_walk_valsorted ( g_dtp[i], dtrace_aggwalk, NULL ) != 0) {
          pfc ( "WARN: Failed to walk aggregate\n", 33 );
        }

        if (VERBOSE) printf ( "===========================================\n" );

      }

      msg_packet.set_time ( (uint64_t)time(NULL) );      

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

/*
 * Print "something" from a libdtrace aggregate
 * in the format it was meant to be.
 */
 int formatted_print ( const dtrace_recdesc_t *rec, caddr_t addr ) {
 switch (rec->dtrd_size) {
    case sizeof(uint8_t):
      printf ( "i8:%d", *((uint8_t *)addr));
      return 0;
    case sizeof(uint16_t):
      printf ( "16:%d", *((uint16_t *)addr) );
      return 0;
    case sizeof(uint32_t):
      printf ( "32:%d", *((uint32_t *)addr) );
      return 0;
    case sizeof(uint64_t):
      printf ( "64:%u", *((uint64_t *)addr) );
      return 0;
    default:
      printf ( "%s", (const char *)addr );
      return 0;
    return -1;
  }
}

/* Walk through aggregate and add data to msg_packet */
static int dtrace_aggwalk ( const dtrace_aggdata_t *agg, void *arg )
{
  struct {
    int type;
    uint32_t core;
    uint64_t usage;
    uint32_t pid;
    std::string name;
  } data; 
  
  bool reset = 1;
  dtrace_aggdesc_t *aggdesc = agg->dtada_desc;

  int i;
  for (i = 1; i < aggdesc->dtagd_nrecs; i++) {
    const dtrace_recdesc_t *rec = &aggdesc->dtagd_rec[i];
    caddr_t addr = agg->dtada_data + rec->dtrd_offset;
 
    if (i==1) {
      data.type = *((uint32_t *)addr);
    }
 
    /* Again, this is a very ugly way to do this */
    if (data.type==0) {
      if (i==2) data.core = *((uint32_t *)addr);
      else if (i==3) data.usage = (uint64_t)*((uint32_t *)addr); 
    } else if (data.type==1) {
      if (i==2) data.core = *((uint32_t *)addr);
      else if (i==3) data.name = (std::string)(const char *)addr;
      else if (i==4) data.pid = *((uint32_t *)addr);
      else if (i==5) data.usage = *((uint64_t *)addr);
    } else if (data.type==2) {
      if (i==2) msg_packet.set_ticks ( (int32_t)*((uint64_t *)addr) );
    } else {
      pfc ( "WARN: Unrecognized dtrace script type\n", 33 );
      formatted_print ( rec, addr );
      printf ( "\n" );
    }
  }

  /* Actually add to the protobuf now */
  if (data.type==0) {
    PB_MSG::Packet_Cpu *msg_cpu = msg_packet.add_cpu();
    msg_cpu->set_core ( data.core );
    msg_cpu->set_usage ( (uint32_t)data.usage );
  } else if (data.type==1) {
    PB_MSG::Packet_Process *msg_process = msg_packet.add_process();
    msg_process->set_pid ( data.pid );
    msg_process->set_execname ( data.name );
    msg_process->set_usage ( (uint32_t)data.usage ); 
    msg_process->set_cpu ( data.core );
  }

  if (reset) return (DTRACE_AGGWALK_REMOVE);
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
 * Find the associated kstat using libkstat
 *   methods. Note the necessity to cast 
 *   strings to raw char *'s.
 */
int retreive_kstat ( std::string module, std::string name, std::string statistic, int instance, uint64_t *value ) {

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
    ksp = kstat_lookup ( kc, (char *)module.c_str(), instance, (char *)name.c_str() );
  } else {
    ksp = kstat_lookup ( kc, (char *)module.c_str(), instance, NULL );
  }
 
  kstat_read ( kc, ksp, NULL );
  knp = (kstat_named_t *)kstat_data_lookup ( ksp, (char *)statistic.c_str() ); 

  if (knp == NULL) {
    pfc ( " > libkstat lookup failed\n", 31 );
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
      if (VERBOSE) pfc ( "Working with uint32\n", 33 );
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

/*
 * Handle signals so that we don't have to
 * worry about errant dtrace scripts
 */
void sigint_handler ( int s ) {
  pfc ( "\nWARN: dtrace script instances were not cleaned!\nKilling program...\n", 33 );
  exit(1);
}


