/*
 *  server.cpp
 *
 *    Pushes kstat & dtrace statistics
 *    via ZMQ & protocol buffers to
 *    anyone who will listen.
 *
 *    CREATED:  16 JULY 2013
 *    UPDATED:  23 JULY 2013
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

/*
 * Note the existence of:
 */
void sigint_handler (int s);
int pfc (const char *c, int color);
int print_message (PB_MSG::Packet pckt);
void usage ();
void printfc (const char *c, int color);

int send_message ();
int send_message (const char *c);
int send_message (PB_MSG::Packet pckt);

int retreive_kstat (std::string module, std::string name, std::string statistic, int instance, uint64_t *value);
int formatted_print (const dtrace_recdesc_t *rec, caddr_t addr); 
int dtrace_setopts (dtrace_hdl_t **this_g_dtp);
int dtrace_init (dtrace_hdl_t **this_g_dtp, std::string s);
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
int main (int argc, char **argv) {

    /* Handle signals (for cleanup) */
    signal(SIGINT, sigint_handler);

    /* Splash */
    pfc ("\n Statistics Server\n", 37);
    pfc ("\n  Reports dtrace and kstat statistics\n", 37);
    pfc ("  using Google Protocol Buffers and\n", 37);
    pfc ("  ZMQ (0MQ) sockets.\n\n", 37);

    /* Parse command line */
    int i=0;
    for (i=0; i<argc; i++) {
      if (VERBOSE) printf ( "Argument %s\n", argv[i]);
      if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "-V")) {
        pfc (" . running in verbose mode\n", 36);
        VERBOSE = 1;
      }
    }
 
    /* Set up zmq socket */
    socket.bind ("tcp://*:7211");
    send_message ((const char *)"Init"); 

    /* Protobuf stuff */
    GOOGLE_PROTOBUF_VERIFY_VERSION;
    
    /* Dtrace init */
    dtrace_hdl_t *g_dtp[S::DTRACE::number]; 
    for (i=0; i<S::DTRACE::number; i++) {
      dtrace_init (&g_dtp[i], S::dtrace[i]);
      dtrace_setopts (&g_dtp[i]);
      if (dtrace_go (g_dtp[i])) {
        pfc ("ERROR: Unable to start dtrace script\n", 31);
      }
      (void) dtrace_status (g_dtp[i]);
    }

    /* Kstat init */
    kc = kstat_open();
    uint64_t value;
    uint64_t st = time (NULL);  // start time

    pfc ("\n Server online!", 31);
    printf ("\n \033[00;31mStart time was: %2d:%2d:%2d UTC\033[00m\n\n", (st/3600)%24, (st/60)%60, st%60);

    /* Send data in loop */
    while (true) {
      
      msg_packet.Clear(); // Clear the message (no need to reallocate)
      
      /* Do kstat look-ups */
      int i=0;
      
      // The chain should be updated occasionally

      if (kstat_chain_update ( kc )) {
        if (VERBOSE) pfc (" . kstat chain updated\n", 36);
      }
      
      /* General statistics */

      /* Memory */
      PB_MSG::Packet_Mem *msg_mem = msg_packet.add_mem();
      for (i=0; i<S::MEM::number; i++) {
        if (retreive_kstat (S::MEM::module[i], S::MEM::name[i], S::MEM::statistic[i], -1, &value)) {
          pfc ("Failure in function \"retreive_kstat\" @memory \n", 31);
        } else {
          if (S::MEM::statistic[i]=="physmem") {
            msg_mem->set_physmem_1 ((uint32_t)(value>>32));
            msg_mem->set_physmem_2 ((uint32_t)((value<<32)>>32));
          } else if (S::MEM::statistic[i]=="rss") {
            msg_mem->set_rss_1 ((uint32_t)(value>>32));
            msg_mem->set_rss_2 ((uint32_t)((value<<32)>>32));
          } else if (S::MEM::statistic[i]=="pp_kernel") {
            msg_mem->set_pp_kernel_1 ((uint32_t)(value>>32));
            msg_mem->set_pp_kernel_2 ((uint32_t)((value<<32)>>32));
          } else if (S::MEM::statistic[i]=="freemem") {
            msg_mem->set_freemem_1 ((uint32_t)(value>>32));
            msg_mem->set_freemem_2 ((uint32_t)((value<<32)>>32));
          } else if (S::MEM::statistic[i]=="physcap") {
            msg_mem->set_physcap_1 ((uint32_t)(value>>32));
            msg_mem->set_physcap_2 ((uint32_t)((value<<32)>>32));
          } else if (S::MEM::statistic[i]=="swap") {
            msg_mem->set_swap_1 ((uint32_t)(value>>32));
            msg_mem->set_swap_2 ((uint32_t)((value<<32)>>32));
          } else if (S::MEM::statistic[i]=="swapcap") {
            msg_mem->set_swapcap_1 ((uint32_t)(value>>32));
            msg_mem->set_swapcap_2 ((uint32_t)((value<<32)>>32));
          }

          if (VERBOSE) printf ("Received: %u for %s\n", value, S::MEM::statistic[i].c_str());
        } 
      }

      /* Network */
      int instance;
      /* Allow for instance loop for future dev */
      for (instance=0; instance<1; instance++) {
        PB_MSG::Packet_Net *msg_net = msg_packet.add_net();
        msg_net->set_instance ("net0");
        for (i=0; i<S::NET::number; i++) {
          if (retreive_kstat (S::NET::module[i], S::NET::name[i], S::NET::statistic[i], -1, &value)) {
            pfc ("WARN: Failure in function \"retreive_kstat\" @network \n", 33);
          } else {
            /* This is an ugly way to do this...  */
            if (S::NET::statistic[i]=="obytes64") {
              msg_net->set_obytes64_1 ((uint32_t)(value>>32));
              msg_net->set_obytes64_2 ((uint32_t)((value<<32)>>32));
            } else if (S::NET::statistic[i]=="rbytes64") {
              msg_net->set_rbytes64_1 ((uint32_t)(value>>32));
              msg_net->set_rbytes64_2 ((uint32_t)((value<<32)>>32));
            } else if (S::NET::statistic[i]=="opackets") {
              msg_net->set_opackets_1 ((uint32_t)(value>>32));
              msg_net->set_opackets_2 ((uint32_t)((value<<32)>>32));
            } else if (S::NET::statistic[i]=="ipackets") {
              msg_net->set_ipackets_1 ((uint32_t)(value>>32));
              msg_net->set_ipackets_2 ((uint32_t)((value<<32)>>32));
            } else pfc ("WARN: Unexpected statistic returned @net\n", 33); 

            if (VERBOSE) printf ("Received: %u for %s\n", value, S::NET::statistic[i].c_str());
          }
        }
      }

      /* Disk */
      instance = -1;
      while (++instance>-1) {
        std::ostringstream name;
        name << (std::string)"sd" << instance;
        int return_value = retreive_kstat ((std::string)"sd", name.str(), "NULL", instance, &value);
        if (return_value == -1) {
          pfc ("WARN: Failure in function \"retreive_kstat\" @disk \n", 33);
        } else if (return_value == 1) {
          break;
          // We ran out of disk instances
          // Continue as usual
        } else {
          // All should have been taken care of within retreive_kstat
          // Nothing to see here. Move along.
        }
      }
 
      /* Do dtrace look-ups */
      for (i=0; i<S::DTRACE::number; i++) {
        (void) dtrace_status (g_dtp[i]);
        if (dtrace_aggregate_snap (g_dtp[i]) != 0) {
          pfc ("WARN: Failed to snap aggregate\n", 33);
        }
        if (dtrace_aggregate_walk_valsorted (g_dtp[i], dtrace_aggwalk, NULL) != 0) {
          pfc ("WARN: Failed to walk aggregate\n", 33);
        }
        if (VERBOSE) printf ("===========================================\n");
      }

      msg_packet.set_time ((uint64_t)time(NULL));      

      if (VERBOSE) {
        print_message (msg_packet);
      }
      send_message (msg_packet);
      
      sleep (1);
    }

    return 0;
}

/* Initialize dtrace program (passed by handle and string) */
int dtrace_init (dtrace_hdl_t **this_dtp, std::string this_prog) {

  int done = 0;
  int err;
  dtrace_prog_t *prog;
  dtrace_proginfo_t info;

  if ((*this_dtp = dtrace_open (DTRACE_VERSION, 0, &err)) == NULL) {
    pfc ("ERROR: Failed to init dtrace\n", 31);
    return -1;  
  }

  if ((prog = dtrace_program_strcompile (*this_dtp, (const char *)this_prog.c_str(),
    DTRACE_PROBESPEC_NAME, 0, 0, NULL)) == NULL) {
    pfc ("ERROR: Failed to compile program\n", 31);
    return -1;  
  }

  if (dtrace_program_exec (*this_dtp, prog, &info) == -1) {
    pfc ("ERROR: Failed to init probes\n", 31);
    return -1;
  }

  return 0;
}

/*
 * Print "something" from a libdtrace aggregate
 * in the format it was meant to be.
 */
 int formatted_print (const dtrace_recdesc_t *rec, caddr_t addr) {
 switch (rec->dtrd_size) {
    case sizeof(uint8_t):
      printf ("i8:%d", *((uint8_t *)addr));
      return 0;
    case sizeof(uint16_t):
      printf ("16:%d", *((uint16_t *)addr));
      return 0;
    case sizeof(uint32_t):
      printf ("32:%d", *((uint32_t *)addr));
      return 0;
    case sizeof(uint64_t):
      printf ("64:%u", *((uint64_t *)addr));
      return 0;
    default:
      printf ("%s", (const char *)addr);
      return 0;
    return -1;
  }
}

/*
 * Given a bucket-id, return the max value.
 * The min value is simply (max/2)+1 
 */
inline int64_t max_quantize_range (int bckt_id) {
  if (bckt_id < DTRACE_QUANTIZE_ZEROBUCKET) {
    return -1; 
  } else if (bckt_id == DTRACE_QUANTIZE_ZEROBUCKET) {
    return 0;
  } else {
    return DTRACE_QUANTIZE_BUCKETVAL (bckt_id);
  }
}

/* Walk through aggregate and add data to msg_packet */
static int dtrace_aggwalk (const dtrace_aggdata_t *agg, void *arg)
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
      if (i==2) msg_packet.set_ticks ((uint32_t)*((uint64_t *)addr));
    } else if (data.type==3) {
      if (rec->dtrd_action == DTRACEAGG_QUANTIZE) {
        const int64_t *agg_data = (int64_t *)(agg->dtada_data +
          rec->dtrd_offset);
        int range_cnt;
        for (range_cnt=0; range_cnt<DTRACE_QUANTIZE_NBUCKETS; range_cnt++) {
          if (!agg_data[range_cnt]) continue;
          if (VERBOSE) printf ("%12d : %8d \n", max_quantize_range (range_cnt), agg_data[range_cnt]);

          int64_t l_range = max_quantize_range (range_cnt);
          PB_MSG::Packet_CallHeat *msg_callheat = msg_packet.add_callheat();
          msg_callheat->set_name ((std::string)"allcall");
          msg_callheat->set_lowt_1 ((uint32_t)(l_range>>32)); 
          msg_callheat->set_lowt_2 ((uint32_t)((l_range<<32)>>32));
          msg_callheat->set_value_1 ((uint32_t)(agg_data[range_cnt]>>32));
          msg_callheat->set_value_2 ((uint32_t)((agg_data[range_cnt]<<32)>>32));
        }
      }
    } else {
      pfc ("WARN: Unrecognized dtrace script type @334\n", 33);
      if (VERBOSE) formatted_print (rec, addr);
      printf ("\n");
    } 
  }

  /* Actually add to the protobuf now */
  if (data.type==0) {
    PB_MSG::Packet_Cpu *msg_cpu = msg_packet.add_cpu();
    msg_cpu->set_core (data.core);
    msg_cpu->set_usage ((uint32_t)data.usage);
  } else if (data.type==1) {
    PB_MSG::Packet_Process *msg_process = msg_packet.add_process();
    msg_process->set_pid (data.pid);
    msg_process->set_execname (data.name);
    msg_process->set_usage ((uint32_t)data.usage); 
    msg_process->set_cpu (data.core);
  } 

  if (reset) return (DTRACE_AGGWALK_REMOVE);
  return (DTRACE_AGGWALK_NEXT);
}

/* For the dtrace command line */
int dtrace_setopts (dtrace_hdl_t **this_g_dtp) {

  if (dtrace_setopt(*this_g_dtp, "strsize", "32") == -1) {
    pfc ("failed to set 'strsize'", 31);
  }
  if (dtrace_setopt(*this_g_dtp, "bufsize", "1k") == -1) {
    pfc ("failed to set 'bufsize'", 31);
  }
  if (dtrace_setopt(*this_g_dtp, "aggsortrev", NULL) == -1) {
    pfc ("failed to set 'aggsortrev'", 31);
  }
  if (dtrace_setopt(*this_g_dtp, "aggsize", "1M") == -1) {
    pfc ("failed to set 'aggsize'", 31);
  }
  if (dtrace_setopt(*this_g_dtp, "aggrate", "2msec") == -1) {
    pfc ("failed to set 'aggrate'", 31);
  }
 
  return 0;
}

/* Print contents of packet message for user */
int print_message(PB_MSG::Packet pckt) {
  printf ("String representation of packet:\n");
  pckt.PrintDebugString(); 
  return 0;
}  

/*
 * Find the associated kstat using libkstat
 *   methods. Note the necessity to cast 
 *   strings to raw char *'s.
 */
int retreive_kstat (std::string module, std::string name, std::string statistic, int instance, uint64_t *value) {

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
    ksp = kstat_lookup (kc, (char *)module.c_str(), -1, (char *)name.c_str());
  } else {
    ksp = kstat_lookup (kc, (char *)module.c_str(), instance, NULL);
  }

  if ((ksp==NULL) && (statistic=="NULL")) {
    return 1;
  } 

  if (ksp == NULL) {
    pfc ( "ksp @439 returned NULL\n", 31 );
  }

  /* If we're reading a KSTAT_TYPE_IO file, we have
   * to handle things a bit differently 
   */
  if (ksp->ks_type == KSTAT_TYPE_IO) {
    
    kstat_io_t kio;   // Data structure to hold IO stat
    kstat_read (kc, ksp, &kio); 
    
    if (&kio == NULL) {
      printf ("NULL for instance %d\n", instance);
    }
                   
    /* I'm keeping this non-modular here and
     * pushing directly to the protocol buffer
     * here because it will save significant
     * space and time/processing power
     */
    
    PB_MSG::Packet_Disk *msg_disk = msg_packet.add_disk();
    msg_disk->set_instance ((uint32_t)instance);
    
    msg_disk->set_nread_1 ((uint32_t)(kio.nread>>32));
    msg_disk->set_nread_2 ((uint32_t)((kio.nread<<32)>>32));
    msg_disk->set_nwritten_1 ((uint32_t)(kio.nwritten>>32));
    msg_disk->set_nwritten_2 ((uint32_t)((kio.nwritten<<32)>>32));
    msg_disk->set_reads ((uint32_t)kio.reads);
    msg_disk->set_writes ((uint32_t)kio.writes);
    msg_disk->set_wtime_1 ((uint32_t)(kio.wtime>>32));
    msg_disk->set_wtime_2 ((uint32_t)((kio.wtime<<32)>>32));
    msg_disk->set_wlentime_1 ((uint32_t)(kio.wlentime>>32));
    msg_disk->set_wlentime_2 ((uint32_t)((kio.wlentime<<32)>>32));
    msg_disk->set_rtime_1 ((uint32_t)(kio.rtime>>32));
    msg_disk->set_rtime_2 ((uint32_t)((kio.rtime<<32)>>32));
    msg_disk->set_rlentime_1 ((uint32_t)(kio.rlentime>>32));
    msg_disk->set_rlentime_2 ((uint32_t)((kio.rlentime<<32)>>32));

  } else {

    kstat_read (kc, ksp, NULL);
    knp = (kstat_named_t *)kstat_data_lookup (ksp, (char *)statistic.c_str()); 

    if (knp == NULL) {
      pfc (" > libkstat lookup failed\n", 31);
      return -1;
    }

    /* Cast data depending on type */
    switch (knp->data_type) {
      case KSTAT_DATA_CHAR:
        printf ("char value: %c\n", knp->value.c);
        pfc ("Not yet implemented : 1\n", 33);
        break;
      case KSTAT_DATA_INT32:
        *value = knp->value.i32;
        pfc ("Not yet tested : 2\n", 33);
        break;
      case KSTAT_DATA_UINT32:
        *value = (uint64_t)knp->value.ui32;
        if (VERBOSE) pfc ("Working with uint32\n", 33);
        break;
      case KSTAT_DATA_INT64:
        *value = (uint64_t)knp->value.i64;
        break;
      case KSTAT_DATA_UINT64:
        *value = knp->value.ui64;
        break;
      default:
        // We should never end up in here
        printf ("Something rather peculiar happened.\n");
        break;
    }
  }

  return 0;
}

/*
 * This function sends a message 
 * over the established zmq socket
 */
int send_message () {
  send_message ((const char *)"Hello");
  return 0;
}

int send_message (const char *c) {
  try {
    zmq::message_t msg (strlen(c));
    memcpy ((void *)msg.data(), c, strlen(c));
    socket.send (msg);
    return 0;
  } catch (int e) {
    printf ("Error number %d\n", e);
    return -1;
  }
}

int send_message (PB_MSG::Packet pckt) {
  try {
    std::string pckt_serialized;
    pckt.SerializeToString (&pckt_serialized);
    zmq::message_t msg (pckt_serialized.size());
    memcpy ((void *)msg.data(), pckt_serialized.c_str(), pckt_serialized.size());
    socket.send (msg);
  } catch (int e) {
    printf ("Error number %d\n", e);
    return -1;
  }
  return 0;
}

/*
 * Prints a colorized version of input text
 */
int pfc (const char *c, int color) {

  printf ("\033[00;%dm%s\033[00m", color, c);
  return 0;
}

/*
 * Handle signals so that we don't have to
 * worry about errant dtrace scripts
 */
void sigint_handler (int s) {
  pfc ("\nWARN: dtrace script instances were not cleaned!\nKilling program...\n", 33);
  exit(1);
}
