/*
 *  server.cpp
 *
 *    Pushes kstat & dtrace statistics
 *    via ZMQ & protocol buffers to
 *    anyone who will listen. Also saves
 *    recorded data into fastbit database
 *    using information from "db.conf"
 *    file.
 *
 *    COMPILE WITH:
 *      g++ server.cpp packet.pb.cc -ldtrace
 *        -lzmq -lfastbit -lkstat -lprotobuf
 *        -O2 -o server_release
 *
 *    CREATED:  16 JUL 2013
 *    UPDATED:   2 AUG 2013
 *
 */

// 32bit support
#define MSB(n) (uint32_t)(n>>32) 
#define LSB(n) (uint32_t)((n<<32)>>32) 

#include <string>
#include <iostream>
#include <unistd.h>
#include <cstring>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <sstream>

#include <dtrace.h>      // for libdtrace
#include <zmq.hpp>       // for zmq sockets
#include <kstat.h>       // for kstats
#ifdef FULLBIT
#include "packet64.pb.h" // 64bit protocol buffer header
#else
#include "packet.pb.h"   // protocol buffer header
#endif
#include "scripts.hpp"   // script repository

/*
 * Note the existence of:
 */
void sig_handler (int s);
int pfc (const char *c, int color);
int print_message (PB_MSG::Packet pckt);
void usage ();

int send_message ();
int send_message (const char *c);
int send_message (PB_MSG::Packet pckt);

int retreive_kstat (std::string module, std::string name, std::string statistic, int instance, uint64_t *value);
int formatted_print (const dtrace_recdesc_t *rec, caddr_t addr); 
int dtrace_setopts (dtrace_hdl_t **this_g_dtp);
int dtrace_init (dtrace_hdl_t **this_g_dtp, std::string s);
static int dtrace_aggwalk (const dtrace_aggdata_t *data, void *arg);

/*
 * Globally-existing variables &c.
 */
bool do_loop = 1;
bool VERBOSE = 0; 
bool VERBOSE2 = 0;
bool QUIET = 0;
int millisec = 999;
const char *port = "7211";
zmq::context_t context (1); 
zmq::socket_t socket (context, ZMQ_PUB);
PB_MSG::Packet msg_packet;
kstat_ctl_t *kc;

/*
 *  main loop
 */
int main (int argc, char **argv) {

    /* Handle signals (for cleanup) */
    signal (SIGINT, sig_handler);
    signal (SIGTERM, sig_handler);
    signal (SIGQUIT, sig_handler);

    /* Splash */
    pfc ("\n Statistics Server\n", 37);
    pfc ("\n  Reports dtrace and kstat statistics\n", 37);
    pfc ("  using Google Protocol Buffers and\n", 37);
    pfc ("  ZMQ (0MQ) sockets.\n\n", 37);

    /* Parse command line */
    size_t i=0;
    for (i=0; i<argc; i++) {
      if (VERBOSE) printf ( "Argument %s\n", argv[i]);
      if (!strcmp (argv[i], "-v") || !strcmp (argv[i], "-V")) {
        pfc (" . running in verbose mode\n", 36);
        VERBOSE = 1;
      }
      if (!strcmp (argv[i], "-vlite")) {
        pfc (" . running in light verbose mode\n", 36);
        VERBOSE2 = 1;
      }
      if (!strcmp (argv[i], "-h") || !strcmp (argv[i], "-H")) {
        pfc (" . printing usage information\n", 36);
        usage();
        return 1;
      }
      if (!strcmp (argv[i], "-p") || !strcmp (argv[i], "-P")) {
        port = argv[i+1];
        std::cout << "\033[00;36m . using port " << port << "\033[00m\n"; 
      }
      if (!strcmp(argv[i], "-q") || !strcmp (argv[i], "-Q")) {
        QUIET = 1;
        std::cout << "\033[00;36m . running in quiet mode\033[00m\n";
      }
      if (!strcmp(argv[i], "-d") || !strcmp (argv[i], "-D")) {
        millisec = atoi(argv[i+1]);
        if (millisec > 999) millisec = 999;
        std::cout << "\033[00;36m . using delay of " << argv[i+1] << "\033[00m\n";
      }
    }
 
    /* Set up zmq socket */
    char buf[15];
    sprintf(buf, "tcp://*:%s", port);
    if (!QUIET) {
      socket.bind (buf);
    }

    /* Protobuf stuff */
    GOOGLE_PROTOBUF_VERIFY_VERSION;
    
    /* Dtrace init */
    dtrace_hdl_t *g_dtp[DTRACE::number]; 
    for (i=0; i<DTRACE::number; i++) {
      dtrace_init (&g_dtp[i], DTRACE::dtrace[i]);
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
    while (do_loop) {
      
      msg_packet.Clear(); // Clear the message (no need to reallocate)
      
      /* Do kstat look-ups */
      int i=0;
      
      // The kstat chain should be updated occasionally
      if (kstat_chain_update ( kc )) {
        if (VERBOSE) pfc (" . kstat chain updated\n", 36);
      }
      
      /* General statistics */
      msg_packet.set_processes (0);
      msg_packet.set_threads (0);

      /* Memory */
      PB_MSG::Packet_Mem *msg_mem = msg_packet.add_mem();
      for (i=0; i<MEM::number; i++) {
        if (retreive_kstat (MEM::module[i], MEM::name[i], MEM::statistic[i], -1, &value)) {
          pfc ("Failure in function \"retreive_kstat\" @memory \n", 31);
        } else {
          #ifdef FULLBIT
             if (MEM::statistic[i]=="physmem") {
             msg_mem->set_physmem (value);
           } else if (MEM::statistic[i]=="rss") {
             msg_mem->set_rss (value);
           } else if (MEM::statistic[i]=="pp_kernel") {
             msg_mem->set_pp_kernel (value);
            } else if (MEM::statistic[i]=="freemem") {
              msg_mem->set_freemem (value);
            } else if (MEM::statistic[i]=="physcap") {
              msg_mem->set_physcap (value);
            } else if (MEM::statistic[i]=="swap") {
              msg_mem->set_swap (value);
            } else if (MEM::statistic[i]=="swapcap") {
              msg_mem->set_swapcap (value);
            }
          #else
             if (MEM::statistic[i]=="physmem") {
              msg_mem->set_physmem_1 (MSB(value));
              msg_mem->set_physmem_2 (LSB(value));
            } else if (MEM::statistic[i]=="rss") {
              msg_mem->set_rss_1 (MSB(value));
              msg_mem->set_rss_2 (LSB(value));
            } else if (MEM::statistic[i]=="pp_kernel") {
              msg_mem->set_pp_kernel_1 (MSB(value));
              msg_mem->set_pp_kernel_2 (LSB(value));
            } else if (MEM::statistic[i]=="freemem") {
              msg_mem->set_freemem_1 (MSB(value));
              msg_mem->set_freemem_2 (LSB(value));
            } else if (MEM::statistic[i]=="physcap") {
              msg_mem->set_physcap_1 (MSB(value));
              msg_mem->set_physcap_2 (LSB(value));
            } else if (MEM::statistic[i]=="swap") {
              msg_mem->set_swap_1 (MSB(value));
              msg_mem->set_swap_2 (LSB(value));
            } else if (MEM::statistic[i]=="swapcap") {
              msg_mem->set_swapcap_1 (MSB(value));
              msg_mem->set_swapcap_2 (LSB(value));
            }
         #endif

          if (VERBOSE) std::cout << "Received: " << value << " for " 
                  << MEM::statistic[i].c_str() << std::endl;
        } 
      }

      /* Network */
      int instance;
      /* Allow for instance loop for future dev */
      for (instance=0; instance<NET::num_instance; instance++) {
        PB_MSG::Packet_Net *msg_net = msg_packet.add_net();
        msg_net->set_instance (NET::name[instance][0].c_str());
        for (i=0; i<NET::number; i++) {
          if (retreive_kstat (NET::module[instance][i], NET::name[instance][i],
                       NET::statistic[instance][i], -1, &value)) {
            pfc ("WARN: Failure in function \"retreive_kstat\" @network \n", 33);
          } else {
            /* This is an ugly way to do this...  */
            #ifdef FULLBIT
              if (NET::statistic[0][i]=="obytes64") {
                msg_net->set_obytes64 (value);
              } else if (NET::statistic[0][i]=="rbytes64") {
                msg_net->set_rbytes64 (value);
              } else if (NET::statistic[0][i]=="opackets") {
                msg_net->set_opackets (value);
              } else if (NET::statistic[0][i]=="ipackets") {
                msg_net->set_ipackets (value);
           #else
              if (NET::statistic[0][i]=="obytes64") {
                msg_net->set_obytes64_1 (MSB(value));
                msg_net->set_obytes64_2 (LSB(value));
              } else if (NET::statistic[0][i]=="rbytes64") {
                msg_net->set_rbytes64_1 (MSB(value));
                msg_net->set_rbytes64_2 (LSB(value));
              } else if (NET::statistic[0][i]=="opackets") {
                msg_net->set_opackets_1 (MSB(value));
                msg_net->set_opackets_2 (LSB(value));
              } else if (NET::statistic[0][i]=="ipackets") {
                msg_net->set_ipackets_1 (MSB(value));
                msg_net->set_ipackets_2 (LSB(value));
            #endif 
            } else if (NET::statistic[0][i]=="ierrors") {
              msg_net->set_ierrors ((uint32_t)value);
            } else if (NET::statistic[0][i]=="oerrors") {
              msg_net->set_oerrors ((uint32_t)value);
            } else {
              pfc ("WARN: Unexpected statistic returned @net\n", 33);
              std::cout << "Statistic: " << NET::statistic[0][i] << std::endl;
            }

            if (VERBOSE) printf ("Received: %u for %s\n", value, NET::statistic[instance][i].c_str());
          }
        }
      }

      /* Disk */
      instance = -1;
      while (++instance>-1) {
        std::ostringstream name;
        name << std::string("sd") << instance;
        int return_value = retreive_kstat (std::string("sd"), name.str(), "NULL", instance, &value);
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
      for (i=0; i<DTRACE::number; i++) {
        (void) dtrace_status (g_dtp[i]);
        if (dtrace_aggregate_snap (g_dtp[i]) != 0) {
          pfc ("WARN: Failed to snap aggregate\n", 33);
        }
        if (dtrace_aggregate_walk_valsorted (g_dtp[i], dtrace_aggwalk, NULL) != 0) {
          pfc ("WARN: Failed to walk aggregate\n", 33);
        }
        if (VERBOSE) std::cout << "===========================================" << std::endl;
      }

      msg_packet.set_time ((uint64_t)time(NULL));      

      if (VERBOSE) {
        print_message (msg_packet);
      }

      if (!QUIET) send_message (msg_packet);

      struct timespec req = {0};
      req.tv_sec = 0;
      req.tv_nsec = millisec * 1000000L;
      nanosleep(&req, (struct timespec *)NULL);
    }

    /* Kill DTrace scripts */
    for (size_t scpt=0; scpt<DTRACE::number; scpt++) {
      dtrace_close (g_dtp[scpt]); 
    }
    pfc (" . dtrace scripts killed\n", 36);
    
    /* Shutdown ProtoBuf library */
    google::protobuf::ShutdownProtobufLibrary();

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

  size_t i;
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
      else if (i==3) data.name = std::string((const char *)addr);
      else if (i==4) data.pid = *((uint32_t *)addr);
      else if (i==5) data.usage = *((uint64_t *)addr);
    } else if (data.type==2) {
      if (i==2) {
        msg_packet.set_ticks ((uint32_t)*((uint64_t *)addr));
        if (VERBOSE2) printf("UTC %2d:%2d:%2d | ticks (last cycle) %d\n", (time(NULL)/3600)%24,
            (time(NULL)/60)%60, time(NULL)%60, msg_packet.ticks()); 
      }
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
          msg_callheat->set_name (std::string("allcall"));
          #ifdef FULLBIT
            msg_callheat->set_lowt (l_range); 
            msg_callheat->set_value (agg_data[range_cnt]);
         #else
            msg_callheat->set_lowt_1 (MSB(l_range)); 
            msg_callheat->set_lowt_2 (LSB(l_range));
            msg_callheat->set_value_1 (MSB(agg_data[range_cnt]));
            msg_callheat->set_value_2 (LSB(agg_data[range_cnt]));
          #endif
        }
      }
    } else if (data.type==4) {
    /* Count processes */
      if (i==2) msg_packet.set_processes (msg_packet.processes()+1);
    } else if (data.type==5) {
    /* Count threads */
      if (i==2) msg_packet.set_threads (msg_packet.threads()+1); 
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
  std::cout << "String representation of packet:" << std::endl;
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

  if (VERBOSE) {
    std::cout << "KSTAT " << module << name << statistic << std::endl;
  }
 
  if (strcmp(name.c_str(), "NULL")) {
    ksp = kstat_lookup (kc, (char *)module.c_str(), -1, (char *)name.c_str());
  } else {
    ksp = kstat_lookup (kc, (char *)module.c_str(), instance, NULL);
  }

  if ((ksp==NULL) && (statistic=="NULL")) {
    return 1;
  } 

  if (ksp == NULL) {
    std::cout << "Fail on: " << module << " " << name << " " << statistic;
    pfc ( "\nksp @439 returned NULL\n", 31 );
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
 
    #ifdef FULLBIT 
      msg_disk->set_nread (kio.nread);
      msg_disk->set_nwritten (kio.nwritten);
      msg_disk->set_reads (kio.reads);
      msg_disk->set_writes (kio.writes);
      msg_disk->set_wtime (kio.wtime);
      msg_disk->set_wlentime (kio.wlentime);
      msg_disk->set_rtime (kio.rtime);
      msg_disk->set_rlentime (kio.rlentime);
    #else
      msg_disk->set_nread_1 (MSB(kio.nread));
      msg_disk->set_nread_2 (LSB(kio.nread));
      msg_disk->set_nwritten_1 (MSB(kio.nwritten));
      msg_disk->set_nwritten_2 (LSB(kio.nwritten));
      msg_disk->set_reads ((uint32_t)kio.reads);
      msg_disk->set_writes ((uint32_t)kio.writes);
      msg_disk->set_wtime_1 (MSB(kio.wtime));
      msg_disk->set_wtime_2 (LSB(kio.wtime));
      msg_disk->set_wlentime_1 (MSB(kio.wlentime));
      msg_disk->set_wlentime_2 (LSB(kio.wlentime));
      msg_disk->set_rtime_1 (MSB(kio.rtime));
      msg_disk->set_rtime_2 (LSB(kio.rtime));
      msg_disk->set_rlentime_1 (MSB(kio.rlentime));
      msg_disk->set_rlentime_2 (LSB(kio.rlentime));
    #endif

    ksp = kstat_lookup (kc, (char *)"sderr", instance, NULL);
    kstat_read (kc, ksp, NULL);

    knp = (kstat_named_t *)kstat_data_lookup (ksp, (char *)"Hard Errors");
    msg_disk->set_harderror ((uint32_t)knp->value.ui32);
   
    knp = (kstat_named_t *)kstat_data_lookup (ksp, (char *)"Soft Errors");
    msg_disk->set_softerror ((uint32_t)knp->value.ui32); 

    knp = (kstat_named_t *)kstat_data_lookup (ksp, (char *)"Transport Errors");
    msg_disk->set_tranerror ((uint32_t)knp->value.ui32);

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
        std::cout << "Something rather peculiar happened." << std::endl;
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
  send_message ((const char *)"EMPTY");
  return 0;
}

int send_message (const char *c) {
  try {
    zmq::message_t msg (strlen(c));
    memcpy ((void *)msg.data(), c, strlen(c));
    if (!QUIET) socket.send (msg);
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
    if (!QUIET) socket.send (msg, ZMQ_NOBLOCK);
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
  std::cout << "\033[00;" << color << "m" << c << "\033[00m";
  return 0;
}

/*
 * Handle signals so that we don't have to
 * worry about errant dtrace scripts
 */
void sig_handler (int s) {
  do_loop = 0;
  pfc ("\n\nSafely killing program...\nStopping dtrace script interface\n", 32);
}

/*
 * Prints usage information to user
 */
void usage () {
  std::cout << "\nusage:  server [-h] [-p NUMBER] [-v] [-vlite]"; 
  std::cout << "\n    -h         prints this help/usage page";
  std::cout << "\n    -p PORT    use port PORT";
  std::cout << "\n    -v         run in verbose mode (print all queries and ZMQ packets)";
  std::cout << "\n    -vlite     prints time (and dtrace ticks (tick-4999)) for each sent message";
  std::cout << "\n    -d DELAY   wait DELAY<1000 ms instead of default 1000";
  std::cout << "\n    -q         quiet mode, prints diagnostic information and does not send messages";
#ifdef FULLBIT
  std::cout << "\n\nThis version of the server uses 64bit protocol buffers and is unsupported";
#else
  std::cout << "\n\nThis version of the server uses 32bit protocol buffer values.";
#endif
  std::cout << "\n\n";
}

