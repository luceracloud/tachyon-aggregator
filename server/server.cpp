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
 *    UPDATED:   8 AUG 2013
 *
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

#include <zmq.hpp>

#include "util.hpp"

#include "packet.pb.h"
#include "scripts.hpp"

#include "zmq.hpp"
#include "kstat.hpp"
#include "zone.hpp"
#include "dtrace.hpp"


/*
 * Note the existence of:
 */
void sig_handler (int s);
int pfc (const char *c, int color);
void usage ();

int send_message ();
int send_message (const char *c);
int send_message (PBMSG::Packet pckt);

int retreive_kstat (std::string module, std::string name, std::string statistic, int instance, uint64_t *value);

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
kstat_ctl_t *kc;

/* These are global because of the way
 * that the libdtrace aggregate walker
 * works.
 */
std::map <std::string, Zone*> ZoneData;
std::map <size_t, std::string> ZoneIndices;

/*
 *  main loop
 */
int main (int argc, char **argv) {

    /* Handle signals (for cleanup) */
    signal (SIGINT, sig_handler);
    signal (SIGTERM, sig_handler);
    signal (SIGQUIT, sig_handler);

    /* Splash */
    UTIL::white();
    std::cout << "\n Statistics Server\n";
    std::cout << "\n  Reports dtrace and kstat statistics\n";
    std::cout << "  using Google Protocol Buffers and\n";
    std::cout << "  ZMQ (0MQ) sockets.\n\n";
    UTIL::clear();

    /* Parse command line */
    for (size_t i=0; i<argc; i++) {
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
    for (size_t i=0; i<DTRACE::number; i++) {
      DTRACE::init (&g_dtp[i], DTRACE::dtrace[i]);
      DTRACE::setopts (&g_dtp[i]);
      if (dtrace_go (g_dtp[i])) {
        UTIL::red();
        std::cout << "ERROR: Unable to start dtrace script" << std::endl;
        UTIL::clear();
      }
      (void) dtrace_status (g_dtp[i]);
    }

    /* Kstat init */
    kc = kstat_open();
    uint64_t st = time (NULL);  // start time

    /* Allow us to pass values in and out */
    uint64_t value;
		std::vector<uint64_t> values;
		std::vector<std::string> names;
    std::vector<std::string> zones;

    UTIL::red();
    std::cout << "\n Server online!";
    printf ("\n \033[00;31mStart time was: %2d:%2d:%2d UTC\033[00m\n\n", (st/3600)%24, (st/60)%60, st%60);
    UTIL::clear();

    /* Collect and send data in loop */
    while (do_loop) {
     
			/* Clean up from last cycle and updates */
      ZoneData.clear();
      ZoneIndices.clear();
 
      /* Custom format to add zones to ZoneData &
       * to populate the repeated zonename in the GZ
       * zone/packet
       */
      if (KSTAT::retreive_multiple_kstat (kc, std::string("zones"), 
                                          std::string("zonename"), &values, 
                                          &names, &zones)) {
        UTIL::red();
        std::cout << "Something went terribly wrong. We cannot populate the\nlist of zones. "
                      "Skipping current sample, attempting next time." << std::endl;
        UTIL::clear();
      } else {
        /* Init GZ first to hold other zones & set as type:GZ */
        size_t z_index = 0;
        ZoneData.insert (std::make_pair (std::string("global"), new Zone ("global", true)));
        ZoneIndices.insert (std::make_pair (z_index, "global"));
        for (size_t i=0; i<zones.size(); i++) {
          ZoneData["global"]->add_zone (&zones.at (i));
          if (names.at(i)!="global") {
            ZoneData.insert (std::make_pair (zones.at(i), new Zone (std::string( zones.at (i)), false)));
            ZoneIndices.insert (std::make_pair (z_index, zones.at (i)));
          }
          z_index++;
        }
      }

      /* The kstat chain should be updated occasionally */
      if (kstat_chain_update ( kc )) {
        if (VERBOSE) {
          UTIL::cyan();
          std::cout << " . kstat chain updated" << std::endl;
          UTIL::clear();
        }
      }
     
			/*
			 * Grab memory statistics, first
 			 * from GZ, then from elsewhere.
 			 */
			for (size_t i=0; i<MEM::GZ_size; i++) {
			  if (KSTAT::retreive_kstat (kc, MEM::GZ_modl[i], MEM::GZ_name[i], MEM::GZ_stat[i], -1, &value)) {
          std::cout << "Unable to grab memory statistic\n";
        } else {
          ZoneData["global"]->add_mem (&MEM::GZ_stat[i], value);
        }
      }
			for (size_t i=0; i<MEM::size; i++) {
				if (KSTAT::retreive_multiple_kstat (kc, MEM::modl[i], MEM::stat[i], 
                                            &values, &names, &zones)) {
					std::cout << "Unable to retreive expected kstats for " << MEM::modl[i] << " " <<
											 MEM::stat[i] << __LINE__ << std::endl;
				} else {
          for (size_t j=0; j<names.size(); j++) {
            ZoneData[zones.at(j)]->add_mem (&MEM::stat[i], values.at (j));
          }
				}
			}

			/*
 			 * Grab network statistics, we
 			 * only care about NGZ stats,
 			 * as there are no GZ-specific
 			 * ones
 			 */
      for (size_t i=0; i<NET::size; i++) {
        if (KSTAT::retreive_multiple_kstat (kc, NET::modl[i], NET::stat[i], 
                                            &values, &names, &zones)) {
          std::cout << "Unable to retreive expected kstat for " << NET::modl[i] << " " <<
                       NET::stat[i] << __LINE__ << std::endl;
        } else {
          for (size_t j=0; j<values.size(); j++) {
            ZoneData[zones.at (j)]->add_network (&names.at (j), &NET::stat[i], values.at (j));
          }
        }
      }

      /*
       * Grab disk statistics. This
       * one works a bit differently
       * because of the way that I/O
       * kstats are stored. Still
       * returns a vector like the others.
       */
      for (size_t instance=0; instance<13; instance++) {
        for (size_t i=0; i<DISK::GZ_size; i++) {
          if (KSTAT::retreive_multiple_kstat (kc, DISK::GZ_modl[i], DISK::GZ_stat[i],
                                          &values, &names, &zones, (std::string *)"disk")) {
            std::cout << "Unable to retreive expected GZ kstat for " << DISK::GZ_modl[i] << " " <<
                        " " << DISK::GZ_stat[i] << "server.cpp:" << __LINE__ << std::endl;
          } else {
            for (size_t j=0; j<values.size(); j++) {
              ZoneData[zones.at (j)]->add_disk (&names[i], &DISK::GZ_stat[i], values.at (j));
            }
          }
        }
      }
      for (size_t i=0; i<DISK::size; i++) {
        if (KSTAT::retreive_multiple_kstat (kc, DISK::modl[i], DISK::stat[i], 
                                            &values, &names, &zones)) {
          std::cout << "Unable to retreive expected kstat for " << DISK::modl[i] << " " <<
                       DISK::stat[i] << " server.cpp:" << __LINE__ << std::endl;
        } else {
          for (size_t j=0; j<values.size(); j++) {
            ZoneData[zones.at (j)]->add_disk (&DISK::modl[i], &DISK::stat[i], values.at (j));
          }
        }
      }

      /* Do dtrace look-ups */
      for (int i=0; i<DTRACE::number; i++) {
        (void) dtrace_status (g_dtp[i]);
        if (dtrace_aggregate_snap (g_dtp[i]) != 0) {
          UTIL::yellow();
          std::cout << "WARN: Failed to snap aggregate" << std::endl;
          UTIL::clear();      
        }
        if (dtrace_aggregate_walk_valsorted (g_dtp[i], DTRACE::aggwalk, NULL) != 0) {
          UTIL::yellow();
          std::cout << "WARN: Failed to walk aggregate" << std::endl;
          UTIL::clear();  
        }
        if (VERBOSE) std::cout << "===========================================" << std::endl;
      }

      /*
       *  Here we have to actually send
       *  the proto packets.
       */
      if (!VERBOSE) {
        for (size_t i=0; i<ZoneData.size(); i++) {
          ZoneData.at (ZoneIndices.at(i))->set_time( time(NULL) );
          UTIL::blue();
          std::cout << std::endl << "BEGIN Zone Packet:" << std::endl;
          UTIL::clear();
          ZoneData.at(ZoneIndices.at(i))->print_zone();
        } 
      }

      for (size_t i=0; i<ZoneData.size(); i++) {
        Zone *z = ZoneData.at (ZoneIndices.at (i));
        PBMSG::Packet *pckt = z->ReturnPacket();
        if (!QUIET) send_message (*pckt);
        if (VERBOSE) std::cout << "Packet sent" << std::endl;
      }
        
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
    
//    PB_MSG::Packet_Disk *msg_disk = msg_packet.add_disk();

    ksp = kstat_lookup (kc, (char *)"sderr", instance, NULL);
    kstat_read (kc, ksp, NULL);

    knp = (kstat_named_t *)kstat_data_lookup (ksp, (char *)"Hard Errors");
 //   msg_disk->set_harderror ((uint32_t)knp->value.ui32);
   
    knp = (kstat_named_t *)kstat_data_lookup (ksp, (char *)"Soft Errors");
 //   msg_disk->set_softerror ((uint32_t)knp->value.ui32); 

    knp = (kstat_named_t *)kstat_data_lookup (ksp, (char *)"Transport Errors");
 //   msg_disk->set_tranerror ((uint32_t)knp->value.ui32);

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

int send_message (PBMSG::Packet pckt) {
  try {
    std::string pckt_serialized;
    pckt.SerializeToString (&pckt_serialized);
    zmq::message_t msg (pckt_serialized.size());
    memcpy ((void *)msg.data(), pckt_serialized.c_str(), pckt_serialized.size());
    if (!QUIET) socket.send (msg, ZMQ_NOBLOCK);
  } catch (int e) {
    std::cout << "Error number " << e << std::endl;
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
  UTIL::white();
  std::cout << "\nusage:  server [-h] [-p NUMBER] [-v] [-vlite]"; 
  std::cout << "\n    -h         prints this help/usage page";
  std::cout << "\n    -p PORT    use port PORT";
  std::cout << "\n    -v         run in verbose mode (print all queries and ZMQ packets)";
  std::cout << "\n    -vlite     prints time (and dtrace ticks (tick-4999)) for each sent message";
  std::cout << "\n    -d DELAY   wait DELAY<1000 ms instead of default 1000";
  std::cout << "\n    -q         quiet mode, prints diagnostic information and does not send messages";
  std::cout << "\n\n";
  UTIL::clear();
}

