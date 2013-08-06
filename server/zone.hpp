/* Defines the datatype for
 * the "zone" struct, not
 * to be confused with the
 * "gzone" struct, defined
 * elsewhere.
 *
 * These are (unfortuntately)
 * necessary because of the
 * way that data is returned
 * from kstat & dtrace.
 */


#include "pckt.pb.h"

struct Zone {
  public:
    Zone (std::string n, bool g) {
      this->name = n;
      if (g) {
        this->set_global (true);
        // do not init any mappables
      } else {
        this->set_global (false);
        packet->add_net(); 
      }
    }
    ~Zone () {
      free (&(this->packet));
    }
    
    /* General Statistics */ 
    void set_time (uint64_t t) {
      this->packet->set_time (t); 
    }
    void set_ticks (uint64_t t) {
      this->packet->set_ticks (t);
    }
    void set_threads (uint32_t t) {
      this->packet->set_threads (t);
    }
    void set_processes (uint32_t p) {
      this->packet->set_processes (p);
    }
    void set_global (bool is_global) {
      global_zone = is_global;
    }
    
    /* Simple additions */
    void add_cpu (uint32_t c, uint32_t u) {
      PBMSG::Packet_Cpu *cpu = packet->add_cpu();
      cpu->set_core (c);
      cpu->set_usage (u);
    }


    /* Mappable additions */
    void add_network (std::string d, std::string s, uint64_t v) {
      if (this->global_zone) {
        
      } else {
        PBMSG::Packet_Net net = this->packet->net(0);
        //net.set_obytes64 (v);
        //*(this->packet->net(0)).set_obytes64 (v);
        //*(this->packet).net(0).set_obytes64 (v);
      }
    }
    void add_disk (std::string d, std::string s, uint64_t v) {
      if (this->global_zone) {
        
      } else {
        
      }
    }


   /* Return the packet */ 
   PBMSG::Packet ReturnPacket () {
    return *(this->packet);
   } 
    
  private:
    bool global_zone;
    PBMSG::Packet *packet;
    std::map <std::string, uint32_t> net_map;
    std::map <std::string, uint32_t> disk_map;
    std::string name;  




};







struct zonestat {

/*
  message Packet {

  repeated zonename

  message Mem {
    optional uint64 rss = 1;
    optional uint64 physcap = 2;
    optional uint64 swap = 3;
    optional uint64 swapcap = 4;
  }

  message Net {
    optional string instance = 1;
    optional uint64 obytes64 = 2;
    optional uint64 rbytes64 = 3;
    optional uint64 opackets = 4;
    optional uint64 ipackets = 5;
    optional uint32 ierrors = 6;
    optional uint32 oerrors = 7;
  }

  message Disk {
    required string instance = 1;
    optional uint64 nread = 2;
    optional uint64 nwritten = 3;
    optional uint32 reads = 4;
    optional uint32 writes = 5;
    optional uint64 rtime = 6;
    optional uint64 wtime = 7;
    optional uint64 rlentime = 8;
    optional uint64 wlentime = 9;
    optional uint32 harderror = 10;
    optional uint32 softerror = 11;
    optional uint32 tranerror = 12;
  }
  
  message Process {
    optional uint32 pid = 1;
    optional string execname = 2;
    optional uint32 usgae = 3;
    optional uint32 cpu = 4;
  }

  message CallFreq {
    required string name = 1;
    optional uint64 time = 2;
    optional uint32 value = 3;
  }
*/





};





