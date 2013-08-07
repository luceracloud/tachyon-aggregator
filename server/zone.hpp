/* Defines the datatype for
 * the "zone" struct, which
 * can be toggled either GZ
 * or NGZ.
 *
 * These are (unfortuntately)
 * necessary because of the
 * way that data is returned
 * from kstat & dtrace.
 */


#include "pckt.pb.h"

class Zone {
  public:
    Zone (std::string n, bool g) {
      packet = (PBMSG::Packet *)(new PBMSG::Packet);
      packet->set_name (n);
      packet->add_mem();
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

    /* Memory additions */
    void add_mem (std::string *n, uint64_t x) {
      if (*n == "rss") {
        this->packet->mutable_mem(0)->set_rss (x);
      } else if (*n == "physcap") {
        this->packet->mutable_mem(0)->set_physcap (x);
      } else if (*n == "swap") {
        this->packet->mutable_mem(0)->set_swap (x); 
      } else if (*n == "swapcap") {
        this->packet->mutable_mem(0)->set_swapcap (x);
      } else if (*n == "physmem") {
        this->packet->mutable_mem(0)->set_physmem (x);
      } else if (*n == "pp_kernel") {
        this->packet->mutable_mem(0)->set_pp_kernel (x);
      } else if (*n == "freemem") {
        this->packet->mutable_mem(0)->set_freemem (x);
      } else if (*n == "nalloc_calls") {
        this->packet->mutable_mem(0)->set_nalloc_calls (x);
      } else if (*n == "nfree_calls") {
        this->packet->mutable_mem(0)->set_nfree_calls (x);
      } else {
        UTIL::yellow();
        std::cout << "WARN: encountered unknown type in memory @Zone.hpp:" << __LINE__ << std::endl;
        UTIL::clear();
      }
    }
 
    /* Simple additions */
    void add_cpu (uint32_t c, uint32_t u) {
      PBMSG::Packet_Cpu *cpu = this->packet->add_cpu();
      cpu->set_core (c);
      cpu->set_usage (u);
    }
    void add_zone (std::string *s) {
      this->packet->add_zonename (*s);
    }
    void add_process (uint32_t p, std::string *e, uint32_t u, uint32_t c) {
      PBMSG::Packet_Process *proc = this->packet->add_process();
      proc->set_pid (p);
      proc->set_execname (*e);
      proc->set_usage (u);
      proc->set_cpu (c);
    }
    void add_callfreq (std::string *n, uint64_t t, uint32_t v) {
      PBMSG::Packet_CallFreq *callfreq = this->packet->add_callfreq();
      callfreq->set_name (*n);
      callfreq->set_time (t);
      callfreq->set_value (v);
    }

    /* Mappable additions */
    void add_network (std::string *d, std::string *s, uint64_t v) {
      PBMSG::Packet_Net *net;
     
      // WARN TODO SOMETHING NOT ENTIRELY CORRECT HERE
 
      if (this->global_zone) {
        if (this->net_map.find(*d) == this->net_map.end()) {
          net = this->packet->add_net();
          net->set_instance (*d); // PIECEMEAL TODO
          this->net_map.insert (std::make_pair (*d, net_map.size()));
        } else {
          net = this->packet->mutable_net (this->net_map[*d]);
        }
      } else {
        net = this->packet->mutable_net (0);
      }
      
      if (*s == "instance") {
        net->set_instance (*d);    // TODO, ENSURE THIS WORKS!
      } else if (*s == "obytes64") {
        net->set_obytes64 (v);
      } else if (*s == "rbytes64") {
        net->set_rbytes64 (v);
      } else if (*s == "opackets") {
        net->set_opackets (v);
      } else if (*s == "ipackets") {
        net->set_ipackets (v);
      } else if (*s == "oerrors") {
        net->set_oerrors (v);
      } else if (*s == "ierrors") {
        net->set_ierrors (v);
      } else {
        UTIL::yellow();
        std::cout << "WARN: encountered unknown type in network @Zone.hpp:" << __LINE__ << std::endl;
        UTIL::clear();
      }

    }
    void add_disk (std::string *d, std::string *s, uint64_t v) {
      PBMSG::Packet_Disk *disk;
      
      if (this->global_zone) {
        if (this->disk_map.find(*d) == this->disk_map.end()) {
          std::cout << "Did not find disk zone\n";
        } else {
          
        }
      } else {
        
      }
    }

    /* Return the packet */ 
    PBMSG::Packet ReturnPacket () {
      return *(this->packet);
    } 
    void print_zone () {
      this->packet->PrintDebugString ();
      //this->packet->DebugString ();
    }
    
  private:
    bool global_zone;
    PBMSG::Packet *packet;
    std::map <std::string, uint32_t> net_map;
    std::map <std::string, uint32_t> disk_map;


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





