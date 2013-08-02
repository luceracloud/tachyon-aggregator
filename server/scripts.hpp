/*
 *  scripts.hpp
 *
 *    Header file for C++ statistics
 *    server. Includes kstat & dtrace
 *    scripts in respective namespaces.
 *
 *  CREATED:  17 JULY 2013
 *  UPDATED:  23 JULY 2013
 */
    
#define MEM_NUM 7
#ifdef ZONE
#define NET_NUM 6 
#else
#define NET_NUM 8
#endif
#define DISK_NUM 6
#define DTRACE_NUM 6

namespace MEM {
  size_t number = MEM_NUM; 
  std::string module[ MEM_NUM ] = { "unix", "unix", "unix", "memory_cap", "memory_cap", "memory_cap", "memory_cap" };
  std::string name[ MEM_NUM ] = { "system_pages", "system_pages", "system_pages", "NULL", "NULL", "NULL", "NULL" };
  std::string statistic[ MEM_NUM ] = { "freemem", "physmem", "pp_kernel", "physcap", "rss", "swap", "swapcap" };      
} 

namespace NET {
  size_t number = NET_NUM;
#ifdef ZONE
  size_t num_instance = 1;
  std::string module[1][ NET_NUM ] = { { "link", "link", "link", "link", "link", "link" } };
  std::string name[1][ NET_NUM ] = { { "net0", "net0", "net0", "net0", "net0", "net0" } };
  std::string statistic[1][ NET_NUM ] = { { "obytes64", "rbytes64", "opackets", "ipackets", "ierrors", "oerrors" } }; 
#else
  size_t num_instance = 2;
  std::string module[2][ NET_NUM ] = {
    { "link", "link", "link", "link", "link", "link" },
    { "link", "link", "link", "link", "link", "link" } };
  std::string name[2][ NET_NUM ] = {
    { "ixgbe0", "ixgbe0", "ixgbe0", "ixgbe0", "ixgbe0", "ixgbe0" },
    { "int0", "int0", "int0", "int0", "int0", "int0" } };
  std::string statistic[2][ NET_NUM ] = {
    { "obytes64", "rbytes64", "opackets", "ipackets", "ierrors", "oerrors" },
    { "obytes64", "rbytes64", "opackets", "ipackets", "ierrors", "oerrors" } }; 
#endif
}

namespace DISK {
  size_t number = DISK_NUM;
  /* All functionality implemented within the
   * server program
   */
}

namespace DTRACE {
  size_t number = DTRACE_NUM;

  std::string dtrace[DTRACE_NUM] = {
    (std::string)"profile:::profile-4999\n{\n@[0,cpu] = count();\n}",
    (std::string)"profile:::profile-4999\n{\n@[1,cpu,execname,pid] = count();\n}",
    (std::string)"profile:::tick-4999\n{\n@[2] = count();\n}",
    (std::string)"syscall:::entry\n{\nself->begun=timestamp;\n}\nsyscall:::return\n{\nself->ended=timestamp;\n@[3]=quantize(self->ended-self->begun);\n}",
    std::string("profile:::profile-4999\n{\n@[4,pid] = count();\n}"),
    std::string("profile:::profile-4999\n{\n@[5,curthread] = count();\n}")
  };

}
  

