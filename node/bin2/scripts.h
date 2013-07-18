/*
 *  scripts.h
 *
 *    Header file for C++ statistics
 *    server. Includes kstat & dtrace
 *    scripts in respective namespaces
 *
 *  CREATED:  17 JULY 2013
 *  UPDATED:  17 JULY 2013
 */
    
#define MEM_NUM 6
#define NET_NUM 4
#define DISK_NUM 6

namespace S {

  namespace MEM {
    int number = MEM_NUM; 
    std::string module[ MEM_NUM ] = { "unix", "unix", "unix", "memory_cap", "memory_cap", "memory_cap" };
    std::string name[ MEM_NUM ] = { "system_pages", "system_pages", "system_pages", "NULL", "NULL", "NULL" };
    std::string statistic[ MEM_NUM ] = { "freemem", "physmem", "pp_kernel", "rss", "swap", "swapcap" };      
  } 

  namespace NET {
    int number = NET_NUM;
    std::string module[ NET_NUM ] = { "link", "link", "link", "link" };
    std::string name[ NET_NUM ] = { "net0", "net0", "net0", "net0" };
    std::string statistic[ NET_NUM ] = { "obytes64", "rbytes64", "opackets", "ipackets" };      
  }

  namespace DISK {
    int number = DISK_NUM;
    std::string module[ MEM_NUM ] = { "sd", "sd", "sd", "sd", "sd", "sd" };
    std::string name[ MEM_NUM ] = { "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" };
    std::string statistic[ MEM_NUM ] = { "nread", "nwritten", "reads", "writes", "wtime", "wlentime" };      
  }

  
  namespace DTRACE {
    int number = 1;

    std::string trial = 
    "profile-4999\n"
    "{\n"
    "  @[execname]=count();\n"
    "}";

    std::string dist = "profile:::profile-4999\n{\n@P[cpu] = count();\n}";
    std::string proc = "profile:::profile-4999\n{\n@P[pid,execname,cpu] = count();\n}";
    std::string ticks = "profile:::tick-4999\n{\n@P = count();\n}";

 
  }


}



