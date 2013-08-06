/*
 *  scripts.hpp
 *
 *    Header file for C++ statistics
 *    server. Includes kstat & dtrace
 *    scripts in respective namespaces.
 *
 *  CREATED:  17 JUL 2013
 *  UPDATED:   6 AUG 2013
 */

#define DISK_NUM 6
#define DTRACE_NUM 6

namespace MEM {
	/* Global-zone specific stuff */
	const size_t GZ_size = 5;
	std::string GZ_modl[ GZ_size ] = { "unix", "unix", "unix", "unix", "unix" };
	std::string GZ_name[ GZ_size ] = { "system_pages", "system_pages", "system_pages", "system_pages", "system_pages" }; 
	std::string GZ_stat[ GZ_size ] = { "physmem", "pp_kernel", "freemem", "nalloc_calls", "nfree_calls" };

	/* Other */
	const size_t size = 4;
	std::string modl[ size ] = { "memory_cap", "memory_cap", "memory_cap", "memory_cap" };
	std::string stat[ size ] = { "rss", "physcap", "swap", "swapcap" };
  //size_t number = 4; 
  //std::string module[ MEM_NUM ] = { "unix", "unix", "unix", "memory_cap", "memory_cap", "memory_cap", "memory_cap" };
  //std::string name[ 7 ] = { "system_pages", "system_pages", "system_pages", "NULL", "NULL", "NULL", "NULL" };
  //std::string statistic[ 7 ] = { "freemem", "physmem", "pp_kernel", "physcap", "rss", "swap", "swapcap" };      
} 

namespace NET {
	/* Global-zone specific stuff */
	const size_t GZ_size = 0;
	std::string GZ_modl[ GZ_size ] = {};
	std::string GZ_name[ GZ_size ] = {};
	std::string GZ_stat[ GZ_size ] = {};

	/* Other */
	const size_t size = 6;
  std::string modl[ size ] = { "link", "link", "link", "link", "link", "link" };
  std::string stat[ size ] = { "obytes64", "rbytes64", "opackets", "ipackets", "oerrors", "ierrors" };
}

namespace DISK {
  /* Global-zone specific stuff */
  const size_t GZ_size = 0;
  std::string GZ_modl[ GZ_size ] = {};
  std::string GZ_name[ GZ_size ] = {};
  std::string GZ_stat[ GZ_size ] = {};

  /* Other */
  const size_t size = 0;
  std::string modl[ size ] = {};
  std::string stat[ size ] = {};
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
  
namespace TEST {

  const int a = 6;

  std::string module[a] = { "zone_zfs", "memory_cap", "memory_cap", "memory_cap", "memory_cap", "zone_zfs" };
  std::string statistic[a] = { "reads", "rss", "physcap", "swap", "swapcap", "writes" };



}









