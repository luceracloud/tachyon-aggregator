/*
 *  scripts.hpp
 *
 *    Header file for C++ statistics
 *    server. Includes kstat & dtrace
 *    scripts in respective namespaces.
 *
 *  CREATED:  17 JUL 2013
 *  UPDATED:   8 AUG 2013
 */


namespace MEM {
	/* Global-zone specific scripts */
	const size_t GZ_size = 5;
	std::string GZ_modl[ GZ_size ] = { "unix", "unix", "unix", "unix", "unix" };
	std::string GZ_name[ GZ_size ] = { "system_pages", "system_pages", "system_pages", "system_pages", "system_pages" }; 
	std::string GZ_stat[ GZ_size ] = { "physmem", "pp_kernel", "freemem", "nalloc_calls", "nfree_calls" };

	/* Other */
	const size_t size = 4;
	std::string modl[ size ] = { "memory_cap", "memory_cap", "memory_cap", "memory_cap" };
	std::string stat[ size ] = { "rss", "physcap", "swap", "swapcap" };
} 

namespace NET {
	/* NGZ scripts */
	const size_t size = 6;
  std::string modl[ size ] = { "link", "link", "link", "link", "link", "link" };
  std::string stat[ size ] = { "obytes64", "rbytes64", "opackets", "ipackets", "oerrors", "ierrors" };
}

namespace DISK {
  /* Global-zone specific scripts */
  const size_t GZ_size = 8;
  std::string GZ_modl[ GZ_size ] = { "sd", "sd", "sd", "sd", "sd", "sd", "sd", "sd" };
  std::string GZ_stat[ GZ_size ] = { "nread", "nwritten", "reads", "writes", "rtime", "wtime", "rlentime",
                "wlentime" };

  /* Other */
  const size_t size = 14;
  std::string modl[ size ] = { "zone_zfs", "zone_zfs", "zone_zfs", "zone_zfs", "zone_zfs", "zone_zfs",
        "zone_vfs", "zone_vfs", "zone_vfs", "zone_vfs", "zone_vfs", "zone_vfs", "zone_vfs", "zone_vfs" };
  std::string stat[ size ] = { "nread", "nwritten", "reads", "writes", "rtime", "rlentime", 
        "nread", "nwritten", "reads", "rlentime", "rtime", "wlentime", "writes", "wtime" };

}

namespace DTRACE {
  size_t number = 6; 

  std::string dtrace[6] = {
    (std::string)"profile:::profile-4999\n{\n@[0,zonename,cpu] = count();\n}",

    (std::string)"profile:::profile-4999\n{\n@[1,zonename,cpu,execname,pid] = count();\n}",

    (std::string)"profile:::tick-4999\n{\n@[2,zonename] = count();\n}",

    (std::string)"syscall:::entry\n{\nself->begun=timestamp;\n}\nsyscall:::return\n"
        "{\nself->ended=timestamp;\n@[5,zonename]=quantize(self->ended-self->begun);\n}",

    std::string("profile:::profile-4999\n{\n@[3,zonename,pid] = count();\n}"),

    std::string("profile:::profile-4999\n{\n@[4,zonename,curthread] = count();\n}")
  };
}

