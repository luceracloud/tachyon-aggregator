



#define COMBINE(a,b) (((uint64_t)a<<32)|(uint64_t)b)



namespace FASTBIT {
/*
 * Read config file and populate
 * table accordingly
 */
bool load_config (const char *fname, std::string *column_line, ibis::tablex *tbl) {
   
  std::vector<std::string> columns_vector;

  std::string columns;

  FILE *fconf;
  char *line = NULL;
  size_t len = 0;
  ssize_t read;
  char mode = 0;
  uint_fast8_t repeat = 0;

  /* Load file */
  fconf = fopen(fname, "r");
  if (fconf == NULL) {
    std::cout << "Unable to read configuration file\n"; 
    return 1;
  }

  /* Read and parse config file */
  while ((read = getline(&line, &len, fconf)) != -1) {
    if (line[0]==35 || line[0]==10) {
    } else if (line[0]==36) {
      mode = 2;
    } else if (mode==2) {
      mode = 1;
      repeat = atoi(line);
    } else if (mode==1) {
      mode = 2 + atoi(line);
      if (columns_vector.size()>0) columns_vector.clear();
    } else if (mode>2) {
      mode--;
      columns_vector.push_back (std::string(line).substr(0, strlen(line)-1));
      columns += ",";
      
      if (mode==2) {
        std::ostringstream oss;
        for (int i=0; i<repeat; i++) {
          for (int j=0; j<columns_vector.size(); j++) {
            oss << columns_vector.at(j);
            if (repeat>1) oss << i;
            oss << ","; 
          }
        }
        *column_line += oss.str();
      }
    }
  }

  /* Free memory before exiting & close file */
  if (line) free(line);
  fclose(fconf);
  
  column_line->erase (column_line->size() - 1);

  return 0;
}

/*
 * Read GENERAL data and append it to ostring
 */
bool read_gen (PB_MSG::Packet *pckt, std::ostringstream *line) {
  #ifdef FULLBIT
    // 64bit code here
  #else
    *line << pckt->time() << "," << pckt->ticks() << ",";
  #endif

  return 0;
}

/*
 * Read MEM data and append it to ostring
 */
bool read_mem (PB_MSG::Packet *pckt, std::ostringstream *line) {
  for (int i=0; i<pckt->mem_size(); i++) {
    const PB_MSG::Packet_Mem &mem_pckt = pckt->mem(i);
    #ifdef FULLBIT
    #else
      *line << COMBINE(mem_pckt.physmem_1(),mem_pckt.physmem_2()) << ",";
      *line << COMBINE(mem_pckt.rss_1(),mem_pckt.physmem_2()) << ",";
      *line << COMBINE(mem_pckt.pp_kernel_1(),mem_pckt.pp_kernel_2()) << ",";
      *line << COMBINE(mem_pckt.freemem_1(),mem_pckt.freemem_2()) << ",";
      *line << COMBINE(mem_pckt.physcap_1(),mem_pckt.physcap_2()) << ",";
      *line << COMBINE(mem_pckt.swap_1(),mem_pckt.swap_2()) << ",";
      *line << COMBINE(mem_pckt.swapcap_1(),mem_pckt.swapcap_2()) << ",";
    #endif
    }

  return 0;
}

/*
 * Read CPU data and append it to ostring 
 */
bool read_cpu (PB_MSG::Packet *pckt, std::ostringstream *line) {

  const uint_fast8_t NUMBER_OF_CORES = 16; // load from db.conf file 
  uint32_t usage[NUMBER_OF_CORES] = {};

  for (int i=0; i<pckt->cpu_size(); i++) {
    const PB_MSG::Packet_Cpu &cpu_pckt = pckt->cpu(i);
    usage[cpu_pckt.core()] = cpu_pckt.usage();
  }

  for (int i=0; i<NUMBER_OF_CORES; i++) {
    *line << usage[i] << ",";
  }

  return 0;
}

/*
 *
 */
bool read_disk (PB_MSG::Packet *pckt, std::ostringstream *line) {

  for (int i=0; i<pckt->disk_size(); i++) {
    const PB_MSG::Packet_Disk &disk_pckt = pckt->disk(i);
    *line << disk_pckt.instance() << ",";
    *line << COMBINE(disk_pckt.nread_1(),disk_pckt.nread_2()) << ",";
    *line << COMBINE(disk_pckt.nwritten_1(),disk_pckt.nwritten_2()) << ",";
    *line << disk_pckt.reads() << ",";
    *line << disk_pckt.writes() << ",";
    *line << COMBINE(disk_pckt.wtime_1(),disk_pckt.wtime_2()) << ",";
    *line << COMBINE(disk_pckt.wlentime_1(),disk_pckt.wlentime_2()) << ",";
    *line << COMBINE(disk_pckt.rtime_1(),disk_pckt.rtime_2()) << ",";
    *line << COMBINE(disk_pckt.rlentime_1(),disk_pckt.rlentime_2()) << ",";
  }

 return 0;
}

/*
 *
 */
bool read_net (PB_MSG::Packet *pckt, std::ostringstream *line) {

  for (int i=0; i<pckt->net_size(); i++) {
    const PB_MSG::Packet_Net &net_pckt = pckt->net(i);
    *line << net_pckt.instance() << ",";
    *line << COMBINE(net_pckt.obytes64_1(),net_pckt.obytes64_2()) << ",";
    *line << COMBINE(net_pckt.rbytes64_1(),net_pckt.rbytes64_2()) << ",";
    *line << COMBINE(net_pckt.opackets_1(),net_pckt.opackets_2()) << ",";
    *line << COMBINE(net_pckt.ipackets_1(),net_pckt.ipackets_2()) << ",";
  }

  //device, obytes64, rbytes64, opackets, ipackets
  return 0;
}

/*
 *
 */
bool read_proc (PB_MSG::Packet *pckt, std::ostringstream *line) {
  
  for (int i=0; i<pckt->process_size(); i++) {
    const PB_MSG::Packet_Process &proc_pckt = pckt->process(i);
    *line << proc_pckt.pid() << ",";
    *line << proc_pckt.execname() << ",";
    *line << proc_pckt.usage() << ",";
    *line << proc_pckt.cpu() << ",";
  }

  return 0;
}




}

/*
 * $GEN
 * 1
 * 9
 * time
 * ticks
 * physmem
 * rss
 * pp_kernel
 * freemem
 * physcap
 * swap
 * swapcap
 * $CPU
 * 16
 * 1
 * coreusage
 * $DISK
 * 13
 * 9
 * disk
 * nread
 * nwritten
 * reads
 * writes
 * wtime
 * wlentime
 * rtime
 * rlentime
 * $NET
 * 1
 * 5
 * device
 * obytes64
 * rbytes64
 * opackets
 * ipackets
 * $PROC
 * 20
 * 4
 * pid
 * execname
 * usage
 * cpu
 */












