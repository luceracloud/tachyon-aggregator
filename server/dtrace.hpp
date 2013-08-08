/*
 *  dtrace.hpp
 *
 *   Includes a nice library of functions 
 *   that allow easy interface with libdtrace 
 *   from a C++ program.
 *
 *   CREATED:   8 AUG 2013
 *   EDITED:    8 AUG 2013
 */

#define EX32(addr) *((uint32_t *)addr)

#include <dtrace.h>

extern bool VERBOSE;
extern bool VERBOSE2;
extern std::map <std::string, Zone*> ZoneData;
extern std::map <size_t, std::string> ZoneIndices;

namespace DTRACE {

// Debugging function, prints
// something from the libdtrace
// aggreate in the form it was
// originally written.
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

/* Initialize dtrace program (passed by handle and string) */
int init (dtrace_hdl_t **this_dtp, std::string this_prog) {

  int done = 0;
  int err;
  dtrace_prog_t *prog;
  dtrace_proginfo_t info;

  if ((*this_dtp = dtrace_open (DTRACE_VERSION, 0, &err)) == NULL) {
    UTIL::red();
    std::cout << "ERROR: Failed to init dtrace" << std::endl;
    UTIL::clear();
    return -1;
  }

  if ((prog = dtrace_program_strcompile (*this_dtp, (const char *)this_prog.c_str(),
    DTRACE_PROBESPEC_NAME, 0, 0, NULL)) == NULL) {
    UTIL::red();
    std::cout << "ERROR: Failed to compile program" << std::endl;
    UTIL::clear();
    return -1;
  }

  if (dtrace_program_exec (*this_dtp, prog, &info) == -1) {
    UTIL::red();
    std::cout << "ERROR: Failed to init probes" << std::endl;
    UTIL::clear();
    return -1;
  }

  return 0;
}

/* For dtrace "commandline" */
int setopts (dtrace_hdl_t **this_g_dtp) {

  if (dtrace_setopt(*this_g_dtp, "strsize", "32") == -1) {
    UTIL::red();
    std::cout << "DTrace failed to set 'strsize'" << std::endl;
    UTIL::clear();
  }
  if (dtrace_setopt(*this_g_dtp, "bufsize", "1k") == -1) {
    UTIL::red();
    std::cout << "DTrace failed to set 'bufsize'" << std::endl;
    UTIL::clear();
  }
  if (dtrace_setopt(*this_g_dtp, "aggsortrev", NULL) == -1) {
    UTIL::red();
    std::cout << "DTrace failed to set 'aggsortrev'" << std::endl;
    UTIL::clear();
  }
  if (dtrace_setopt(*this_g_dtp, "aggsize", "1M") == -1) {
    UTIL::red();
    std::cout << "DTrace failed to set 'aggsize'" << std::endl;
    UTIL::clear();
  }
  if (dtrace_setopt(*this_g_dtp, "aggrate", "2msec") == -1) {
    UTIL::red();
    std::cout << "DTrace failed to set 'aggrate'" << std::endl;
    UTIL::clear();
  }

  return 0;
}

/* 
 * Given a bucket id, return the max
 * value. The min of the value is
 * simply (max/2)+1.
 */
inline int64_t max_quantize_range (int bckt_id) {
  if (bckt_id < DTRACE_QUANTIZE_ZEROBUCKET) return -1; 
  else if (bckt_id == DTRACE_QUANTIZE_ZEROBUCKET) return 0;
  else return DTRACE_QUANTIZE_BUCKETVAL (bckt_id);
}
 
static int aggwalk (const dtrace_aggdata_t *agg, void *arg) {

  /* Where to store the data */
  struct {
    int type;
    uint32_t core;
    uint64_t usage;
    uint32_t pid;
    std::string zonename;
    std::string name;
  } data;

  dtrace_aggdesc_t *aggdesc = agg->dtada_desc;

  for (size_t i=1; i<aggdesc->dtagd_nrecs; i++) {
    // Each of these relates to a horizontal element of the aggregate
    
    const dtrace_recdesc_t *rec = &aggdesc->dtagd_rec[i];
    caddr_t addr = agg->dtada_data + rec->dtrd_offset;

    if (i==1) {
      data.type = EX32(addr);
    } else if (i==2) {
      data.zonename = (const char *)addr;
    }

    /* Ugly but readable way to do this */
    
    // usage
    if (data.type==0) {
      if (i==3) {
        data.core = EX32(addr);
      } else if (i==4) {
        ZoneData[data.zonename]->add_cpu(data.core,EX32(addr));
      }
    } 
    
    // process
    if (data.type==1) {
      if (i==3) {
        data.core = EX32(addr);
      } else if (i==4) {
        data.name = (const char *)addr;
      } else if (i==5) {
        data.pid = EX32(addr);
      } else if (i==6) {
        ZoneData[data.zonename]->add_process(data.pid, &data.name, EX32(addr), data.core);
      }
    }

    // tick count
    if (data.type==2) {
      if (i==3) {
        ZoneData[data.zonename]->set_ticks(EX32(addr));
      }
    }

    // process count
    if (data.type==3) {
      if (i==4) {
        ZoneData[data.zonename]->add_process();
      }
    }

    // thread count
    if (data.type==4) {
      if (i==4) {
        ZoneData[data.zonename]->add_thread();
      }
    }

    // callfreq
    if (data.type==5 && i>2) {

      if (rec->dtrd_action != DTRACEAGG_QUANTIZE) {
        UTIL::yellow();
        std::cout << "Unexpected form of DTrace data when expecting quantize" << std::endl;
        UTIL::clear();
        break;
      }
      
      const int64_t *agg_data = (int64_t *)(agg->dtada_data + rec->dtrd_offset);
      for (size_t range_count=0; range_count<DTRACE_QUANTIZE_NBUCKETS; range_count++) {
        if (!agg_data[range_count]) continue; // Don't add anything if we have zero data
        
        if (VERBOSE) std::cout << max_quantize_range (range_count) << " : " << agg_data[range_count];
  
        int64_t lrange = max_quantize_range (range_count);

        std::string call_name = "all"; 
        ZoneData[data.zonename]->add_callfreq(&call_name, lrange, agg_data[range_count]);
      }
    }

  }

  return (DTRACE_AGGWALK_REMOVE);
}

}

