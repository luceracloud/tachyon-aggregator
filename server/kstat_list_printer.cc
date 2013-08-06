#include <kstat.h>
#include <iostream>
#include <iomanip>

int main (int argc, char **argv) {

  size_t counter = 0;
  kstat_ctl_t *kc = kstat_open();
  kstat_t *ksp = kstat_lookup (kc, NULL, -1, NULL);

  while (ksp != NULL) {
    if (counter%15==0) {
      printf ("\n%-10s  %-10s  %-10s\n", "CLASS", "MODULE", "NAME");
      printf ("==================================\n");
    }
    
    printf ("\n%-10s  %-10s  %-10s\n", ksp->ks_class, ksp->ks_module, ksp->ks_name);
    printf ("----------------------------------\n");
    printf (" > id       %d\n", ksp->ks_kid);
    printf (" > ndata    %d\n", ksp->ks_ndata);
    printf (" > type     ", ksp->ks_type);
    char *datap, *namep;
    int32_t size;
    kstat_named_t *names;
    switch (ksp->ks_type) {
      case KSTAT_TYPE_RAW:
        std::cout << "RAW\n";
        break;
      case KSTAT_TYPE_NAMED:
        std::cout << "NAMED\n";

        break;
      case KSTAT_TYPE_INTR:
        std::cout << "INTERRUPT\n";
        break;
      case KSTAT_TYPE_IO:
        std::cout << "I/O\n";
        break;
      case KSTAT_TYPE_TIMER:
        std::cout << "TIMER\n";
        break;
      default:
        std::cout << "The spec appears to be incorrect...\n";
    }


    ksp = ksp->ks_next;
    counter++;
  }


  return 0;
}
