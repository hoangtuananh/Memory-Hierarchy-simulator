Memory-Hierarchy-simulator
==========================

  This code simulates a memory hierarchy consisting of a main memory and zero or more data caches (e.g., level 1, level 2). The following parameters can be controlled:

       main memory size
       number of cache levels
       for each cache:
         cache line size (power of 2)
         number of sets (power of 2)
         number of ways
         replacement method: least recently used (LRU), round robin, or random
         allocate policy   : read allocate or write allocate
         write policy      : write back or write through
         
  Code exists to test various combinations of parameter settings to verify correct operation and to report cache performance statistics.
  The simulated memory system is byte addressable and 32 bits wide. This means the following:
         - A word is 32 bits (4 bytes).
         - Clients can read and write bytes or words of memory.
         - Clients use byte addresses for all accesses. The variable "address" always
            refers to a byte address.
         - Internally (within the superclass Memory and all its subclasses) all memory
           transactions (reads and writes) are in units of words.
         - Internally, all addresses are word addresses. The variable "wordAddress" always
           refers to a word address. address == wordAddress << 2.
           
  Almost all contemporary memory systems are byte addressable, and for efficiency conduct memory transactions in much wider units. Current typical computers use a memory width of 32, 64, or 128 bits.

