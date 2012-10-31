package memoryHierarchy;

import java.util.*;

// ***********************
// *                     *
// *   Memory Hierarchy  *
// *                     *
// ***********************

/*
 * This code simulates a memory hierarchy consisting of a main memory and zero or more data
 * caches (e.g., level 1, level 2). The following parameters can be controlled:
 * 
 *      main memory size
 *      number of cache levels
 *      for each cache:
 *        cache line size (power of 2)
 *        number of sets (power of 2)
 *        number of ways
 *        replacement method: least recently used (LRU), round robin, or random
 *        allocate policy   : read allocate or write allocate
 *        write policy      : write back or write through
 *        
 * Code exists to test various combinations of parameter settings to verify correct
 * operation and to report cache performance statistics.
 * 
 * The simulated memory system is byte addressable and 32 bits wide. This means the
 * following:
 *        - A word is 32 bits (4 bytes).
 *        - Clients can read and write bytes or words of memory.
 *        - Clients use byte addresses for all accesses. The variable "address" always
 *           refers to a byte address.
 *        - Internally (within the superclass Memory and all its subclasses) all memory
 *          transactions (reads and writes) are in units of words.
 *        - Internally, all addresses are word addresses. The variable "wordAddress" always
 *          refers to a word address. address == wordAddress << 2.
 *          
 * Almost all contemporary memory systems are byte addressable, and for efficiency conduct
 * memory transactions in much wider units. Current typical computers use a memory width of
 * 32, 64, or 128 bits.
 */
public class MemoryHierarchy
{
  // ********************************
  // *                              *
  // *  Memory Abstract Superclass  *
  // *                              *
  // ********************************
  /*
   * A Memory object (any concrete subclass of Memory) can read and write bytes or words,
   * and keeps count of the number of reads and writes. For word transactions, the low
   * two bits of the address are ignored. Bytes are read by reading the word that contains
   * the byte, and then extracting it. All subclasses provide word reads only and inherit
   * the ability to do byte reads from this superclass.
   * 
   * Byte writes are more complex. This superclass provides the ability to do byte writes
   * by first reading the word that contains the byte, then modifying the byte, then writing
   * back the modified word. This is a very simple way of limiting all internal transactions
   * to words, and subclasses can inherit it (as MainMemory does) or override it and use it
   * in the overriding function (as Cache and MemoryArray do). The only reason that subclasses
   * override writeByte is to provide better accounting of memory transactions and cache hits,
   * accounting that better simulates real hardware.
   */
  public abstract class Memory
  {
    // To create a concrete kind of memory, extend this class and override these methods.
    // Do not call these methods anywhere outside of this class, use the non-abstract
    // methods below. This design allows subclasses to inherit the access counting and
    // byte transaction handling without doing anything. I put "override" in the names
    // to remind clients not to call these.
    abstract int   readOverride(int wordAddress);
    abstract void writeOverride(int wordAddress, int data);
    
    // returns  Total memory size in bytes.
    // note     Clients can call this.
    public abstract int size();
    
    // Normally public fields are discouraged, but I don't feel like adding getters and
    // setters. C# does this better.
    public int readCount, writeCount;
    
    // effect   Reset the access counts
    // note     Subclasses can override this to do their own resetting, but must
    //          call this too.
    public void reset()
    {
      readCount  = 0;
      writeCount = 0;
    }
    
    // returns  Word at specified byte address.
    // effect   Update access counts.
    // note     Low two bits of address are ignored.
    public final int readWord(int address)
    {
      ++readCount;
      return readOverride(address >> 2);
    }
    
    // effects  Write specified word data to specified byte address.
    //          Update access counts.
    // note     Low two bits of address are ignored.
    public final void writeWord(int address, int data)
    {
      ++writeCount;
      writeOverride(address >> 2, data);
    }
    
    // returns  Byte at specified address
    // effect   Update access counts
    public final int readByte(int address)
    {
      int data = readWord(address);
      return (data >> 8 * (address & 3)) & 0xFF;
    }
    
    // effects  Write specified byte data to specified address by doing a
    //          read/modify/write. Update access counts.
    // note     This may be overridden for subclasses that want to pass
    //          byte writes to another Memory class
    public void writeByte(int address, int data)
    {
      int byteShift = 8 * (address & 3);
      int byteMask = 0xFF << byteShift;
      int word = readOverride(address >> 2);  // don't count this as a read
      word = (word & ~byteMask) | ((data << byteShift) & byteMask);
      writeWord(address, word);
    }
    
    // returns  Total number of accesses
    public int accesses()
    {
      return readCount + writeCount;
    }
  }

  // ***********************************
  // *                                 *
  // *  Main Memory Concrete Subclass  *
  // *                                 *
  // ***********************************
  /*
   * This is always the lowest level of a specific memory hierarchy. It holds the
   * entire contents of the simulated memory.
   */
  public class MainMemory extends Memory
  {
    // This is the entire memory. Reading and writing simply accesses this array.
    private int[] mem;
    
    // effect   Construct memory of specified size in bytes.
    public MainMemory(int bytes)
    {
      mem = new int[bytes >> 2];
    }
    
    @Override
    int readOverride(int wordAddress)
    {
      return mem[wordAddress];
    }

    @Override
    void writeOverride(int wordAddress, int data)
    {
      mem[wordAddress] = data;
    }

    @Override
    public int size()
    {
      return mem.length << 2;
    }
    
  }
  
  // **************************
  // *                        *
  // *  Replacement Policies  *
  // *                        *
  // **************************
  
  public enum ReplacementPolicy
  {
    LRU,
    ROUND_ROBIN,
    RANDOM
  }
    
  // ***********************
  // *                     *
  // *  Misc Useful Stuff  *
  // *                     *
  // ***********************
  
  // Return a mask where the specified number of least significant bits are 1 and
  // all other bits are 0.
  static private int mask(int bits)
  {
    return (1 << bits) - 1;
  }
  
  // **********************************
  // *                                *
  // *  Data Cache Concrete Subclass  *
  // *                                *
  // **********************************
  /*
   * Zero or more Cache objects (subclasses of Memory) can be created to be higher
   * levels of the memory hierarchy, above the one required MainMemory object.
   * 
   * Cache lines must be a power of 2 words. A Cache is n-way set associative. The number of
   * sets must be a power of 2; the number of ways (n) must be at least 1 but need not be a
   * power of two. Line size, number of sets, and number of ways are established at
   * construction and cannot be changed (but a different Cache can be constructed).
   * 
   * If the number of ways is 1, the cache is direct mapped. If the number of sets is 1,
   * the cache is fully associative. Real computers typically use data caches with 2 or 4
   * ways. The total size of the cache in bytes is
   *             (number of sets) * (number of ways) * (line size in bytes).
   *
   * A word address has the following fields:
   * 
   *         MSB                           LSB
   *        -----------------------------------
   *        |      tag      |  set  |  offset |
   *        -----------------------------------
   * 
   * "offset" has logLineWords bits. "set" has logSets bits. "tag" has all leftover most
   * significant bits. For a fully associative cache there is one set, logSets = 0, and
   * the "set" field has no bits.
   */
  public class Cache extends Memory
  {
    // **** Private fields ****
  
    private int logSets;        // log2(number of sets), >= 0
    private int logLineWords;   // log2(number of words in a cache line), > 0
    private int numWays;        // number of ways, > 0
    private Set[] sets;         // array of sets in this cache
    
    private Memory lowerLevel;  // The lower level of memory to which to pass transactions,
                                // which may be a MainMemory or another Cache.

    private int hitCount;       // Total number of hits since reset()
    
    // Policies
    private boolean           writeAllocate;  // false for read allocate
    private boolean           writeThru;      // false for write back
    private ReplacementPolicy replace;        // see enum above
    public boolean            passByteWrites; //

    // **** Cache frame nested class ****
    
    // A cache Frame holds the state of one cache line.
    private class Frame
    {
      public int      tag;            // tag field of the memory address from which this
                                      // line came
      public boolean  valid;          // this line holds valid data (it has been allocated)
      public boolean  dirty;          // this line holds data that needs to be written back
                                      // to lowerLevel
      public int      lastAccessTime; // Used by LRU replacement method
      public int[]    line;           // The cache line
      
      // effect   Construct an invalid cache frame
      public Frame()
      {
        line = new int[1 << logLineWords];
        valid = false;
      }
      
      // effect   Read the cache line containing wordAddress from lowerLevel.
      // note     The frame becomes valid and clean.
      public void load(int wordAddress)
      {
        //! Set tag from wordAddress. Look for useful helper method below.
        //! Initialize valid and dirty.
        
        valid   =   true;
        dirty   =   false;
        tag     =   tagNum(wordAddress);
        
        //! Loop over all words in line and read the word from lowerLevel memory.
        //! You must use the readWord method to read the words (not readOverride).
        //! Keep in mind that you have been given a wordAddress above, but readWord
        //! expects a byte address. The starting word address in memory of the line
        //! you are reading is obtained from the wordAddress argument by masking off
        //! (clearing) the low order logLineWords bits. Look around for a helper
        //! method that creates masks.
        
        int memAddress = wordAddress & (~mask(logLineWords));
        for ( int i = 0; i < line.length; i++){
            line[i]   = lowerLevel.readWord((memAddress + i) << 2);
        }
        
        
      }
      
      // effect   If the frame is valid and dirty, write it back to lowerLevel.
      // notes    The set number must be given so that we know where to write the line.
      //          The rest of the address information comes from tag.
      //          The frame becomes clean, stays valid.
      public void writeback(int set)
      {
        //! Only do something here if the frame is valid and dirty. Otherwise just return.
        //! Loop over all words in line and write the word to lowerLevel memory.
        //! You must use the writeWord method to write the words (not writeOverride).
        //! Keep in mind that writeWord expects a byte address. The starting word address
        //! in memory of the line you are reading is obtained from the set argument and
        //! the frame's tag. See the comment above that shows the fields of a word
        //! address. Remember to clear the dirty flag.
        if (valid == true && dirty == true){
            int memAddress = (tag << (logLineWords + logSets)) + (set<<logLineWords);
        
            for ( int i = 0; i < line.length; i++){
                lowerLevel.writeWord((memAddress + i) << 2, line[i]);
            }
            
            //clear the dirty flag
            dirty = false;
        }
        
      }
    
    }
    // **** Cache set nested class ****
    
    private class Set
    {
      public int roundRobinIndex;   // Used by round-robin replacement.
      public Frame[] ways;          // The array of ways (Frames) in this set.
      
      // effect   Construct a set of invalid ways. Clear roundRobinIndex.
      public Set()
      {
        ways = new Frame[numWays];
        int way;
        for (way = 0; way < numWays; ++way)
          ways[way] = new Frame();
        roundRobinIndex = 0;
      }
    }
    
    // **** Getters and setters for cache policies ****

    public boolean getWriteAllocate()
    {
      return writeAllocate;
    }
    
    public void setWriteAllocate(boolean wa)
    {
      writeAllocate = wa;
    }
    
    public boolean getWriteThru()
    {
      return writeThru;
    }
    
    public void setWriteThru(boolean wt)
    {
      // If the cache is changing from write back to write through, flush it so that
      // all frames are clean as a write through cache would be. Cache stays valid.
      if (wt && !writeThru)
        flush(false);
      
      writeThru = wt;
    }
    
    public ReplacementPolicy getReplace()
    {
      return replace;
    }
    
    public void setReplace(ReplacementPolicy rp)
    {
      // If the cache is becoming LRU, flush and invalidate it because all of the
      // lastAccessTimes have not been maintained and would cause malfunction.
      if (rp == ReplacementPolicy.LRU && replace != rp)
        flush(true);
      
      replace = rp;
    }
    
    // **** Get various cache statistics ****
    
    // returns  Number of cache hits since last reset
    public int hits()
    {
      return hitCount;
    }

    // returns  Number of cache misses since last reset
    public int misses()
    {
      return accesses() - hits();
    }

    // returns  Cache hit rate as a fraction of total accesses
    public double hitRate()
    {
      return (double)hits() / accesses();
    }
    
    // returns  Cache miss rate as a fraction of total accesses
    public double missRate()
    {
      return (double)misses() / accesses();
    }
    
    // **** Get fields of a wordAddress ****

    // returns  Tag field of specified wordAddress
    private int tagNum(int wordAddress)
    {
      return wordAddress >> logLineWords + logSets;
    }
    
    // returns  Set field of specified wordAddress
    private int setNum(int wordAddress)
    {
      return (wordAddress >> logLineWords) & mask(logSets);
    }
    
    // returns  Offset field of specified wordAddress
    private int lineOffset(int wordAddress)
    {
      return wordAddress & mask(logLineWords);
    }
    
    // **** Constructor ****
    
    public Cache(int logSets, int logLineBytes, int numWays, Memory lowerLevel)
    {
      this.logLineWords = logLineBytes - 2;
      this.logSets      = logSets;
      this.numWays      = numWays;
      this.lowerLevel   = lowerLevel;
      
      writeAllocate  = false;
      writeThru      = false;
      replace        = ReplacementPolicy.LRU;
      passByteWrites = true;
      
      hitCount = 0;
      
      sets = new Set[1 << logSets];
      int set;
      for (set = 0; set < sets.length; ++set)
        sets[set] = new Set();
    }

    // **** Fundamental cache operations ****
    
    // effects  Write all dirty lines back to lowerLevel, so that cache becomes
    //          clean and is coherent with lowerLevel.
    //          If invalidate is true, also set all frames to invalid.
    public void flush(boolean invalidate)
    {
      int set;
      for (set = 0; set < sets.length; ++set)
        for (Frame frame : sets[set].ways)
        {
          frame.writeback(set);
          if (invalidate)
            frame.valid = false;
      
        }
    }
    
    // returns  The frame containing the specified wordAddress if it is in the
    //          cache, otherwise null.
    // effect   Maintains LRU data.
    private Frame lookup(int wordAddress)
    {
      Frame frame = null;
      
      //! Get the set and tag associated with the wordAddress. Look above for
      //! helper functions.
      
      int tag = tagNum(wordAddress);
      int set = setNum(wordAddress);
      
      //! Look for the tag in all valid ways of the set. If you find it, remember the
      //! frame where it was. If you are only implementing a direct mapped cache
      //! there is only one way so you on;y have to check if that one way is valid.
      for (int i = 0 ; i < sets[set].ways.length; i++){
          if (sets[set].ways[i].valid && sets[set].ways[i].tag == tag   ){
            frame = sets[set].ways[i];
            break;
          }
      }       
      //! If the frame is in the cache and we are using LRU replacement, update all of
      //! the lastAccessTime fields of all ways of this set.
      
      if (frame != null){

        //other frame that have lastAccessTime < this frame's lastAccessTime is increment by 1
        //other frame that have lastAccessTime > this frame's lastAccessTime stays the same
            for ( int i = 0 ; i < sets[set].ways.length; i++){
                if (sets[set].ways[i].valid && sets[set].ways[i].lastAccessTime < frame.lastAccessTime){
                    sets[set].ways[i].lastAccessTime++;
                }
            }
            
        //this frame's lastAccessTime is reset
            frame.lastAccessTime = 0;            
      }
      
      return frame;
    }

    // returns  A newly allocated Frame containing the specified wordAddress.
    // effects  Reads the newly allocated line from lowerLevel.
    //          If necessary, finds and evicts another cache line in the set.
    //          Maintains LRU, round-robin data.
    // requires The specified wordAddress must not already be in the cache
    private Frame allocate(int wordAddress)
    {
      Frame frame = null;
      
      //! Get the set associated with wordAddress
      
      int   set         = setNum(wordAddress);
    
      //! Look for an invalid frame in this set that can be used to hold the new line.
      //! If an invalid frame was found, maintain LRU data.
      
      for (int i = 0 ; i < sets[set].ways.length ; i ++){ 
       if (sets[set].ways[i].valid == false){
         frame =  sets[set].ways[i];
         
          for (int j = 0 ; j < sets[set].ways.length ; j ++){
            if (sets[set].ways[j].valid)  {
                sets[set].ways[j].lastAccessTime++;
            }
          }
          
          frame.lastAccessTime = 0;
         
         break;
       }
      }
     
      //! If no invalid frame was found, we have to evict someone.
      if (frame == null)
      {
        //! First choose the frame to evict
        
        switch (replace)
        {
            case LRU:
              {
                //! Find the least-recently used frame. Update LRU data for all other
                //! frames (all of which will be valid).
               for (int i = 0 ; i < sets[set].ways.length ; i ++){
                    if (sets[set].ways[i].lastAccessTime == sets[set].ways.length-1){
                        frame = sets[set].ways[i];
                        
                      for (int j = 0 ; j < sets[set].ways.length ; j ++){
                        if (sets[set].ways[j].lastAccessTime < frame.lastAccessTime)  {
                            sets[set].ways[j].lastAccessTime++;
                        }
                      }
                      
                        frame.lastAccessTime = 0;
                        
                        break;
                    } 
               }
              }
              break;
              
            case ROUND_ROBIN:
              //! Select the next frame in round robin order. Update roundRobinIndex.
              int rrIndex = sets[set].roundRobinIndex;

              if (sets[set].roundRobinIndex != (sets[set].ways.length-1)){
                sets[set].roundRobinIndex++ ;
              }else{
                sets[set].roundRobinIndex = 0;  
              }
              
              frame =  sets[set].ways[rrIndex];
              break;
              
            case RANDOM:
              //! Select a random frame using Math.random(). 
              Random generator = new Random();
              int r = generator.nextInt(sets[set].ways.length);
              frame = sets[set].ways[r];
              break;          
        }
        
        //! Now write back the frame being evicted.
        frame.writeback(set);
      }
      
      //! We have a new frame, load it from lowerLevel.
      frame.load(wordAddress);
      
      return frame;
    }
    
    // **** Override superclass methods ****
    
    // effects  Flush and invalidate the cache.
    //          Reset the superclass.
    //          Clear statistics.
    //          Reset lowerLevel.
    @Override
    public void reset()
    {
      flush(true);
      super.reset();
      hitCount = 0;
      lowerLevel.reset();
    }
    
    // effect   Implement the read operation.
    @Override
    int readOverride(int wordAddress)
    {
      //! Lookup the wordAddress.
      Frame frame = lookup(wordAddress);    
      
      //! If the word is not in the cache allocate a new frame. Otherwise update
      //! hitCount.
      
      if (frame == null){
        frame = allocate(wordAddress);
      }else{
        hitCount++;
      }
      //! Return the appropriate word from the frame. Look for a helper function above.
      return frame.line[lineOffset(wordAddress)];
    }

    // effect   Implement the write operation. Handle all write policies.
    @Override
    void writeOverride(int wordAddress, int data)
    {
      Frame frame = null;
      
      //! Lookup the word address to see if the word is already in the cache.
      frame = lookup(wordAddress); 
      
      if (frame == null)
      {
        //! Not found. If write allocate policy, allocate a new cache line. If
        //! read allocate policy, write the word directly to lowerLevel and
        //! return. Remember that we have a wordAddress, but writeWord expects
        //! a byte address.
        if (writeAllocate){
            frame = allocate(wordAddress);
        }else{
            lowerLevel.writeWord(wordAddress<<2 , data);
            return;
        }
      }
      else
        ++hitCount;
      
      //! Frame containing the word to be written was either found or allocated. Write
      //! the word to the cache line. If write through policy, also write it to lowerLevel.
      //! If write back policy, make the frame dirty.
      
      
      frame.line[lineOffset(wordAddress)] = data;
      
      //write back
      
      if (writeThru){
         lowerLevel.writeWord(wordAddress<<2 , data);
      }else{
         frame.dirty = true;
      }
    }

    @Override
    public int size()
    {
      return lowerLevel.size();
    }
    
    @Override
    public void writeByte(int address, int data)
    {
      if (!passByteWrites || writeAllocate || lookup(address >> 2) != null)
      {
        // Here we handle a byte write by using the superclass's read/modify/write
        // mechanism. We do this because one of these is true
        //      - we are not passing byte writes through to lower levels (parameter choice)
        //      - we are using a write allocate policy (parameter choice)
        //      - the byte is in the cache
        // In this case there is an extra cache hit count from the write after modify, so
        // subtract it out.
        super.writeByte(address, data);
        --hitCount;
      }
      else
      {
        // Here we pass a byte write to lower level memory because none of the above
        // conditions holds. We do have to update our own write count.
        lowerLevel.writeByte(address, data);
        ++writeCount;
      }
    }
  }

  // ************************************
  // *                                  *
  // *  Memory Array Concrete Subclass  *
  // *                                  *
  // ************************************
  /*
   * A MemoryArray is a subclass of Memory that represents a subset of any other
   * concrete subclass of Memory. By "subset" I mean that the MemoryArray starts at some
   * base address and contains some client-specified number of bytes. A MemoryArray is
   * used by MemoryTest objects to simulate variables and arrays that would be in
   * memory, so that correct operation can be verified and cache performance can be
   * measured. The reason that a MemoryArray is a subclass of Memory is so that we
   * can inherit readByte, writeByte, readWord, and writeWord, and only have to implement
   * word transactions.
   */
  
  // Memory arrays are allocated at this address.
  private int freeAddress = 0;
  
  public void freeMemory()
  {
    freeAddress = 0;
  }
    
  public class MemoryArray extends Memory
  {
    Memory mem;       // The concrete memory object holding the MemoryArray
    int base, bytes;  // Location and size of this MemoryArray.
    
    // effect   Construct a MemoryArray and allocate it at the next free address.
    MemoryArray(int bytes, Memory mem)
    {
      this.bytes = bytes;
      this.mem = mem;
      base = freeAddress;
      freeAddress += (bytes + 3) & ~3;
    }
    
    // effect   Get the byte address corresponding the the specified word
    //          offset in this array.
    // throws   Unchecked exception if wordOffset is not in the array
    // note     An unchecked exception is used so I don't have to add checks to
    //          dozens of other methods throughout the code.
    private int addr(int wordOffset)
    {
      int byteOffset = wordOffset << 2;
      if (byteOffset < 0 || byteOffset >= bytes)
        throw new RuntimeException("Memory array out of bounds");
      return base + byteOffset;
    }
    
    @Override
    protected int readOverride(int wordOffset)
    {
      return mem.readWord(addr(wordOffset));
    }

    @Override
    protected void writeOverride(int wordOffset, int data)
    {
      mem.writeWord(addr(wordOffset), data);
    }

    // Let base memory decide how to handle byte writes
    @Override
    public void writeByte(int address, int data)
    {
      mem.writeByte(address, data);
      ++writeCount;
    }
    
    @Override
    public int size()
    {
      return bytes;
    }
  }
  
  // ********************
  // *                  *
  // *  Memory Testers  *
  // *                  *
  // ********************
  
  public interface MemoryTest
  {
    // returns  True if the test passes
    // effects  Check the specified memory hierarchy for correct operation.
    //          The memory hierarchy will contain performance statistics.
    boolean test(Memory mem);
  }
  
  // The checksum tester writes a random pattern of bytes to the entire memory and
  // then reads it back, verifying a checksum.
  public class ChecksumTest
  {
    public boolean test(Memory wrMem, Memory rdMem)
    {
      int a, checksum = 0;
      for (a = 0; a < wrMem.size(); ++a)
      {
        int d = (int)(Math.random() * 256);
        wrMem.writeByte(a, d);
        checksum += d;
      }
      
      if (rdMem != wrMem && wrMem instanceof Cache)
        ((Cache)wrMem).flush(false);
      
      for (a = 0; a < rdMem.size(); ++a)
        checksum -= rdMem.readByte(a);
      
      return checksum == 0;
    }
  }
  
  public class ChecksumReadTest extends ChecksumTest implements MemoryTest
  {
    @Override
    public boolean test(Memory mem)
    {
      return test(mem instanceof Cache ? ((Cache)mem).lowerLevel : mem, mem);
    }
  }
  
  public class ChecksumWriteTest extends ChecksumTest implements MemoryTest
  {
    @Override
    public boolean test(Memory mem)
    {
      return test(mem, mem instanceof Cache ? ((Cache)mem).lowerLevel : mem);
    }
  }
  
  public class ChecksumFullTest extends ChecksumTest implements MemoryTest
  {
    @Override
    public boolean test(Memory mem)
    {
      return test(mem, mem);
    }
  }
  
  // The blob tester generates an interesting pattern of memory access. It does
  // connectivity (blob) analysis on a square image containing two connected regions, a
  // torus (ring) that spans the image, and a solid circle inside the torus. The stack
  // method for doing connectivity analysis is used, which is what generates the
  // the interesting pattern. The image, stack, and neighborhood table are kept in
  // memory (using MemoryArray objects), and compete for cache space.
  public class BlobTest implements MemoryTest
  {
    // The areas of the two blobs are returned here for printing.
    public int[] area;
    
    @Override
    public boolean test(Memory mem)
    {
      // Allocate the memory arrays
      int imageSize = 320;
      MemoryArray image     = new MemoryArray(imageSize * imageSize, mem);
      MemoryArray neighbors = new MemoryArray(8 * 4                , mem);
      MemoryArray stack     = new MemoryArray(imageSize * imageSize, mem);
      
      // Initialize image
      int x, y;
      int[] area = { 0, 0};
      for (y = 0; y < imageSize; ++y)
        for (x = 0; x < imageSize; ++x)
        {
          double u = x - imageSize / 2.0 + 0.5;
          double v = y - imageSize / 2.0 + 0.5;
          double r = Math.sqrt(u * u + v * v);
          int    z = 0;
          if (r < 0.17 * imageSize)
          {
            // Inner solid circle
            ++area[1];
            z = 1;
          }
          else if (r > 0.33 * imageSize && r < 0.50 * (imageSize - 1))
          {
            // Outer ring
            ++area[0];
            z = 1;
          }
          image.writeByte(y * imageSize + x, z);
        }
      this.area = area.clone();
      
      // Initialize the neighbors table
      neighbors.writeWord( 0,              1);  // E
      neighbors.writeWord( 4,  imageSize + 1);  // NE
      neighbors.writeWord( 8,  imageSize    );  // N
      neighbors.writeWord(12,  imageSize - 1);  // NW
      neighbors.writeWord(16,            - 1);  // W
      neighbors.writeWord(20, -imageSize - 1);  // SW
      neighbors.writeWord(24, -imageSize    );  // S
      neighbors.writeWord(28, -imageSize + 1);  // SE
      
      // Do stack-based connectivity
      int sp, blobIndex = 0;
      for (x = 0; x < imageSize * imageSize; ++x)
        if (image.readByte(x) != 0)
        {
          image.writeByte(x, 0);
          stack.writeWord(0, x);
          sp = 4;
          do
          {
            int nbr;
            y = stack.readWord(sp -= 4);
            --area[blobIndex];
            for (nbr = 0; nbr < 32; nbr += 4)
            {
              int a = y + neighbors.readWord(nbr);
              if (image.readByte(a) != 0)
              {
                image.writeByte(a, 0);
                stack.writeWord(sp, a);
                sp += 4;
              }
            }
          }
          while (sp > 0);
          ++blobIndex;
        }
      
      // Verify that the areas are correct.
      return area[0] == 0 && area[1] == 0;
    }
    
  }
  
  // ******************
  // *                *
  // *  Main Program  *
  // *                *
  // ******************

  public static void main(String[] args)
  {
    // Make a MemoryHierarchy and a main memory.
    MemoryHierarchy mh     =    new MemoryHierarchy();
    MainMemory      dram   = mh.new MainMemory(0x100000);
    
    // Make a table of replacement policies and names for printing
    ReplacementPolicy[] policies =
      {
        ReplacementPolicy.LRU, ReplacementPolicy.ROUND_ROBIN, ReplacementPolicy.RANDOM
      };
    String[] policyNames = {"LRU", "Round", "Random"};
    
    // Make a table of associativity names for printing
    String[] assocNames  = {"DirectMap", "SetAssoc", "FullAssoc"};
    
    // Make memory tester
    //MemoryTest mt = mh.new ChecksumFullTest();
    MemoryTest mt = mh.new BlobTest();
    
    // Run test out of main memory
    boolean ok = mt.test(dram);
    if (mt instanceof BlobTest)
    {
      BlobTest bt = (BlobTest)mt;
      System.out.printf("Blob areas = %d, %d\n\n", bt.area[0], bt.area[1]);
    }
    System.out.print("                             Reads    Writes   %Miss\n");
    System.out.printf("          Main Memory %s %9d %9d\n",
                      ok ? "OK" : "ER", dram.readCount, dram.writeCount);
    
    // Establish total cache size, and line size that we'll use.
    int logCacheBytes = 12, logLineBytes = 5, logSets, logWays = 0;
    
    // Variables that are used to loop over all associativity and policy choices
    int assoc = 0, allocPolicy = 0, thruPolicy = 0, replacePolicy = 0;
    
    // Loop over all associativity choices
    for (assoc = 0; assoc < 3; ++assoc)
    {
      switch (assoc)
      {
      case 0:
        logWays = 0;  // Direct map has 1 way
        break;
      case 1:
        logWays = 1;  // Set associative has 2 ways
        break;
      case 2:
        logWays = logCacheBytes - logLineBytes; // Fully associative
        break;
      }
      
      // Now that we know the number of ways we can calculate the number of sets
      // and create a level 1 cache.
      logSets = logCacheBytes - logLineBytes - logWays;
      Cache L1 = mh.new Cache(logSets, logLineBytes, 1 << logWays, dram);
      
      // Get cache parameters into a printable string
      System.out.printf("\n%9s: %d bytes, %d sets, %d ways, %d line\n",
                        assocNames[assoc],
                        1 << logCacheBytes, 1 << logSets, 1 << logWays, 1 << logLineBytes);
      
      // Loop over all allocation policies
      for (allocPolicy = 0; allocPolicy < 2; ++allocPolicy)
      {
        L1.setWriteAllocate(allocPolicy != 0);
        
        // Loop over all replacement policies. If we're testing a direct mapped cache, no need to try
        // different replacement methods since the one way in each set is evicted regardless.
        for (replacePolicy = 0; replacePolicy < 1 || assoc != 0 && replacePolicy < 3; ++replacePolicy)
        {
          L1.setReplace(policies[replacePolicy]);
        
          // Loop over write back or write through
          // Actually, only do write back because write through is not interesting
          for (thruPolicy = 0; thruPolicy < 2; ++thruPolicy)
          {
            L1.setWriteThru(thruPolicy != 0);
            
            // Run the test
            L1.reset();
            mh.freeMemory();
            ok = mt.test(L1);
            L1.flush(true);
            
            // Now print test result and cache performance.
            System.out.printf("%6s %s %s %s %9d %9d  %6.2f\n",
                              policyNames[replacePolicy],
                              L1.getWriteAllocate() ? "WrAlloc" : "RdAlloc",
                              L1.getWriteThru() ? "WrThru" : "WrBack", ok ? "OK" : "ER",
                              dram.readCount, dram.writeCount, L1.missRate() * 100);
          }        
        }
      }
    }
  }

}