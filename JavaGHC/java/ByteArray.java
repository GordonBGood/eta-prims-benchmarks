package eta.runtime.io;

import java.util.Arrays;
import java.nio.ByteOrder;
import java.nio.ByteBuffer;

import eta.runtime.Runtime;
import eta.runtime.stg.Value;

public final class ByteArray extends Value { // I don't think the Value extension is ever used...

    public static ByteArray create(int n) {
        return create(n, 0, false);
    }

    public static ByteArray create(int n, boolean pinned) {
        return create(n, 0, pinned);
    }

    public static ByteArray create(int n, int alignment, boolean pinned) {
// Should actually return a ByteBuffer as for the Unpinned case, where it is a normal appearing slice...
        // force all conventionally allocated to pinned for speed!
        // TODO:  add alignment functionality or are all addresses already aligned NO - only for Java >- 9?
        long address = MemoryManager.allocateBuffer(n, true); // pinned! // allocate n + 8...
        ByteArray bytearr = new ByteArray(n, address);
        if (n > 0 && address > 0) IO.recordByteArray(bytearr);
        return bytearr;
//        ByteBuffer bytebfr = MemoryManager.getBoundedBuffer(address)
//                                            .order(ByteOrder.nativeOrder());
//        bytebfr.putLong(address + n, address); // address to extra 8 bytes
        // modify to return ByteBuffer sized to requested size; address above
//        return ((ByteBuffer)bytebfr.limit(bytebfr.position() + n)).slice();
    }

    public static ByteBuffer createUnpinned(int n) { // would actually like to return a ByteBuffer!
        // the bypassed creations produce ByteBuffer directly instead of ByteArray!
        // force all conventionally allocated to pinned for speed (ignore setting)!
        int size = n + 8;
        ByteBuffer bytebfr;
        /*
        // take care of JVM <= 1.8 being slow if not directly allocated...
        if (java.lang.System.getProperty("java.version").startsWith("1."))
            bytebfr = ByteBuffer.allocateDirect(size);
        else bytebfr = ByteBuffer.allocate(size);
        int alignment = 0;
        for (int m = 1; m <= 8; m <<= 1) if (m >= alignment) alignment = m;
        if (alignment > 8) alignment = 8;
        int xtra = 8 + ((alignment - 1) << 1); // allocate n + 8 for address storage...
        long address = MemoryManager.allocateBuffer(n + xtra, true); // pinned!
        long algnaddr = address & (alignment - 1); // if addresses are always aligned, this is unnecessary
        */
        long address = MemoryManager.allocateBuffer(size, true); // pinned!
        ByteArray bytearr = new ByteArray(n, address);
        if (n > 0 && address > 0) IO.recordByteArray(bytearr);
        bytebfr = MemoryManager.getBoundedBuffer(address);
        System.out.println(address); // verify that aligned addresses or not
        System.out.println(n); // verify that aligned addresses or not
        System.out.println(bytebfr.limit()); // verify that aligned addresses or not
        bytebfr = bytebfr.order(ByteOrder.nativeOrder());
        bytebfr.putLong(bytebfr.position() + n, address); // address to extra 8 bytes
        // modify to return ByteBuffer sized to requested size; address above
        return ((ByteBuffer)bytebfr.limit(bytebfr.position() + n)).slice()
                                            .order(ByteOrder.nativeOrder());
    }

    public static long getAddress(ByteBuffer bytebuf) {
        int lastndx = bytebuf.limit();
        ByteBuffer bytebfr = bytebuf.duplicate().order(ByteOrder.nativeOrder());
        bytebfr.limit(lastndx + 8);
        return bytebfr.getLong(lastndx);
    }

    public int  size;
    public long bufferAddress;

    private ByteArray(int size, long address) {
        this.size = size;
        this.bufferAddress = address;
    }
}
