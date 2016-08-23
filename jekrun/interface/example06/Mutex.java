package example06;

/**
 * <p>Java code for the object of the mutex example.</p>
 * <p>This class provides a mutex object.</p>
 * <p/>
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 * <p/>
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 * <p/>
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class Mutex {
    private boolean locked;

    /**
     * <p>Create a new mutex.</p>
     */
    public Mutex() {
    }

    /**
     * <p>Acquire the lock.</p>
     * <p>Acquiring a lock is not reentrant, the current implementation
     * is simple, we do not keep a counter, only a flag. So when the
     * same process tries to acquire the same lock again, it will block
     * and this will get unnoticed.</p>
     * <p>Acquiring a lock is cancellable by calling the method
     * interrupt() on the blocking thread. The cancellation will leave
     * the lock properly in locked state.</p>
     *
     * @throws InterruptedException If the request was cancelled.
     */
    public void acquire() throws InterruptedException {
        synchronized (this) {
            while (locked)
                this.wait();
            locked = true;
        }
    }

    /**
     * <p>Release the lock.</p>
     * <p>Releasing does not check the owner, the current implementation
     * is simple, we do not keep the owner. So a process can acquire
     * the lock and a different process can release the lock.</p>
     *
     * @throws IllegalStateException If the lock was not yet acquired.
     */
    public void release() {
        synchronized (this) {
            if (!locked)
                throw new IllegalStateException();
            locked = false;
            this.notifyAll();
        }
    }

}
