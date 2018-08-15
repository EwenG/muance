package muance.javafx;

import java.util.concurrent.SynchronousQueue;

public class FrozenRenderQueue {
  private Object originRendeQueue;
  private SynchronousQueue renderQueueIn;
  private SynchronousQueue renderQueueOut;

  public FrozenRenderQueue(Object originRendeQueue, SynchronousQueue renderQueueIn, SynchronousQueue renderQueueOut) {
    this.originRendeQueue = originRendeQueue;
    this.renderQueueIn = renderQueueIn;
    this.renderQueueOut = renderQueueOut;
  }

  public Object getOriginRendeQueue() {
    return originRendeQueue;
  }

  public SynchronousQueue getRenderQueueIn() {
    return renderQueueIn;
  }

  public SynchronousQueue getRenderQueueOut() {
    return renderQueueOut;
  }
}
