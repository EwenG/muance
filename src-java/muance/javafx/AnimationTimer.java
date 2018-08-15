package muance.javafx;

import clojure.lang.IFn;

public class AnimationTimer extends javafx.animation.AnimationTimer {
  private IFn handle;
  private FrozenRenderQueue frozenRenderQueue;

  public AnimationTimer(IFn handle, FrozenRenderQueue frozenRenderQueue) {
    this.handle = handle;
    this.frozenRenderQueue = frozenRenderQueue;
  }

  public FrozenRenderQueue getFrozenRenderQueue() {
    return frozenRenderQueue;
  }

  @Override
  public void handle(long l) {
    handle.invoke(this, l);
  }
}
