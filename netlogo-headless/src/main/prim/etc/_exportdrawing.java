// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim.etc;

import org.nlogo.nvm.RuntimePrimitiveException;

public final class _exportdrawing
    extends org.nlogo.nvm.Command {
  @Override
  public void perform(final org.nlogo.nvm.Context context) {
    final String filePath = argEvalString(context, 0);
    workspace.waitFor
        (new org.nlogo.api.CommandRunnable() {
          public void run() {
            try {
              workspace.exportDrawing
                  (workspace.fileManager().attachPrefix
                      (filePath),
                      "png");
            } catch (java.io.IOException ex) {
              throw new RuntimePrimitiveException
                  (context, _exportdrawing.this,
                      token().text() +
                          ": " + ex.getMessage());
            }
          }
        });
    context.ip = next;
  }

}
