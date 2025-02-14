// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim.etc;

import org.nlogo.agent.Agent;
import org.nlogo.agent.AgentIterator;
import org.nlogo.agent.AgentSet;
import org.nlogo.api.LogoException;
import org.nlogo.core.Syntax;
import org.nlogo.nvm.Context;
import org.nlogo.nvm.Reporter;

import java.util.ArrayList;
import java.util.List;

public final class _withmin
    extends Reporter {


  @Override
  public Object report(final Context context) throws LogoException {
    AgentSet sourceSet = argEvalAgentSet(context, 0);
    double winningValue = Double.MAX_VALUE;
    Context freshContext = new Context(context, sourceSet);
    List<Agent> result = new ArrayList<Agent>();
    args[1].checkAgentSetClass(sourceSet, context);
    for (AgentIterator iter = sourceSet.iterator(); iter.hasNext();) {
      Agent tester = iter.next();
      Object value = freshContext.evaluateReporter(tester, args[1]);
      if (!(value instanceof Double)) {
        continue;
      }
      double dvalue = ((Double) value).doubleValue();
      if (dvalue <= winningValue) {
        if (dvalue < winningValue) {
          winningValue = dvalue;
          result.clear();
        }
        result.add(tester);
      }
    }
    return AgentSet.fromArray(sourceSet.kind(), result.toArray(new Agent[result.size()]));
  }
}
