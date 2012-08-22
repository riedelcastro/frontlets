package org.riedelcastro.frontlets.programs;

import scala.collection.Seq;
import scala.collection.immutable.Seq$;
import scala.collection.mutable.ArrayBuffer;

/**
 * @author Sebastian Riedel
 */
public class Test implements Executable{

    public final ProgramInfo info;

    public Test(ProgramInfo info) {
        this.info = info;
    }

    @Override
    @SuppressWarnings({"unchecked"})
    public State execute(State input) {
        int a = 0;
        double b = 1.0;
        Integer A = a;
        int k = A.intValue();
        scala.collection.mutable.Map<Var<Object>,Object> map = new scala.collection.mutable.HashMap<Var<Object>,Object>();
        Var<Object> var = new SimpleVar<Object>("x",1);
        map.update(var, 1);
        ArrayBuffer<Term<Object>> terms = new ArrayBuffer<Term<Object>>();
        terms.$plus$eq(var);
        terms.$plus$eq(var);
        ArrayBuffer<Object> values = new ArrayBuffer<Object>();

        Eval eval = Eval$.MODULE$.apply(terms,values);
        State state = State$.MODULE$.apply(map);
        var.eval(state,eval).get();
        return state;
    }

    public static void main(String[] args) {
        Test test = new Test(null);
        System.out.println(test.execute(null));
    }
}
