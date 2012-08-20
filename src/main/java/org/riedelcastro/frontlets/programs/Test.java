package org.riedelcastro.frontlets.programs;

/**
 * @author Sebastian Riedel
 */
public class Test implements Executable{

    public final ProgramInfo info;

    public Test(ProgramInfo info) {
        this.info = info;
    }

    @Override
    public State execute(State input) {
        int a = 0;
        double b = 1.0;
        scala.collection.mutable.Map<Var<Object>,Object> map = new scala.collection.mutable.HashMap<Var<Object>,Object>();
        Var<Object> var = info.boundVariables()[0];
        map.update(var, 1);
        return State$.MODULE$.apply(map);
    }

    public static void main(String[] args) {
        Test test = new Test(null);
        System.out.println(test.execute(null));
    }
}
