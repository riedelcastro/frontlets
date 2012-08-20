package org.riedelcastro.frontlets.programs;

/**
 * @author Sebastian Riedel
 */
public class Test implements Executable{
    @Override
    public State execute(State input) {
        scala.collection.mutable.Map<Var<Object>,Object> map = new scala.collection.mutable.HashMap<Var<Object>,Object>();
        return State$.MODULE$.apply(map);
    }

    public static void main(String[] args) {
        Test test = new Test();
        System.out.println(test.execute(null));
    }
}
