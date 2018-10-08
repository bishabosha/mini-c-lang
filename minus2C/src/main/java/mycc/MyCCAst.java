package mycc;

public class MyCCAst {

    public interface Ast {
        int tag();
        int type();
    }

    public interface Node {
        Ast ast();
    }

}
