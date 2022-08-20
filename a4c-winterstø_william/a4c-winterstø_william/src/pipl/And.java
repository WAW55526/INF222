package pipl;

public class And extends Expr {
	
	Expr value1;
	Expr value2;
	
	public And(Expr val1, Expr val2) {
		value1 = val1;
		value2 = val2;
	}

	@Override
	public Value eval(State state) {
		
		Value val1 = value1.eval(state);
		Value val2 = value2.eval(state);
		
		if (val1.getClass().equals(B.class) && val2.getClass().equals(B.class)) {
			return new B(((B)val1).v0 && ((B)val2).v0);
		}
		
		return null;
	}

}
