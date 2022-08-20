package pipl;

public class Not extends Expr {
	
	Expr value;
	
	public Not(Expr val) {
		value = val;
	}

	@Override
	public Value eval(State state) {
		
		Value val = value.eval(state);
		
		if (val.getClass().equals(B.class)) {
			return new B(!((B)val).v0);
		}
		
		return null;
	}

}
