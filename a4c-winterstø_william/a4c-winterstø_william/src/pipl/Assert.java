package pipl;

public class Assert extends Stmt {
	
	Expr value;
	
	public Assert(Expr val) {
		value = val;
	}

	@Override
	public State exec(State state) {
		
		Value val = value.eval(state);
		
		if (val.getClass().equals(B.class)) {
			if (((B)val).v0) {
				return state;
			}
		}
		
		return null;
	}

}
