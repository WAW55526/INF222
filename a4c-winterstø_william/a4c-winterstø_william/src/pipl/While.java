package pipl;

public class While extends Stmt {
	
	Expr value1;
	Stmt value2;
	
	public While(Expr val1, Stmt val2) {
		value1 = val1;
		value2 = val2;
	}

	@Override
	public State exec(State state) {
		
		Value val1 = value1.eval(state);
		
		if (val1.getClass().equals(B.class)) {
			while (((B)val1).v0) {
				state = value2.exec(state);
				val1 = value1.eval(state);
			}
			return state
		}
		
		return null;
	}

}
