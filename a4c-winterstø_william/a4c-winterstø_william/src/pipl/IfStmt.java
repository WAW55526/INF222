package pipl;

public class IfStmt extends Stmt {
	
	Expr value1;
	Stmt value2;
	Stmt value3;
	
	public IfStmt(Expr val1, Stmt val2, Stmt val3) {
		value1 = val1;
		value2 = val2;
		value3 = val3;
	}

	@Override
	public State exec(State state) {
		
		Value val1 = value1.eval(state);
		
		if (val1.getClass().equals(B.class)) {
			if (((B)val1).v0) {
				state = value2.exec(state);
			}
			else {
				state = value3.exec(state);
			}
			
			return state;
		}
		
		return null;
	}

}
