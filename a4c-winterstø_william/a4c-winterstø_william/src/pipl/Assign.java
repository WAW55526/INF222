package pipl;

public class Assign extends Stmt {
	
	String value1;
	Expr value2;
	
	public Assign(String val1, Expr val2) {
		value1 = val1;
		value2 = val2;
	}

	@Override
	public State exec(State state) {
		
		Value val2 = value2.eval(state);
		
		if (state.isVariable(value1)) {
			state.changeValue(value1, val2);
		}
		else {
			state.addVariable(value1, val2);
		}
		
		return state;
	}

}
