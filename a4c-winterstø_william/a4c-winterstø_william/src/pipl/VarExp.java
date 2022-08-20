package pipl;

public class VarExp extends Expr {
	
	String value;
	
	public VarExp(String val) {
		value = val;
	}

	@Override
	public Value eval(State state) {
		
		return state.getValue(value);
	}

}
