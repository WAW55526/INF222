package pipl;

public class Choice extends Expr {
	
	Expr value1;
	Expr value2;
	Expr value3;
	
	public Choice(Expr val1, Expr val2, Expr val3) {
		value1 = val1;
		value2 = val2;
		value3 = val3;
	}

	@Override
	public Value eval(State state) {
		
		Value val1 = value1.eval(state);
		Value val2 = value2.eval(state);
		Value val3 = value3.eval(state);
		
		if (val1.getClass().equals(B.class)) {
			
			if (((B)val1).v0) {
				return val2;
			}
			return val3;
		}
		
		return null;
	}

}
