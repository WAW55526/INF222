package pipl;

public class Equal extends Expr {
	
	Expr value1;
	Expr value2;
	
	public Equal(Expr val1, Expr val2) {
		value1 = val1;
		value2 = val2;
	}

	@Override
	public Value eval(State state) {
		
		Value val1 = value1.eval(state);
		Value val2 = value2.eval(state);
		
		if (val1.getClass().equals(I.class) && val2.getClass().equals(I.class)) {
			return new B(val1.equals(val2));
		}
		
		return null;
	}

}
