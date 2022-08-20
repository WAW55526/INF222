package pipl;

public class Plus extends Expr {
	
	Expr value1;
	Expr value2;
	
	public Plus(Expr val1, Expr val2) {
		value1 = val1;
		value2 = val2;
	}

	@Override
	public Value eval(State state) {
		Value val1 = value1.eval(state);
		Value val2 = value2.eval(state);
		
		if (val1.getClass().equals(I.class) && val2.getClass().equals(I.class)) {
			return new I(((I)val1).v0 + ((I)val2).v0);
		}
		
		return null;
	}

}
