package pipl;

public class Uminus extends Expr {
	
	Expr value;
	
	public Uminus(Expr val) {
		value = val;
	}

	@Override
	public Value eval(State state) {
		
		Value val = value.eval(state);
		
		if (val.getClass().equals(I.class)) {
			return new I(-((I)val).v0);
		}
		
		return null;
	}

}
