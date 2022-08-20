package pipl;

public class BL extends Expr {
	
	boolean value;
	
	public BL(boolean val) {
		value = val;
	}

	@Override
	public Value eval(State state) {
		
		return new B(value);
	}

}
