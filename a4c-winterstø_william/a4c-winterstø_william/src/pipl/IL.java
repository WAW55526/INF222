package pipl;
public class IL extends Expr {
	
	int value;
	
	public IL(int val) {
		value = val;
	}

	@Override
	public Value eval(State state) {
		return new I(value);
	}

}
