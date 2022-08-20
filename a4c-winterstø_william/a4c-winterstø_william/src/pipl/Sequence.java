package pipl;

import java.util.List;

public class Sequence extends Stmt {
	
	List<Stmt> value;
	
	public Sequence(List<Stmt> val) {
		value = val;
	}

	@Override
	public State exec(State state) {
		
		for (Stmt stmt : value) {
			stmt.exec(state);
		}
		
		return state;
	}

}
