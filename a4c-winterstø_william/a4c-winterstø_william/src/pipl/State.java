package pipl;

import java.util.Map;
import java.util.Objects;
import java.util.Arrays;
import java.util.HashMap;

//import junit.framework.TestCase;
// import static org.junit.Assert.*;

/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
///////////////////// STATE /////////////////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////

/**
 * A state is an environment of variable-store index associations, and a store
 * which at each store index keeps a value (for that variable).
 * 
 * NB! Type location is represented as a Java byte.
 * 
 * @see BIPL3State.hs
 * 
 * @author Jørn Lode & Magne Haveraaen
 * @since 2022-02-16
 *
 */
public class State implements Copyable<State> {
    /** The next two lines correspond to type State = (Environment,Store) */
    private Environment environment;
    private Store store;

    /** The copy constructor. */
    State(Environment environment, Store store) {
	this.environment = environment;
	this.store = store;
    }

    /** The default constructor. */
    State() {
	this.environment = Environment.emptyEnvironment();
	this.store = Store.emptyStore();
    }

    /**
     * A new state is an empty environment with an empty store. Corresponds to
     * <code>newState :: State</code>
     */
    public static State newState() {
	return new State();
    }

    /**
     * Checks if a name is registered as a variable in the environment
     * <code>isVariable :: String -> State -> Bool</code>
     */
    public boolean isVariable(String vname) {
	return this.environment.isVariable(vname);
    }

    /**
     * Gets the value linked to the variable in the state. Corresponds to
     * <code>getValue :: String -> State -> Value</code>
     */
    Value getValue(String name) {
	return this.store.getStoreValue(this.environment.getVariableLocation(name));
    }

    /**
     * Add a new variable with value to the state.
     * <code>addVariable :: String -> Value -> State -> State</code>
     */
    void addVariable(String name, Value value) {
	byte location = this.store.availableLocation();
	this.environment.addVariable(name, location);
	this.store.enlargeStore(value);
    }

    /**
     * Creates a new variable name as an alias for an existing variable location.
     * <code>createAlias :: String -> String -> State -> State</code>
     */
    public void createAlias(String aliasvar, String vname) {
	this.environment.addVariable(aliasvar, this.environment.getVariableLocation(vname));
    }

    /**
     * Changes the value associated with a known variable.
     * <code>changeValue :: String -> Value -> State -> State</code>
     */
    public void changeValue(String vname, Value value) {
	this.store.setStoreValue(this.environment.getVariableLocation(vname), value);
    }

    /**
     * Get current free location and environment
     * <code>getFreeLocation :: State -> (Location,Environment)</code>
     */
    public LocationEnvironment getFreeLocation() {
	return new LocationEnvironment(this.store.availableLocation(), this.environment.copy());
    }

    /**
     * Reset free location and related environment
     * <code>resetFreeLocation :: (Location,Environment) -> State -> State</code>
     */
    public void resetFreeLocation(LocationEnvironment locenv) {
	this.environment = locenv.getEnvironment();
	this.store.setFreeStoreLocation(locenv.getLocation());
    }

    /** Generated method. Corresponds to Haskell's <code>deriving Show</code>. */
    @Override
    public String toString() {
	return "State [environment=" + environment + ", store=" + store + "]";
    }

    /** Generated method. */
    @Override
    public int hashCode() {
	return Objects.hash(environment, store);
    }

    /** Generated method. Corresponds to Haskell's <code>deriving Eq</code>. */
    @Override
    public boolean equals(Object obj) {
	if (this == obj)
	    return true;
	if (obj == null)
	    return false;
	if (getClass() != obj.getClass())
	    return false;
	State other = (State) obj;
	return Objects.equals(environment, other.environment) && Objects.equals(store, other.store);
    }

    @Override
    public State copy() {
	return new State(this.environment.copy(), this.store.copy());
    }

    /** Unit test for state and values. */
    public static void main(String args[]) throws Exception {
	System.out.println("main unittestState");
//	System.out.println(newState());
	State state = newState();
	state.addVariable("v1", new I(1));
	state.addVariable("v2", new L((byte) 4));
	state.addVariable("v3", new I(9));
	LocationEnvironment locenv = state.getFreeLocation();
	State state3 = state.copy();
//	System.out.println("State3     =" + state3);
	state.changeValue("v2", new I(25));
	state.addVariable("v4", new I(19));
	state.addVariable("v5", new I(93));
	state.createAlias("v6", "v1");
	state.changeValue("v6", new I(42));
	State state3prime = state.copy();
	state3prime.resetFreeLocation(locenv);
//	System.out.println("State3     =" + state3);
//	System.out.println("State3prime=" + state3prime);
//	System.out.println("State8     =" + state);
	// Using JUnit assertions
	//assertEquals(new I(42), state.getValue("v1"));
	//assertEquals(new I(25), state.getValue("v2"));
	//assertEquals(new I(9), state.getValue("v3"));
	//assertEquals(new I(19), state.getValue("v4"));
	//assertEquals(new I(93), state.getValue("v5"));
	//assertEquals(new I(42), state.getValue("v6"));
	//assertFalse(state3.isVariable("v6"));
	//assertFalse(state3prime.isVariable("v6"));
	//assertTrue(state.isVariable("v6"));
	// Using a conditional as in the Haskell version
	if (new I(42).equals(state.getValue("v1")) && new I(25).equals(state.getValue("v2"))
		&& new I(9).equals(state.getValue("v3")) && new I(19).equals(state.getValue("v4"))
		&& new I(93).equals(state.getValue("v5")) && new I(42).equals(state.getValue("v6")))
	    System.out.println("Unit tests hold");
	else
	    System.out.println("Tests failed");
    }

}

/** Corresponding to <code>(Location,Environment)</code> */
class LocationEnvironment {
    private byte location;
    private Environment environment;

    public LocationEnvironment(byte location, Environment environment) {
	super();
	this.location = location;
	this.environment = environment;
    }

    public byte getLocation() {
	return location;
    }

    public Environment getEnvironment() {
	return environment;
    }

}

/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
/////// Environment API and implementation //////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////

/**
 * An Environment for variables. It stores an association of distinct variable
 * names and their store index. Corresponds to
 * <code>type Environment = [(String,Location)]</code>
 */
interface Environment extends Copyable<Environment> {

    /**
     * Defines an empty environment. Corresponds to
     * <code>emptyEnvironment :: Environment</code>
     * 
     * NB! This factory assumes we want to use <code>EnvironmentMap</code> as our
     * data structure.
     */
    public static Environment emptyEnvironment() {
	return new EnvironmentMap();
    }

    /**
     * Add a new variable (and a store index) to the environment.
     * <code>addVariableToEnvironment :: String -> Location -> Environment -> Environment</code>
     */
    public void addVariable(String name, byte location);

    /**
     * Gets the location of a stored variable.
     * <code>getVariableLocation :: String -> Environment -> Location</code>
     */
    public byte getVariableLocation(String name);

    /**
     * Checks if a variable is defined. Corresponds to
     * <code>lookup name environment /= Nothing</code>
     */
    public boolean isVariable(String name);

    /**
     * Finds the location associated to the variable name. Returns null if the
     * variable name is not found. Corresponds to
     * <code>lookup :: String -> Environment -> Maybe Location</code>
     */
    public Byte lookup(String name);

}

/** Implementation of an environment using a HashMap data structure. */
class EnvironmentMap implements Environment {
    Map<String, Byte> map = null;

    EnvironmentMap() {
	super();
	map = new HashMap<String, Byte>();
    }

    private EnvironmentMap(Map<String, Byte> map) {
	super();
	this.map = map;
    }

    @Override
    public void addVariable(String name, byte location) {
	this.map.put(name, location);
    }

    @Override
    public byte getVariableLocation(String name) {
	return this.map.get(name);
    }

    @Override
    public boolean isVariable(String name) {
	return this.map.containsKey(name);
    }

    @Override
    public Byte lookup(String name) {
	return this.map.get(name);
    }

    /** Generated method. Corresponds to Haskell's <code>deriving Show</code>. */
    @Override
    public String toString() {
	return "EnvironmentMap [map=" + map + "]";
    }

    /** Generated method. */
    @Override
    public int hashCode() {
	return Objects.hash(map);
    }

    /** Generated method. Corresponds to Haskell's <code>deriving Eq</code>. */
    @Override
    public boolean equals(Object obj) {
	if (this == obj)
	    return true;
	if (obj == null)
	    return false;
	if (getClass() != obj.getClass())
	    return false;
	EnvironmentMap other = (EnvironmentMap) obj;
	return Objects.equals(map, other.map);
    }

    @SuppressWarnings("unchecked")
    @Override
    public Environment copy() {
	return new EnvironmentMap((Map<String, Byte>) ((HashMap<String, Byte>) this.map).clone());
    }

}

/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
////////// Store API and implementation /////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////

/**
 * A Store is mainly an array of locations, defined by the pair:
 * <ul>
 * <li>a location index for the next available location in the store</li>
 * <li>an array indexed by location of values.</li>
 * </ul>
 * All location values are initialised to U (unknown). Store locations are
 * occupied from the maximum downwards to the minimum array index.
 * <code>type Store = (Location,Array Location Value)</code>
 */
interface Store extends Copyable<Store> {

    /**
     * Creates an empty store, its index range is defined by the Location type
     * <code>emptyStore :: Store</code>
     * 
     * NB! This factory assumes we want to use <code>TestStore</code> as our data
     * structure.
     */
    public static Store emptyStore() {
	return new StoreArray();
    }

    /**
     * Get the value stored for the given index.
     * <code>getStoreValue :: Location -> Store -> Value</code>
     */
    public Value getStoreValue(byte location);

    /**
     * Set a new value at the provided location.
     * <code>setStoreValue :: Location -> Value -> Store -> Store</code>
     */
    public void setStoreValue(byte location, Value value);

    /**
     * Allocate value at next available location and move free location pointer.
     * Includes a safety check so we do not wrap around when allocating new memory.
     * <code>enlargeStore :: Value -> Store -> Store</code>
     */
    public void enlargeStore(Value value);

    /**
     * Location in store available for allocation
     * <code>availableLocation :: Store -> Location</code>
     */
    byte availableLocation();

    /**
     * Set free location in store, must be higher than current free location
     * <code>setFreeStoreLocation :: Location -> Store -> Store</code>
     */
    void setFreeStoreLocation(byte location);
}

/** Implementation of a store using an array of values. */
class StoreArray implements Store {
    /** Number of elements in a Java byte. */
    final private static int storesize = 256;

    /**
     * Computes the index address from the location. Note that a Java byte is a
     * signed 8bit integer (Haskell Int8) and not an unsigned 8bit integer (Haskell
     * Word8). The address computation thus needs to take a possibly negative number
     * to a number in the corresponding positive range. Also note that Java arrays
     * are indexed by Java int (Haskell Int32), so an integer type promotion is
     * needed.
     */
    final private static int getAddress(byte location) {
	return (storesize + location) % storesize;
    }

    private byte nextLocation = 0;

    private Value store[];

    StoreArray() {
	this.nextLocation = (byte) storesize - 1;
	this.store = new Value[storesize];
    }

    private StoreArray(byte nextLocation, Value[] store) {
	super();
	this.nextLocation = nextLocation;
	this.store = store;
    }

    @Override
    public Value getStoreValue(byte location) {
	return this.store[getAddress(location)];
    }

    @Override
    public void setStoreValue(byte location, Value value) {
	this.store[getAddress(location)] = value;
    }

    @Override
    public void enlargeStore(Value value) {
	this.store[getAddress(this.nextLocation)] = value;
	this.nextLocation--;
    }

    @Override
    public byte availableLocation() {
	return this.nextLocation;
    }

    @Override
    public void setFreeStoreLocation(byte location) {
	this.nextLocation = location;
    }

    /** Generated method. Corresponds to Haskell's <code>deriving Show</code>. */
    @Override
    public String toString() {
	return "StoreArray [nextLocation=" + nextLocation + ", store=" + Arrays.toString(store) + "]";
    }

    /** Generated method. */
    @Override
    public int hashCode() {
	final int prime = 31;
	int result = 1;
	result = prime * result + Arrays.hashCode(store);
	result = prime * result + Objects.hash(nextLocation);
	return result;
    }

    /** Generated method. Corresponds to Haskell's <code>deriving Eq</code>. */
    @Override
    public boolean equals(Object obj) {
	if (this == obj)
	    return true;
	if (obj == null)
	    return false;
	if (getClass() != obj.getClass())
	    return false;
	StoreArray other = (StoreArray) obj;
	return nextLocation == other.nextLocation && Arrays.equals(store, other.store);
    }

    @Override
    public Store copy() {
	return new StoreArray(this.nextLocation, Arrays.copyOf(this.store, storesize));
    }

}

/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
////////////////// Deep copyable ////////////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////

/**
 * Interface for deep copying - as opposed to the built in clone which is
 * shallow cloning.
 */
interface Copyable<T> {
    /** Returns a deep copy of current object. */
    T copy();
}
