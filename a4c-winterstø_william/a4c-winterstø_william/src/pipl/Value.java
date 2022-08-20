package pipl;

import java.util.Objects;

/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
////////////////////// VALUE ////////////////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////

/**
 * The value domain for our object language. This is implemented using a
 * functional/value oriented approach, i.e., a value object is never modified.
 * 
 * @author Jørn Lode & Magne Haveraaen
 * @since 2022-02-16
 */
interface Value extends Cloneable {
    /** Wrapper for <code>toString()</code> */
    String show();
}

/** The unused value. */
class U implements Value {

    U() {
    };

    @Override
    public String show() {
	return toString();
    }

    /** Generated method. Corresponds to Haskell's <code>deriving Show</code>. */
    @Override
    public String toString() {
	return "U []";
    }

    /** Hand written method. */
    @Override
    public int hashCode() {
	return 42;
    }

    /** Hand written method. Corresponds to Haskell's <code>deriving Eq</code>. */
    @Override
    public boolean equals(Object obj) {
	if (this == obj)
	    return true;
	if (obj == null)
	    return false;
	if (getClass() != obj.getClass())
	    return false;
	return true;
    }

}

/** Java short corresponds to Haskell Int16. */
class I implements Value {
    Short v0;

    I(int i0) {
	this.v0 = (short) i0;
    }

    @Override
    public String show() {
	return v0.toString();
    }

    /** Generated method. Corresponds to Haskell's <code>deriving Show</code>. */
    @Override
    public String toString() {
	return "I [v0=" + v0 + "]";
    }

    /** Generated method. */
    @Override
    public int hashCode() {
	return Objects.hash(v0);
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
	I other = (I) obj;
	return Objects.equals(v0, other.v0);
    }
}

/** Java boolean corresponds to Haskell Bool. */
class B implements Value {
    Boolean v0;

    B(Boolean i0) {
	this.v0 = i0;
    }

    @Override
    public String show() {
	return v0.toString();
    }

    /** Generated method. Corresponds to Haskell's <code>deriving Show</code>. */
    @Override
    public String toString() {
	return "B [v0=" + v0 + "]";
    }

    /** Generated method. */
    @Override
    public int hashCode() {
	return Objects.hash(v0);
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
	B other = (B) obj;
	return Objects.equals(v0, other.v0);
    }

}

/** Locations: Java byte corresponds to Haskell Word8/Int8. */
class L implements Value {
    Byte location;

    public L(Byte location) {
	super();
	this.location = location;
    }

    @Override
    public String show() {
	return this.toString();
    }

    /** Generated method. Corresponds to Haskell's <code>deriving Show</code>. */
    @Override
    public String toString() {
	return "L [location=" + location + "]";
    }

    /** Generated method. */
    @Override
    public int hashCode() {
	return Objects.hash(location);
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
	L other = (L) obj;
	return Objects.equals(location, other.location);
    }

}
