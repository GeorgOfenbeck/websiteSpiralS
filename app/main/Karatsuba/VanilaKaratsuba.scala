package Karatsuba

import java.math.BigInteger


case class MyBigInt(val mag: Array[Int], val signum: Int) {
  self =>

  private var tfirstNonzeroIntNum = 0
  private var tbitLength = 0

  /**
    * Package private method to return bit length for an integer.
    */
  def bitLengthForInt(n: Int): Int = {
    return 32 - java.lang.Integer.numberOfLeadingZeros(n);
  }

  def plus(inv: Vector[Any]): Vector[Any] = {
    val x: Int = inv(0).asInstanceOf[Int]
    val y: Int = inv(1).asInstanceOf[Int]
    Vector(x+y)
  }
  case class Ops(f: Vector[Any] => Vector[Any]){
    def fetchValues(in: Vector[Any]): Vector[Any] = ???
  }
  case class Depgraph(ops: Vector[Ops])
  def createProg(depgraph: Depgraph): (Vector[Any] => Vector[Any]) = {
    //omitting how we got the inital args
    val fullprog: Vector[Any] => Vector[Any] =
      (ini: Vector[Any]) => {
        depgraph.ops.foldLeft(ini) {
          (acc, op) => {
            val v = op.fetchValues(acc)
            acc ++ op.f(v)
          }
        }
      }
    fullprog
  }


  /**
    * Returns the number of bits in the minimal two's-complement
    * representation of this BigInteger, <i>excluding</i> a sign bit.
    * For positive BigIntegers, this is equivalent to the number of bits in
    * the ordinary binary representation.  (Computes
    * {@code (ceil(log2(this < 0 ? -this : this+1)))}.)
    *
    * @return number of bits in the minimal two's-complement
    *         representation of this BigInteger, <i>excluding</i> a sign bit.
    */
  def bitLength(): Int = {
    var n = tbitLength - 1;
    if (n == -1) {
      // bitLength not initialized yet
      var m = mag;
      var len = m.length;
      if (len == 0) {
        n = 0; // offset by one to initialize
      } else {
        // Calculate the bit length of the magnitude
        var magBitLength = ((len - 1) << 5) + bitLengthForInt(mag(0));
        if (signum < 0) {
          // Check if magnitude is a power of two
          var pow2 = (Integer.bitCount(mag(0)) == 1);
          var i = 1
          while (i < len && pow2) {
            pow2 = (mag(i) == 0);
            i = i + 1
          }

          n = if (pow2) magBitLength - 1 else magBitLength
        } else {
          n = magBitLength;
        }
      }
      tbitLength = n + 1;
    }
    return n;
  }


  /**
    * Returns the index of the int that contains the first nonzero int in the
    * little-endian binary representation of the magnitude (int 0 is the
    * least significant). If the magnitude is zero, return value is undefined.
    */
  def firstNonzeroIntNum(): Int = {
    var fn = tfirstNonzeroIntNum - 2;
    if (fn == -2) {
      // firstNonzeroIntNum not initialized yet
      fn = 0;

      // Search for the first nonzero int
      var mlen = mag.length;
      var i = mlen - 1

      while (i >= 0 && mag(i) == 0) i = i - 1
      fn = mlen - i - 1;
      tfirstNonzeroIntNum = fn + 2; // offset by two to initialize
    }
    return fn;
  }


  /* Returns an int of sign bits */
  def signInt(): Int = {
    return if (signum < 0) -1 else 0
  }

  /**
    * Returns the specified int of the little-endian two's complement
    * representation (int 0 is the least significant).  The int number can
    * be arbitrarily high (values are logically preceded by infinitely many
    * sign ints).
    */
  def getInt(n: Int): Int = {
    if (n < 0)
      return 0;
    if (n >= mag.length)
      return signInt();

    val magInt = mag(mag.length - n - 1);

    return if (signum >= 0) magInt else if (n <= firstNonzeroIntNum()) -magInt else ~magInt
  }

  /**
    * Returns a byte array containing the two's-complement
    * representation of this BigInteger.  The byte array will be in
    * <i>big-endian</i> byte-order: the most significant byte is in
    * the zeroth element.  The array will contain the minimum number
    * of bytes required to represent this BigInteger, including at
    * least one sign bit, which is {@code (ceil((this.bitLength() +
     * 1)/8))}.  (This representation is compatible with the
    * {@link #BigInteger(byte[]) (byte[])} constructor.)
    *
    * @return a byte array containing the two's-complement representation of
    *         this BigInteger.
    * @see #BigInteger(byte[])
    */
  def toByteArray(): Array[Byte] = {
    val byteLen: Int = bitLength() / 8 + 1;
    val byteArray = new Array[Byte](byteLen);

    var i = byteLen - 1
    var bytesCopied = 4
    var nextInt = 0
    var intIndex = 0
    while (i >= 0) {
      if (bytesCopied == 4) {
        nextInt = getInt(intIndex)
        intIndex = intIndex + 1
        bytesCopied = 1;
      } else {
        nextInt >>>= 8;
        bytesCopied = bytesCopied + 1;
      }
      byteArray(i) = nextInt.toByte;
      i = i - 1
    }
    return byteArray;
  }

  /**
    * Returns a BigInteger whose value is {@code (this << n)}.
    * The shift distance, {@code n}, may be negative, in which case
    * this method performs a right shift.
    * (Computes <tt>floor(this * 2<sup>n</sup>)</tt>.)
    *
    * @param  n shift distance, in bits.
    * @return { @code this << n}
    * @see #shiftRight
    */
  def shiftLeft(n: Int): MyBigInt = {
    if (signum == 0)
      return MyBigInt.ZERO;
    if (n > 0) {
      return MyBigInt(MyBigInt.shiftLeft(mag, n), signum);
    } else if (n == 0) {
      return this;
    } else {
      // Possible int overflow in (-n) is not a trouble,
      // because shiftRightImpl considers its argument unsigned
      return shiftRightImpl(-n);
    }
  }

  /**
    * Returns a BigInteger whose value is {@code (this >> n)}. The shift
    * distance, {@code n}, is considered unsigned.
    * (Computes <tt>floor(this * 2<sup>-n</sup>)</tt>.)
    *
    * @param  n unsigned shift distance, in bits.
    * @return { @code this >> n}
    */
  def shiftRightImpl(n: Int): MyBigInt = {
    import MyBigInt._
    val nInts = n >>> 5;
    val nBits = n & 0x1f;
    val magLen = mag.length;
    var newMag: Array[Int] = null;


    // Special case: entire contents shifted off the end
    if (nInts >= magLen)
      return if (signum >= 0) ZERO else negConst(1)

    if (nBits == 0) {
      val newMagLen = magLen - nInts;
      newMag = java.util.Arrays.copyOf(mag, newMagLen);
    } else {
      var i = 0;
      var highBits: Int = mag(0) >>> nBits;
      if (highBits != 0) {
        newMag = new Array[Int](magLen - nInts)
        newMag(i) = highBits;
        i = i + 1
      } else {
        newMag = new Array[Int](magLen - nInts - 1)
      }

      var nBits2 = 32 - nBits;
      var j = 0;
      while (j < magLen - nInts - 1) {
        newMag(i) = (mag(j) << nBits2) | (mag(j) >>> nBits);
        i = i + 1
        j = j + 1
      }
    }

    if (signum < 0) {
      // Find out whether any one-bits were shifted off the end.
      var onesLost = false;
      var i = magLen - 1
      var j = magLen - nInts;
      while (i >= j && !onesLost) {
        onesLost = (mag(i) != 0);
        i = i - 1
      }
      if (!onesLost && nBits != 0)
        onesLost = (mag(magLen - nInts - 1) << (32 - nBits)) != 0

      if (onesLost)
        newMag = javaIncrement(newMag);
    }

    return new MyBigInt(newMag, signum);
  }

  def javaIncrement(jval: Array[Int]): Array[Int] = {
    var lastSum = 0;
    var i = jval.length - 1
    while (i >= 0 && lastSum == 0)
      i = i - 1
    jval(i) = jval(i) + 1
    lastSum = jval(i)

    if (lastSum == 0) {
      val njval = new Array[Int](jval.length + 1)
      njval(0) = 1;
      njval
    } else jval

  }


  /**
    * Throws an {@code ArithmeticException} if the {@code BigInteger} would be
    * out of the supported range.
    *
    * @throws ArithmeticException if { @code this} exceeds the supported range.
    */
  def checkRange() {
    if (mag.length > MyBigInt.MAX_MAG_LENGTH || mag.length == MyBigInt.MAX_MAG_LENGTH && mag(0) < 0) {
      //reportOverflow();
    }
  }

  /**
    * Returns a BigInteger whose value is {@code (-this)}.
    *
    * @return { @code -this}
    */
  def negate(): MyBigInt = {
    return new MyBigInt(this.mag, -this.signum);
  }

  /**
    * Returns a BigInteger whose value is the absolute value of this
    * BigInteger.
    *
    * @return { @code abs(this)}
    */
  def abs(): MyBigInt = {
    return if (signum >= 0) this else this.negate()
  }


  /**
    * Returns a new BigInteger representing n lower ints of the number.
    * This is used by Karatsuba multiplication and Karatsuba squaring.
    */
  def getLower(n: Int): MyBigInt = {
    val len = mag.length;

    if (len <= n) {
      return abs();
    }

    val lowerInts: Array[Int] = new Array[Int](n);
    System.arraycopy(mag, len - n, lowerInts, 0, n);

    return MyBigInt(MyBigInt.trustedStripLeadingZeroInts(lowerInts), 1);
  }

  /**
    * Returns a new BigInteger representing mag.length-n upper
    * ints of the number.  This is used by Karatsuba multiplication and
    * Karatsuba squaring.
    */
  def getUpper(n: Int): MyBigInt = {
    val len: Int = mag.length;

    if (len <= n) {
      return MyBigInt.ZERO;
    }

    val upperLen = len - n;
    val upperInts: Array[Int] = new Array[Int](upperLen)
    System.arraycopy(mag, 0, upperInts, 0, upperLen);

    return MyBigInt(MyBigInt.trustedStripLeadingZeroInts(upperInts), 1);
  }

  def multiply(that: MyBigInt): MyBigInt = MyBigInt.multiply(this, that)

  /**
    * Adds the contents of the int arrays x and y. This method allocates
    * a new int array to hold the answer and returns a reference to that
    * array.
    */
  def add(xp: Array[Int], yp: Array[Int]): Array[Int] = {
    import MyBigInt._
    // If x is shorter, swap the two arrays
    val (x, y) = if (xp.length < yp.length) (yp, xp) else (xp, yp)

    var xIndex = x.length;
    var yIndex = y.length;
    val result: Array[Int] = new Array[Int](xIndex);
    var sum: Long = 0;
    if (yIndex == 1) {
      xIndex = xIndex - 1
      sum = (x(xIndex) & LONG_MASK) + (y(0) & LONG_MASK);
      result(xIndex) = sum.toInt
    } else {
      // Add common parts of both numbers
      while (yIndex > 0) {
        xIndex = xIndex - 1
        yIndex = yIndex - 1
        sum = (x(xIndex) & LONG_MASK) +
          (y(yIndex) & LONG_MASK) + (sum >>> 32);
        result(xIndex) = sum.toInt;
      }
    }
    // Copy remainder of longer number while carry propagation is required
    var carry: Boolean = (sum >>> 32) != 0
    while (xIndex > 0 && carry) {
      xIndex = xIndex - 1
      (result(xIndex) = x(xIndex) + 1)
      carry = (result(xIndex) == 0)
    }

    // Copy remainder of longer number
    while (xIndex > 0) {
      xIndex = xIndex - 1
      result(xIndex) = x(xIndex)
    }

    // Grow result if necessary
    if (carry) {
      val bigger: Array[Int] = new Array[Int](result.length + 1)
      System.arraycopy(result, 0, bigger, 1, result.length);
      bigger(0) = 0x01;
      return bigger;
    }
    return result;
  }

  /**
    * Subtracts the contents of the second int arrays (little) from the
    * first (big).  The first int array (big) must represent a larger number
    * than the second.  This method allocates the space necessary to hold the
    * answer.
    */
  def subtract(big: Array[Int], little: Array[Int]): Array[Int] = {
    import MyBigInt._
    var bigIndex = big.length;
    var result: Array[Int] = new Array[Int](bigIndex)
    var littleIndex = little.length;
    var difference: Long = 0;

    // Subtract common parts of both numbers
    while (littleIndex > 0) {
      bigIndex = bigIndex - 1
      littleIndex = littleIndex - 1
      difference = (big(bigIndex) & LONG_MASK) -
        (little(littleIndex) & LONG_MASK) +
        (difference >> 32);
      result(bigIndex) = difference.toInt
    }

    // Subtract remainder of longer number while borrow propagates
    var borrow: Boolean = (difference >> 32) != 0
    while (bigIndex > 0 && borrow) {
      bigIndex = bigIndex - 1
      borrow = ((result(bigIndex) = big(bigIndex) - 1) == -1);
    }

    // Copy remainder of longer number
    while (bigIndex > 0) {
      result(bigIndex) = big(bigIndex)
    }

    return result;
  }


  /**
    * Returns a BigInteger whose value is {@code (this + val)}.
    *
    * @param  val value to be added to this BigInteger.
    * @return { @code this + val}
    */
  def add(jval: MyBigInt): MyBigInt = {
    import MyBigInt._
    if (jval.signum == 0)
      return this;
    if (signum == 0)
      return jval;
    if (jval.signum == signum)
      return MyBigInt(add(mag, jval.mag), signum);

    val cmp = compareMagnitude(jval);
    if (cmp == 0)
      return ZERO;
    var resultMag: Array[Int] = if (cmp > 0) subtract(mag, jval.mag) else subtract(jval.mag, mag)
    resultMag = trustedStripLeadingZeroInts(resultMag);

    return new MyBigInt(resultMag, if (cmp == signum) 1 else -1)
  }

  /**
    * Returns a BigInteger whose value is {@code (this - val)}.
    *
    * @param  val value to be subtracted from this BigInteger.
    * @return { @code this - val}
    */
  def subtract(jval: MyBigInt): MyBigInt = {
    if (jval.signum == 0)
      return this;
    if (signum == 0)
      return jval.negate();
    if (jval.signum != signum)
      return MyBigInt(add(mag, jval.mag), signum);

    var cmp = compareMagnitude(jval);
    if (cmp == 0)
      return MyBigInt.ZERO;
    var resultMag = if (cmp > 0) subtract(mag, jval.mag) else subtract(jval.mag, mag)
    resultMag = MyBigInt.trustedStripLeadingZeroInts(resultMag);
    return new MyBigInt(resultMag, if (cmp == signum) 1 else -1)
  }


  /**
    * Compares the magnitude array of this BigInteger with the specified
    * BigInteger's. This is the version of compareTo ignoring sign.
    *
    * @param val BigInteger whose magnitude array to be compared.
    * @return -1, 0 or 1 as this magnitude array is less than, equal to or
    *         greater than the magnitude aray for the specified BigInteger's.
    */
  def compareMagnitude(jval: MyBigInt): Int = {
    import MyBigInt._
    val m1 = mag;
    val len1 = m1.length;
    val m2 = jval.mag;
    val len2 = m2.length;
    if (len1 < len2)
      return -1;
    if (len1 > len2)
      return 1;
    var i = 0;
    var a = -1
    var b = 1
    while (i < len1 && a != b) {
      a = m1(i)
      b = m2(i)
      i = i + 1
    }
    if (a != b)
      if ((a & LONG_MASK) < (b & LONG_MASK)) -1 else 1
    else return 0;
  }

}

object MyBigInt {

  /**
    * The threshold value for using squaring code to perform multiplication
    * of a {@code BigInteger} instance by itself.  If the number of ints in
    * the number are larger than this value, {@code multiply(this)} will
    * return {@code square()}.
    */
  val MULTIPLY_SQUARE_THRESHOLD = 20;
  val ZERO: MyBigInt = MyBigInt(new Array[Int](0), 0);
  val LONG_MASK: Long = 0xffffffffL;
  val MAX_MAG_LENGTH = Integer.MAX_VALUE / Integer.SIZE + 1;
  /**
    * Initialize static constant array when class is loaded.
    */
  val MAX_CONSTANT: Int = 16
  val negConst: Array[MyBigInt] = new Array[MyBigInt](MAX_CONSTANT + 1)
  val KARATSUBA_THRESHOLD = 80
  // (1 << 26)


  /**
    * Returns a copy of the input array stripped of any leading zero bytes.
    */
  def stripLeadingZeroBytes(a: Array[Byte]): Array[Int] = {
    var byteLength = a.length;
    var keep = 0;

    // Find first nonzero byte
    while (keep < byteLength && a(keep) == 0)
      keep = keep + 1


    // Allocate new array and copy relevant part of input array
    var intLength = ((byteLength - keep) + 3) >>> 2;
    val result = new Array[Int](intLength);
    var b = byteLength - 1;
    var i = intLength - 1;
    while (i >= 0) {
      result(i) = a(b) & 0xff;
      b = b - 1
      val bytesRemaining = b - keep + 1;
      val bytesToTransfer = Math.min(3, bytesRemaining);
      var j = 8;
      while (j <= (bytesToTransfer << 3)) {
        result(i) |= ((a(b) & 0xff) << j);
        j = j + 8
        b = b - 1
      }
      i = i - 1
    }
    return result;
  }

  /**
    * Takes an array a representing a negative 2's-complement number and
    * returns the minimal (no leading zero bytes) unsigned whose value is -a.
    */
  def makePositive(a: Array[Byte]): Array[Int] = {
    var keep: Int = 0
    var k: Int = 0
    val byteLength: Int = a.length;

    // Find first non-sign (0xff) byte of input

    while (keep < byteLength && a(keep) == -1)
      keep = keep + 1

    /* Allocate output array.  If all non-sign bytes are 0x00, we must
     * allocate space for one extra output byte. */
    k = keep
    while (k < byteLength && a(k) == 0)
      k = k + 1

    val extraByte: Int = if (k == byteLength) 1 else 0;
    val intLength: Int = ((byteLength - keep + extraByte) + 3) >>> 2;
    val result: Array[Int] = new Array[Int](intLength);

    /* Copy one's complement of input into output, leaving extra
     * byte (if it exists) == 0x00 */
    var b: Int = byteLength - 1;
    var i = intLength - 1

    while (i >= 0) {

      result(i) = a(b) & 0xff; //b--
      b = b - 1
      var numBytesToTransfer: Int = Math.min(3, b - keep + 1);
      if (numBytesToTransfer < 0)
        numBytesToTransfer = 0;
      var j: Int = 8
      while (j <= 8 * numBytesToTransfer) {
        result(i) |= ((a(b) & 0xff) << j);
        b = b - 1
        j += 8
      }

      // Mask indicates which bits must be complemented
      val mask: Int = -1 >>> (8 * (3 - numBytesToTransfer));
      result(i) = ~result(i) & mask;
      i = i - 1
    }

    // Add one to one's complement to generate two's complement
    i = result.length - 1
    var cont = true //
    while (i >= 0 && cont) {
      result(i) = ((result(i) & LONG_MASK) + 1).toInt;
      if (result(i) != 0)
        cont = false
      else
        i = i - 1
    }

    return result;
  }

  def apply(barray: Array[Byte]): MyBigInt = {
    if (barray.length == 0)
      throw new NumberFormatException("Zero length BigInteger");

    val (mag, signum) = if (barray(0) < 0) {
      (makePositive(barray), -1)
    } else {
      val mag = stripLeadingZeroBytes(barray);
      val signum = if (mag.length == 0) 0 else 1
      (mag, signum)
    }
    if (mag.length >= MAX_MAG_LENGTH) {
      //checkRange();
    }
    MyBigInt(mag, signum)
  }

  /**
    * Returns a magnitude array whose value is {@code (mag << n)}.
    * The shift distance, {@code n}, is considered unnsigned.
    * (Computes <tt>this * 2<sup>n</sup></tt>.)
    *
    * @param mag magnitude, the most-significant int ({ @code mag[0]}) must be non-zero.
    * @param  n  unsigned shift distance, in bits.
    * @return { @code mag << n}
    */
  def shiftLeft(mag: Array[Int], n: Int): Array[Int] = {
    val nInts: Int = n >>> 5;
    val nBits: Int = n & 0x1f;
    val magLen: Int = mag.length;
    var newMag: Array[Int] = null;

    if (nBits == 0) {
      newMag = new Array[Int](magLen + nInts);
      System.arraycopy(mag, 0, newMag, 0, magLen);
    } else {
      var i = 0;
      var nBits2: Int = 32 - nBits;
      var highBits: Int = mag(0) >>> nBits2;
      if (highBits != 0) {
        newMag = new Array[Int](magLen + nInts + 1)
        newMag(i) = highBits;
        i = i + 1
      } else {
        newMag = new Array[Int](magLen + nInts)
      }
      var j = 0
      while (j < magLen - 1) {
        newMag(i) = mag(j) << nBits | mag(j) >>> nBits2;
        i = i + 1
        j = j + 1
      }
      newMag(i) = mag(j) << nBits;
    }
    return newMag;
  }


  def multiplyByInt(x: Array[Int], y: Int, sign: Int): MyBigInt = {
    if (Integer.bitCount(y) == 1) {
      val t = MyBigInt(shiftLeft(x, Integer.numberOfTrailingZeros(y)), sign);
      return t
    }
    val xlen: Int = x.length;
    var rmag: Array[Int] = new Array[Int](xlen + 1)
    var carry: Long = 0;
    var yl: Long = y & LONG_MASK;
    var rstart: Int = rmag.length - 1;
    var i: Int = xlen - 1
    while (i >= 0) {
      val product: Long = (x(i) & LONG_MASK) * yl + carry;
      rmag(rstart) = product.toInt
      rstart = rstart - 1
      carry = product >>> 32;
      i = i - 1
    }
    if (carry == 0L) {
      rmag = java.util.Arrays.copyOfRange(rmag, 1, rmag.length);
    } else {
      rmag(rstart) = carry.toInt
    }
    return MyBigInt(rmag, sign);
  }


  /**
    * Multiplies int arrays x and y to the specified lengths and places
    * the result into z. There will be no leading zeros in the resultant array.
    */
  def multiplyToLen(x: Array[Int], xlen: Int, y: Array[Int], ylen: Int, zp: Array[Int]): Array[Int] = {

    val xstart: Int = xlen - 1;
    val ystart: Int = ylen - 1;
    var z: Array[Int] = zp

    if (zp == null || zp.length < (xlen + ylen))
      z = new Array[Int](xlen + ylen)

    var carry: Long = 0;
    var j = ystart
    var k = ystart + 1 + xstart
    while (j >= 0) {
      val product: Long = (y(j) & LONG_MASK) *
        (x(xstart) & LONG_MASK) + carry;
      z(k) = product.toInt
      carry = product >>> 32;
      j = j - 1
      k = k - 1
    }
    z(xstart) = carry.toInt

    var i: Int = xstart - 1
    while (i >= 0) {
      carry = 0;
      var j = ystart
      var k = ystart + 1 + i;
      while (j >= 0) {
        val product: Long = (y(j) & LONG_MASK) *
          (x(i) & LONG_MASK) +
          (z(k) & LONG_MASK) + carry;
        z(k) = product.toInt
        carry = product >>> 32;
        j = j - 1
        k = k - 1
      }
      z(i) = carry.toInt
      i = i - 1
    }
    return z;
  }

  /**
    * Returns the input array stripped of any leading zero bytes.
    * Since the source is trusted the copying may be skipped.
    */
  def trustedStripLeadingZeroInts(jval: Array[Int]): Array[Int] = {
    val vlen: Int = jval.length;
    var keep: Int = 0

    // Find first nonzero byte
    while (keep < vlen && jval(keep) == 0)
      keep = keep + 1

    return if (keep == 0) jval else java.util.Arrays.copyOfRange(jval, keep, vlen);
  }

  /**
    * Multiplies two BigIntegers using the Karatsuba multiplication
    * algorithm.  This is a recursive divide-and-conquer algorithm which is
    * more efficient for large numbers than what is commonly called the
    * "grade-school" algorithm used in multiplyToLen.  If the numbers to be
    * multiplied have length n, the "grade-school" algorithm has an
    * asymptotic complexity of O(n^2).  In contrast, the Karatsuba algorithm
    * has complexity of O(n^(log2(3))), or O(n^1.585).  It achieves this
    * increased performance by doing 3 multiplies instead of 4 when
    * evaluating the product.  As it has some overhead, should be used when
    * both numbers are larger than a certain threshold (found
    * experimentally).
    *
    * See:  http://en.wikipedia.org/wiki/Karatsuba_algorithm
    */
  def multiplyKaratsuba(x: MyBigInt, y: MyBigInt): MyBigInt = {
    val xlen = x.mag.length;
    val ylen = y.mag.length;

    // The number of ints in each half of the number.
    val half: Int = (Math.max(xlen, ylen) + 1) / 2;

    // xl and yl are the lower halves of x and y respectively,
    // xh and yh are the upper halves.
    val xl: MyBigInt = x.getLower(half);
    val xh: MyBigInt = x.getUpper(half);
    val yl: MyBigInt = y.getLower(half);
    val yh: MyBigInt = y.getUpper(half);

    val p1: MyBigInt = xh.multiply(yh);
    // p1 = xh*yh
    val p2: MyBigInt = xl.multiply(yl); // p2 = xl*yl

    // p3=(xh+xl)*(yh+yl)
    val p3: MyBigInt = xh.add(xl).multiply(yh.add(yl));

    // result = p1 * 2^(32*2*half) + (p3 - p1 - p2) * 2^(32*half) + p2
    val result: MyBigInt = p1.shiftLeft(32 * half).add(p3.subtract(p1).subtract(p2)).shiftLeft(32 * half).add(p2);

    if (x.signum != y.signum) {
      return result.negate();
    } else {
      return result;
    }
  }

  def multiply(me: MyBigInt, that: MyBigInt): MyBigInt = {
    import me._
    if (that.signum == 0 || signum == 0)
      return ZERO;

    val xlen: Int = me.mag.length;

    if (that == me && xlen > MULTIPLY_SQUARE_THRESHOLD) {
      return ??? //square();
    }

    val ylen = that.mag.length;

    if ((xlen < KARATSUBA_THRESHOLD) || (ylen < KARATSUBA_THRESHOLD)) {
      val resultSign = if (signum == that.signum) 1 else -1;
      if (that.mag.length == 1) {
        return multiplyByInt(mag, that.mag(0), resultSign);
      }
      if (mag.length == 1) {
        return multiplyByInt(that.mag, mag(0), resultSign);
      }
      var result: Array[Int] = multiplyToLen(mag, xlen, that.mag, ylen, null);
      result = trustedStripLeadingZeroInts(result);
      return new MyBigInt(result, resultSign);
    } else {
      return multiplyKaratsuba(me, that);
    }
  }
}

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen._

object VanilaKaratsuba extends org.scalacheck.Properties("Karatsuba") {


  def convert(before: java.math.BigInteger): MyBigInt = {
    MyBigInt(before.toByteArray)
  }


  def chooseBigInt: Gen[BigInt] =
    sized((s: Int) => choose(-s, s)) map (x => BigInt(x))

  def chooseReallyBigInt: Gen[BigInt] = for {
    bi <- chooseBigInt
    n <- choose(32,128)
  } yield bi << n


  property("ByteRep Same") = forAll(chooseReallyBigInt) { l =>

    val ba = l.toByteArray
    val mbi = convert(l.bigInteger)
    val mbiba = mbi.toByteArray()
    ba.corresponds(mbiba) {
      _ == _
    }
  }


  property("ByteRep Same") = forAll(chooseBigInt) { l =>
    val plus3 = l + 3
    val check = l * plus3

    val mbi = convert(l.bigInteger)
    val mbi2 = convert(plus3.bigInteger)

    val mres = mbi.multiply(mbi2)

    mres.toByteArray().corresponds(check.toByteArray) {
      _ == _
    }
  }

  property("ByteRep Same large") = forAll(chooseReallyBigInt) { l =>
    val plus3 = l + 3
    val check = l * plus3

    val mbi = convert(l.bigInteger)
    val mbi2 = convert(plus3.bigInteger)

    val mres = mbi.multiply(mbi2)

    mres.toByteArray().corresponds(check.toByteArray) {
      _ == _
    }
  }


/*    val l: Long = Long.MaxValue
    val bi: BigInt = BigInt(l)


    val oba = bi.bigInteger.toByteArray
    val bifo = new java.math.BigInteger(oba)
    val mi: MyBigInt = convert(bi.bigInteger)

    val ba1 = mi.toByteArray()
    val ba2 = bi.bigInteger.toByteArray()

    for (i <- 0 until ba1.length)
      if (ba1(i) != ba2(i)) println("diff")

    val bs = bi * bi
    val bs1 = bs / bs

    val mbi = convert(bs.bigInteger)
    val mbi1 = convert(bs1.bigInteger)

    val r = bs * bs1

    val rcon = convert(r.bigInteger)
    val m = mbi.multiply(mbi1)




    for (i <- 0 until rcon.mag.size)
      if (rcon.mag(i) != m.mag(i)) println("difference")




    println(r)*/

}
