import static cz.upol.jj1.Main.freq;
import static cz.upol.jj1.Main.freqIgnoreCase;
import static cz.upol.jj1.Main.rpnCalc;
import static cz.upol.jj1.Main.parseInt;
import static cz.upol.jj1.Main.parseDouble;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import cz.upol.jj1.Point;
import cz.upol.jj1.PointFixed;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.Test;

public class Tests {

  @Test
  public void pointTest0() {
    Set<Point> points = new HashSet<>();
    Point point0 = new Point(3, 2);
    Point point1 = new Point(-1, 4);
    Point point2 = new Point(1, -2);
    Point point3 = new Point(3, 2);

    points.add(point0);
    points.add(point1);
    points.add(point2);
    points.add(point3);

    assertEquals(points.size(), 4);
    System.out.println(points);
  }

  @Test
  public void pointTest1() {
    Set<PointFixed> points = new HashSet<>();
    PointFixed point0 = new PointFixed(3, 2);
    PointFixed point1 = new PointFixed(-1, 4);
    PointFixed point2 = new PointFixed(1, -2);
    PointFixed point3 = new PointFixed(3, 2);

    points.add(point0);
    points.add(point1);
    points.add(point2);
    points.add(point3);

    assertEquals(points.size(), 3);
    System.out.println(points);
  }

  @Test
  public void freqTest() {
    Map<String, Integer> frequencies = freq("hello2puck;flip.how elk hello/Hello");

    assertEquals(frequencies.get("Hello"), 1);
    assertEquals(frequencies.get("hello"), 2);
    assertEquals(frequencies.get("elk"), 1);
    // etc.

    System.out.println(frequencies);
  }

  @Test
  public void freqIgnoreCaseTest() {
    Map<String, Integer> frequencies = freqIgnoreCase("hello2puck;flip.how elk hello/Hello");

    assertEquals(frequencies.get("hello"), 3);
    assertEquals(frequencies.get("elk"), 1);
    // etc.

    System.out.println(frequencies);
  }

  @Test
  public void rpnCalcTest() {
    assertThrows(IllegalArgumentException.class, () -> rpnCalc("1 2 asdf "));

    assertEquals(rpnCalc("1 -2 3 + + "), "2");
    assertEquals(rpnCalc("1 32 + 42 * 5 + 66 -"), "1325");

    assertEquals(rpnCalc("1.1 2 +"), "3.1");

    assertEquals(rpnCalc("1.1 2 <"), "#t");
    assertEquals(rpnCalc("#f #t <"), "#t");
    assertEquals(rpnCalc("#f #t >"), "#f");

    assertThrows(IllegalArgumentException.class, () -> rpnCalc("1 #f ="));
    assertEquals(rpnCalc("1 1 ="), "#t");

    assertEquals(rpnCalc("1 2 3 4 < ?"), "2");
    assertThrows(IllegalArgumentException.class, () -> rpnCalc("1 2 3 4 + ?"));
  }

  @Test
  public void rpnCalcBindingsTest() {
    Map<String, Object> bindings = new HashMap<>();
    bindings.put("foo", 6.3);
    bindings.put("bar", 9);

    assertEquals(rpnCalc("foo bar + 3 /", bindings), "5.1000000000000005");  // nice math
  }

  @Test
  public void parseIntTest() {
    assertThrows(NumberFormatException.class, () -> parseInt(null));
    assertThrows(NumberFormatException.class, () -> parseInt("123a"));

    assertEquals(parseInt("1234").get(), 1234);
    assertEquals(parseInt("-00000000694317289").get(), -694317289);
    assertEquals(parseInt("6943172891").get(), Integer.MAX_VALUE);
    assertEquals(parseInt("-6943172891").get(), -Integer.MAX_VALUE);
  }

  @Test
  public void parseDoubleTest() {
    assertThrows(NumberFormatException.class, () -> parseDouble(null));
    assertThrows(NumberFormatException.class, () -> parseDouble("123a"));
    assertThrows(NumberFormatException.class, () -> parseDouble("123.123.123"));

    assertEquals(parseDouble("1234").get(), 1234d);
    assertEquals(parseDouble("-0004535.1239").get(), -4535.1239);
    assertEquals(parseDouble("9999999999999999999999999999999999999999999999999999999999999999"
        + "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999"
        + "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999"
        + "9999999999999999999999999999999999999999999999999999999999999999999999999.123").get(),
        Double.POSITIVE_INFINITY);
    assertEquals(parseDouble("0.00000000000000000000000000000000000000000000000000000000000000"
        + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        + "00000000000000000000000000000000000000000000000000000000000000000000001").get(),
        0d);
  }
}
