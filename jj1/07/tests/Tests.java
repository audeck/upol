import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import cz.upol.jj1.collections.Set;
import cz.upol.jj1.collections.UPLinkedList;
import org.junit.jupiter.api.Test;

public class Tests {

  UPLinkedList<Integer> ll;
  Set<Integer> s;
  Set<Integer> s1;
  Set<Integer> s2;

  @Test
  public void linkedListTest0() {
    // constructor & insert
    ll = new UPLinkedList<Integer>(3, 8, 1, 11);

    // contains
    assertTrue(ll.contains(8));
    assertFalse(ll.contains(0));
    // toString
    assertEquals(ll.toString(), "[3, 8, 1, 11]");
    // size
    assertEquals(ll.size(), 4);
  }

  @Test
  public void linkedListTest1() {
    ll = new UPLinkedList<Integer>(3, 8, 1, 11);

    // remove
    assertFalse(ll.delete(5));
    assertTrue(ll.delete(1));
    assertFalse(ll.delete(1));
    assertEquals(ll.size(), 3);
    assertEquals(ll.toString(), "[3, 8, 11]");
  }

  @Test
  public void linkedListTestEmpty() {
    ll = new UPLinkedList<Integer>();

    assertEquals(ll.size(), 0);
    assertEquals(ll.toString(), "[]");
  }

  @Test
  public void setTest0() {
    // constructor & insert
    s = new Set<Integer>(0, 2, 6, 9, 23);

    // contains
    assertFalse(s.contains(1));
    assertFalse(s.contains(4));
    assertTrue(s.contains(0));
    assertTrue(s.contains(23));

    // size
    assertEquals(s.size(), 5);
  }

  @Test
  public void setTest1() {
    s = new Set<Integer>(0, 2, 6, 9, 23);

    // remove & toString
    assertFalse(s.remove((Integer) 3));
    assertTrue (s.remove((Integer) 2));
    assertEquals(s.size(), 4);
    assertEquals(s.toString(), "[0, 6, 9, 23]");
  }

  @Test public void setTest2() {
    s = new Set<Integer>(0, 2, 6, 9, 23);
    s1 = new Set<Integer>(2, 6, 11, 14);

    Set<Integer> intersection = s.intersection(s1);
    Set<Integer> union = s.intersection(s1);
    Set<Integer> difference = s.difference(s1);

    assertEquals(intersection.compareTo(new Set<Integer>(2, 6)), 0);
    assertEquals(union.compareTo(new Set<Integer>(0, 2, 6, 9, 11, 14, 23)), 0);
    assertEquals(difference.compareTo(new Set<Integer>(0, 9, 23)), 0);
  }

  @Test
  public void setTestEmpty() {
    s = new Set<Integer>();

    // empty set
    assertEquals(s.size(), 0);
    assertEquals(s.toString(), "[]");
  }
}
