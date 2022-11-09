import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import cz.upol.jj1.collections.BitSet;
import cz.upol.jj1.collections.UPLinkedList;
import org.junit.jupiter.api.Test;

public class Tests {

  UPLinkedList ll;
  BitSet bs;

  @Test
  public void linkedListTest() {
    // constructor & insert
    ll = new UPLinkedList(3, 8, 1, 11);

    // toString
    assertEquals(ll.toString(), "[3, 8, 1, 11]");
    // size
    assertEquals(ll.size(), 4);
    // contains
    assertTrue(ll.contains(8));
    assertFalse(ll.contains(0));
    // remove
    assertFalse(ll.delete(5));
    assertTrue(ll.delete(1));
    assertFalse(ll.delete(1));
    assertEquals(ll.size(), 3);
    assertEquals(ll.toString(), "[3, 8, 11]");

    // empty list
    ll.delete(3);
    ll.delete(8);
    ll.delete(11);
    assertEquals(ll.size(), 0);
    assertEquals(ll.toString(), "[]");
  }

  @Test
  public void bitSetTest() {
    // constructor & insert
    bs = new BitSet(0, 2, 6, 9, 23);

    // contains
    assertFalse(bs.contains(1));
    assertFalse(bs.contains(4));
    assertTrue(bs.contains(0));
    assertTrue(bs.contains(23));

    // size
    assertEquals(bs.size(), 5);

    // remove & toString
    assertFalse(bs.delete(3));
    assertTrue(bs.delete(2));
    assertEquals(bs.size(), 4);
    assertEquals(bs.toString(), "[0, 6, 9, 23]");

    // empty set
    bs.delete(0);
    bs.delete(6);
    bs.delete(9);
    bs.delete(23);
    assertEquals(bs.size(), 0);
    assertEquals(bs.toString(), "[]");
  }
}
