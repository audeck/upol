package cz.upol.jj1.collections;

import java.util.Iterator;
import java.util.NoSuchElementException;

/** LinkedList implementation for integers */
public class UPLinkedList<T> implements Iterable<T>, Sequence<T> {

  /** Class represents item in LinkedList */
  private class Node<T> {
    /** Node value */
    private T value;

    /** Pointer to the next node in the list. If NULL this node is the last one. */
    private Node<T> next;

    public Node(T value, Node<T> next) {
      this.value = value;
      this.next = next;
    }

    public Node(T value) {
      this.value = value;
      this.next = null;
    }

    public Node<T> getNext() {
      return next;
    }

    public void setNext(Node<T> next) {
      this.next = next;
    }

    public T getValue() {
      return value;
    }

    public void setValue(T value) {
      this.value = value;
    }
  }

  /** List first value. If NULL, then the list is empty */
  private Node<T> root;

  // public UPLinkedList() {}

  @SafeVarargs
  public UPLinkedList(T... values) {
    for (T value : values) {
      insert(value);
    }
  }

  /**
   * Returns the number of elements in this list. If this list contains more than Integer.MAX_VALUE
   * elements, returns Integer.MAX_VALUE.
   *
   * @return the number of elements in this linked list
   */
  public int size() {
    if (this.root == null) {
      return 0;
    }
    Node<T> current = this.root;
    int size = 1;

    while (current.getNext() != null) {
      current = current.getNext();
      size += 1;

      if (size == Integer.MAX_VALUE) {
        return size;
      }
    }

    return size;
  }

  /**
   * Insert new value to the list
   *
   * @param value Value to be inserted
   */
  public void insert(T value) {
    if (root == null) {
      root = new Node<T>(value);
      return;
    }
    Node<T> current = root;

    while (current.getNext() != null) {
      current = current.getNext();
    }

    current.next = new Node<T>(value);
  }

  /**
   * Checks if list contains a given value
   *
   * @param value checked value
   * @return true if list contains value, otherwise false
   */
  public boolean contains(T value) {
    if (this.root == null) {
      return false;
    }

    Node<T> current = this.root;

    while (current.getNext() != null) {
      if (current.getValue() == value) {
        return true;
      }
      current = current.getNext();
    }

    return current.getValue() == value;
  }

  /**
   * Deletes the first node of given value in this list
   *
   * @param value value to be deleted
   * @return true if a value has been deleted, otherwise false
   */
  public boolean delete(T value) {
    if (this.root == null) {
      return false;
    }

    if (this.root.getValue() == value) {
      this.root = this.root.getNext();
      return true;
    }

    Node<T> previous = this.root;
    Node<T> current = this.root.getNext();

    while (current != null) {
      if (current.getValue() == value) {
        previous.setNext(current.getNext());
        return true;
      }

      previous = current;
      current = current.getNext();
    }

    return false;
  }

  /**
   * Returns a very basic integer iterator of this list. Iterator doesn't check for concurrent
   * modification.
   *
   * @return this list's iterator
   */
  public Iterator<T> iterator() {
    return new ListItr(this.root);
  }

  private class ListItr implements java.util.Iterator<T> {

    private Node<T> next;

    ListItr(Node<T> root) {
      this.next = root;
    }

    /** @return true if list contains a next value, otherwise false */
    public boolean hasNext() {
      return next != null;
    }

    /**
     * Returns the list's current value and iterates over to the next value
     *
     * @return current value in list
     */
    public T next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }

      Node<T> current = next;
      next = next.getNext();

      return current.getValue();
    }
  }

  @Override
  public String toString() {
    StringBuilder description = new StringBuilder("[");
    Iterator<T> itr = this.iterator();
    boolean isFirst = true;

    while (itr.hasNext()) {
      if (!isFirst) {
        description.append(", ");
      }
      description.append(itr.next());
      isFirst = false;
    }

    description.append("]");
    return description.toString();
  }
}
