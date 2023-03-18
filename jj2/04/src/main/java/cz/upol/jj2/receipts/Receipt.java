package cz.upol.jj2.receipts;

import java.util.ArrayList;
import java.util.List;

/**
 * This class represents receipts containing some information.
 */
public class Receipt {
    private long id;
    private String name;
    private String itin;
    private List<ReceiptItem> items;

    public Receipt() {
        items = new ArrayList<ReceiptItem>();
    }

    public Receipt(long id, String name, String itin, List<ReceiptItem> items) {
        this.id = id;
        this.name = name;
        this.itin = itin;
        this.items = items;
    }

    public int getTotal() {
        int total = 0;

        for (ReceiptItem item : this.items) {
            total += item.getUnitPrice() * item.getAmount();
        }

        return total;
    }

    @Override
    public String toString() {
        StringBuilder output = new StringBuilder(
                "Name: " + getName() + "\nItin: " + getItin() + "\nItems:"
        );

        for (ReceiptItem item : items) {
            output.append(item.toString());
        }

        return output.toString();
    }

    //
    // Basic getters & setters
    //

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getItin() {
        return itin;
    }

    public void setItin(String itin) {
        this.itin = itin;
    }

    public List<ReceiptItem> getItems() {
        return items;
    }

    public void setItems(List<ReceiptItem> items) {
        this.items = items;
    }

    public void addItem(ReceiptItem item) {
        this.items.add(item);
    }

    public void removeItem(ReceiptItem item) {
        this.items.remove(item);
    }
}