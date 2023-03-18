package cz.upol.jj2.receipts;

/**
 * This class represents information about a receipt item.
 * @see Receipt
 */
public class ReceiptItem {
    private String name;
    private int unitPrice;
    private int amount; // Should probably be a float

    public ReceiptItem() {}

    public ReceiptItem(String name, int unitPrice, int amount) {
        this.name = name;
        this.unitPrice = unitPrice;
        this.amount = amount;
    }

    @Override
    public String toString() {
        return "\n  Name: " + getName()
                + "\n  Amount: " + getAmount()
                + "\n  Unit Price: " + getUnitPrice();
    }

    //
    // Basic getters & setters
    //

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getUnitPrice() {
        return unitPrice;
    }

    public void setUnitPrice(int unitPrice) {
        if (unitPrice < 0) {
            throw new IllegalArgumentException("The price of a receipt item shouldn't be negative(?)");
        }

        this.unitPrice = unitPrice;
    }

    public int getAmount() {
        return amount;
    }

    public void setAmount(int amount) {
        if (amount < 0) {
            throw new IllegalArgumentException("The amount of an item shouldn't be negative");
        }

        this.amount = amount;
    }

    public void incAmount() {
        this.amount += 1;
    }

    public void decAmount() {
        this.amount -= 1;
    }
}
