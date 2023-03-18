package cz.upol.jj2;

import cz.upol.jj2.receipts.Receipt;
import cz.upol.jj2.receipts.ReceiptDAO;
import cz.upol.jj2.receipts.ReceiptDAOException;
import cz.upol.jj2.receipts.ReceiptItem;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

public class Main {
    public static void main(String[] args) {
        // test();
    }

    private static void test() {
        Path propertiesPath = Paths.get("config.properties");
        Properties properties = new Properties();

        try {
            properties.load(Files.newInputStream(propertiesPath));
        } catch (IOException e) {
            e.printStackTrace();
        }

        String username = properties.getProperty("postgres.username");
        String password = properties.getProperty("postgres.password");

        List<ReceiptItem> r1Items = Arrays.asList(
                new ReceiptItem("Nitroglycerin", 24, 50),
                new ReceiptItem("Jet Propelled Pogo Stick", 100, 4),
                new ReceiptItem("Hen Grenade", 42, 1)
        );
        Receipt r1 = new Receipt(0L, "ACME corp.", "CZ12345678", r1Items);

        List<ReceiptItem> r2Items = Arrays.asList(
                new ReceiptItem("Unobtainium", 999_999_999, 1),
                new ReceiptItem("The Perfectly Generic Item", 1, 123)
        );
        Receipt r2 = new Receipt(42L, "Very Big Corporation of America", "US87654321", r2Items);

        List<ReceiptItem> r3Items = Arrays.asList(
                new ReceiptItem("Butt Stallion", 555, 5),
                new ReceiptItem("Portal Gun", 123_456, 1)
        );
        Receipt r3 = new Receipt(1234L, "Skynet", "AI01010101", r3Items);


        try (ReceiptDAO receiptDAO = new ReceiptDAO(username, password)) {
            receiptDAO.saveOrUpdateReceipt(r1);
            receiptDAO.saveOrUpdateReceipt(r2);
            receiptDAO.saveOrUpdateReceipt(r3);

            // SELECT * FROM receipts;
            // SELECT * FROM receipt_items;

            receiptDAO.removeReceipt(r2);

            // SELECT * FROM receipts;
            // SELECT * FROM receipt_items;

            r2.setId(r3.getId());

            System.out.println(r2);

            receiptDAO.updateReceipt(r2);

            // SELECT * FROM receipts;
            // SELECT * FROM receipt_items;
        } catch (ReceiptDAOException e) {
            e.printStackTrace();
        }
    }
}
