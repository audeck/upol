package cz.upol.jj2.receipts;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

/**
 * This class implements database object functionality for ReceiptItem objects. Objects of this class are closely
 * coupled with ReceiptDAO objects and shouldn't be used on their own.
 * <p>
 * The table objects of this class create and use could be created with the following expression:
 *      CREATE TABLE `TABLE_NAME`(
 *          id         BIGINT  NOT NULL,
 *          name       VARCHAR NOT NULL,
 *          unit_price INT     NOT NULL CHECK (unit_price > 0),
 *          amount     INT     NOT NULL CHECK (amount >= 0),
 *          PRIMARY KEY(id, name),
 *          FOREIGN KEY(id) REFERENCES `ReceiptDAO.TABLE_NAME`
 *      );
 */
class ReceiptItemDAO {

    private static final String TABLE_NAME = "receipt_items";
    private final Connection connection;

    public ReceiptItemDAO(Connection connection) {
        this.connection = connection;
    }



    /* ~~~~~~~~~~~~~~~ Database methods ~~~~~~~~~~~~~~~ */

    /**
     * Queries the database and returns a list of all receipt items corresponding to some receipt `id`.
     */
    public List<ReceiptItem> getItems(long id) throws ReceiptDAOException {
        List<ReceiptItem> items = new ArrayList<>();
        String sql = "SELECT * FROM " + TABLE_NAME + " WHERE id = ?";

        try (PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setLong(1, id);

            try (ResultSet results = statement.executeQuery()) {
                while (results.next()) {
                    items.add(new ReceiptItem(
                            results.getString("name"),
                            results.getInt("unit_price"),
                            results.getInt("amount")
                    ));
                }
            }
        } catch (SQLException e) {
            throw new ReceiptDAOException(
                    "[ReceiptItemDAO]: Failed to get items of receipt #" + id, e);
        }

        return items;
    }

    /**
     * Inserts a list of items into the database.
     */
    public void saveItems(long id, List<ReceiptItem> items) throws ReceiptDAOException {
        for (ReceiptItem item : items) {
            saveItem(id, item);
        }
    }

    /**
     * Inserts a single item into the database.
     */
    public void saveItem(long id, ReceiptItem item) throws ReceiptDAOException {
        String sql = "INSERT INTO " + TABLE_NAME + " (id, name, unit_price, amount) VALUES (?, ?, ?, ?)";

        try (PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setLong(1, id);
            statement.setString(2, item.getName());
            statement.setInt(3, item.getUnitPrice());
            statement.setInt(4, item.getAmount());
            statement.executeUpdate();
        } catch (SQLException e) {
            throw new ReceiptDAOException("[ReceiptItemDAO]: Failed to save item", e);
        }
    }

    /**
     * Updates all items corresponding to receipt `id` by deleting all current such items in the database, and then
     * inserting `items`.
     */
    public void updateItems(long id, List<ReceiptItem> items) throws ReceiptDAOException {
        removeItems(id);
        saveItems(id, items);
    }

    /**
     * Deletes all items corresponding to receipt `id` in the database.
     */
    public void removeItems(long id) throws ReceiptDAOException {
        String sql = "DELETE FROM " + TABLE_NAME + " WHERE id = ?";

        try (PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setLong(1, id);
            statement.executeUpdate();
        } catch (SQLException e) {
            throw new ReceiptDAOException(
                    "[ReceiptItemDAO]: Failed to remove items of receipt #" + id, e);
        }
    }



    /* ~~~~~~~~~~~~~~~ First time setup methods ~~~~~~~~~~~~~~~ */

    void findOrCreateTable() throws ReceiptDAOException {
        if (!findTable()) {
            createTable();
        }
    }

    /**
     * Looks for a table (with a name as defined in this class).
     * @return true if a table was found, otherwise false
     */
    private boolean findTable() throws ReceiptDAOException {
        try {
            DatabaseMetaData metaData = connection.getMetaData();

            try (ResultSet tables = metaData.getTables(null, null, TABLE_NAME, null)) {
                return tables.next();
            }
        } catch (SQLException e) {
            throw new ReceiptDAOException("[ReceiptDAO]: Failed to check if table " + TABLE_NAME + " exists", e);
        }
    }

    /**
     * Creates a table in accordance with this class' specified name.
     */
    private void createTable() throws ReceiptDAOException {
        String sql = """
                CREATE TABLE %s(
                    id         BIGINT  NOT NULL,
                    name       VARCHAR NOT NULL,
                    unit_price INT     NOT NULL CHECK (unit_price > 0),
                    amount     INT     NOT NULL CHECK (amount >= 0),
                    PRIMARY KEY(id, name),
                    FOREIGN KEY(id) REFERENCES %s
                )
                """.formatted(TABLE_NAME, ReceiptDAO.TABLE_NAME);
        try (Statement statement = connection.createStatement()) {
            statement.addBatch(sql);
            statement.executeBatch();
            System.out.println("[ReceiptItemDAO]: Created table " + TABLE_NAME);
        } catch (SQLException e) {
            throw new ReceiptDAOException("[ReceiptItemDAO]: Failed to create table " + TABLE_NAME, e);
        }
    }
}
