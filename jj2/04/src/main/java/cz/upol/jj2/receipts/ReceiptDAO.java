package cz.upol.jj2.receipts;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

/**
 * This class implements database object functionality for handling (querying, saving, updating, and removing) Receipt
 * objects and the table that holds their information. Database implementation is PostgreSQL, with the host and database
 * name being defined by `DB_HOST` and `DB_NAME` respectively. Login credentials are obtained upon instantiation.
 * <p>
 * The table objects of this class create and use could be created with the following expression:
 * <p>
 *      CREATE TABLE `TABLE_NAME`(
 *          id   BIGINT  PRIMARY KEY,
 *          name VARCHAR NOT NULL,
 *          itin VARCHAR NOT NULL
 *      );
 */
public class ReceiptDAO implements AutoCloseable {

    protected static final String DB_HOST = "localhost:5432";
    protected static final String DB_NAME = "receipts";
    protected static final String TABLE_NAME = "receipts";

    private final ReceiptItemDAO receiptItemDAO;
    private final Connection connection;

    /**
     * Instantiates a database object. Also creates a database and table under names specified by this class
     * if needed.
     */
    public ReceiptDAO(String username, String password) throws ReceiptDAOException {
        findOrCreateDatabase(username, password);
        String connectionURL = "jdbc:postgresql://" + DB_HOST + "/" + DB_NAME;

        try {
            this.connection = DriverManager.getConnection(connectionURL, username, password);
            System.out.println("[ReceiptDAO]: Connected to: " + connectionURL);
        } catch (SQLException e) {
            throw new ReceiptDAOException(
                    "[ReceiptDAO]: Failed while connecting to: " + connectionURL, e);
        }

        receiptItemDAO = new ReceiptItemDAO(this.connection);

        findOrCreateTable();
        receiptItemDAO.findOrCreateTable();
    }

    @Override
    public void close() throws ReceiptDAOException {
        try {
            connection.close();
        } catch (SQLException e) {
            throw new ReceiptDAOException("[ReceiptDAO]: Failed to close connection", e);
        }
    }



    /* ~~~~~~~~~~~~~~~ Database methods ~~~~~~~~~~~~~~~ */

    /**
     * Gets all receipt ids that correspond a person's ITIN.
     */
    public List<Long> getIdsByItin(String itin) throws ReceiptDAOException {
        List<Long> ids = new ArrayList<>();
        String sql = "SELECT id FROM " + TABLE_NAME + " WHERE itin = ?";

        try (PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setString(1, itin);

            try (ResultSet results = statement.executeQuery()) {
                while (results.next()) {
                    ids.add(results.getLong("id"));
                }
            }
        } catch (SQLException e) {
            throw new ReceiptDAOException(
                    "[ReceiptDAO]: Unable to get all ids with itin " + itin, e);
        }

        return ids;
    }

    /**
     * Queries the database for a receipt's data and returns a Receipt object according to it.
     */
    public Receipt getReceipt(long id) throws ReceiptDAOException {
        Receipt receipt = null;
        String sql = "SELECT * FROM " + TABLE_NAME + " WHERE id = ?";

        try (PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setLong(1, id);

            try (ResultSet results = statement.executeQuery()) {
                if (results.next()) {
                    receipt = new Receipt(
                            id,
                            results.getString("name"),
                            results.getString("itin"),
                            receiptItemDAO.getItems(id)
                    );
                }
            }
        } catch (SQLException e) {
            throw new ReceiptDAOException("[ReceiptDAO]: Unable to get receipt #" + id, e);
        }

        return receipt;
    }

    /**
     * Returns a list of all Receipt objects corresponding to a person's ITIN
     */
    public List<Receipt> getReceiptsByItin(String itin) throws ReceiptDAOException {
        List<Long> ids = getIdsByItin(itin);
        List<Receipt> receipts = new ArrayList<>();

        for (long id : ids) {
            receipts.add(getReceipt(id));
        }

        return receipts;
    }

    /**
     * Inserts a receipt's data into the database.
     */
    public void saveReceipt(Receipt receipt) throws ReceiptDAOException {
        String sql = "INSERT INTO " + TABLE_NAME + " (id, name, itin) VALUES (?, ?, ?)";

        try (PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setLong(1, receipt.getId());
            statement.setString(2, receipt.getName());
            statement.setString(3, receipt.getItin());
            statement.executeUpdate();

            receiptItemDAO.saveItems(receipt.getId(), receipt.getItems());
        } catch (SQLException e) {
            throw new ReceiptDAOException(
                    "[ReceiptDAO]: Failed to save receipt #" + receipt.getId(), e);
        }
    }

    /**
     * Updates a receipt (based on its id).
     */
    public void updateReceipt(Receipt receipt) throws ReceiptDAOException {
        String sql = "UPDATE " + TABLE_NAME + " SET name = ?, itin = ? WHERE id = ?";

        try (PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setString(1, receipt.getName());
            statement.setString(2, receipt.getItin());
            statement.setLong(3, receipt.getId());
            statement.executeUpdate();

            receiptItemDAO.updateItems(receipt.getId(), receipt.getItems());
        } catch (SQLException e) {
            throw new ReceiptDAOException(
                    "[ReceiptDAO]: Failed to update receipt #" + receipt.getId(), e);
        }
    }

    /**
     * Saves or updates a receipt (based on its id). Dangerous!
     */
    public void saveOrUpdateReceipt(Receipt receipt) throws ReceiptDAOException {
        if ((getReceipt(receipt.getId()) == null)) {
            saveReceipt(receipt);
        } else {
            updateReceipt(receipt);
        }
    }

    /**
     * Deletes a receipt from the database. Checks only the receipt's id(!).
     */
    public void removeReceipt(Receipt receipt) throws ReceiptDAOException {
        String sql = "DELETE FROM " + TABLE_NAME + " WHERE id = ?";

        try (PreparedStatement statement = connection.prepareStatement(sql)) {
            receiptItemDAO.removeItems(receipt.getId());

            statement.setLong(1, receipt.getId());
            statement.executeUpdate();
        } catch (SQLException e) {
            throw new ReceiptDAOException(
                    "[ReceiptDAO]: Failed to remove receipt #" + receipt.getItems(), e);
        }
    }



    /* ~~~~~~~~~~~~~~~ First time setup methods ~~~~~~~~~~~~~~~ */

    /*
     * NOTE: Pro real-world-use-case nedavaji nasledujici metody smysl (jelikoz jde velice jednoduse zajistit, ze
     *       databaze a tabulky budou existovat), ale pro prenosnost bez problemu je implementuji; tj. staci zajistit,
     *       ze localhost:5432 (popr. jakakoliv predefinovana host url) je validni PostgreSQL server (doufam).
     */

    private void findOrCreateTable() throws ReceiptDAOException {
        if (!findTable()) {
            createTable();
        }
    }

    /**
     * Looks for a table in the current database.
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
     * Creates a table named according to this class.
     */
    private void createTable() throws ReceiptDAOException {
        String sql = """
                CREATE TABLE %s(
                    id   BIGINT  PRIMARY KEY,
                    name VARCHAR NOT NULL,
                    itin VARCHAR NOT NULL
                )
                """.formatted(TABLE_NAME);
        try (Statement statement = connection.createStatement()) {
            statement.executeUpdate(sql);
            System.out.println("[ReceiptDAO]: Created table " + TABLE_NAME);
        } catch (SQLException e) {
            throw new ReceiptDAOException("[ReceiptDAO]: Failed to create table " + TABLE_NAME, e);
        }
    }

    private void findOrCreateDatabase(String username, String password) throws ReceiptDAOException {
        String url = "jdbc:postgresql://" + DB_HOST + "/postgres";

        try (Connection connection = DriverManager.getConnection(url, username, password)) {
            if (!findDatabase(connection)) {
                createDatabase(connection);
            }
        } catch (SQLException e) {
            throw new ReceiptDAOException("[ReceiptDAO]: Failed to connect to postgres main", e);
        }
    }

    /**
     * Looks for a database in PSQL's catalog table.
     * @param connection connection to psql's main `postgres` database
     * @return true if a database was found, otherwise false
     */
    private boolean findDatabase(Connection connection) throws ReceiptDAOException {
        String sql = "SELECT 1 FROM pg_database WHERE datname = ?";

        try (PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setString(1, DB_NAME);

            try (ResultSet results = statement.executeQuery()) {
                return results.next();
            }
        } catch (SQLException e) {
            throw new ReceiptDAOException("[ReceiptDAO]: Failed to check if database " + DB_NAME + " exists", e);
        }
    }

    /**
     * Creates a new database in DB_HOST.
     */
    private void createDatabase(Connection connection) throws ReceiptDAOException {
        String sql = "CREATE DATABASE " + DB_NAME;

        try (Statement statement = connection.createStatement()) {
            statement.executeUpdate(sql);
            System.out.println("[ReceiptDAO]: Created database " + DB_NAME);
        } catch (SQLException e) {
            throw new ReceiptDAOException("[ReceiptDAO]: Failed to create database " + DB_NAME, e);
        }
    }
}
