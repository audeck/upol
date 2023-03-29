package cz.upol.jj2.swing;

import cz.upol.jj2.ciphers.CaesarCipher;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class CipherWindow extends JFrame {
    private static final int COMMON_MARGIN = 20;
    private static final int TEXT_PADDING = 6;

    private final CaesarCipher cipher;
    private JTextArea encipherField;
    private JTextArea decipherField;

    public CipherWindow() {
        this.cipher = new CaesarCipher();

        this.setTitle("Caesar's cipher");
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        this.setPreferredSize(new Dimension(800, 600));

        // Menu bar
        this.setJMenuBar(createMenuBar());

        // Main panel
        this.setContentPane(createMainPanel());

        this.pack();
    }

    /**
     * Creates a menu bar with the following hierarchy:
     * <p>
     * "File"
     * |- "Encipher"
     * |- "Decipher"
     * |- "Exit"
     *
     * @return the menu bar object
     */
    private JMenuBar createMenuBar() {
        // Menu bar
        JMenuBar menuBar = new JMenuBar();

        // Menu bar item "File"
        JMenu menuFile = new JMenu("File");
        menuFile.setMnemonic(KeyEvent.VK_F);

        // Menu item item "File" -> "Encipher"
        JMenuItem menuFileEncipher = new JMenuItem("Encipher");
        menuFileEncipher.setMnemonic(KeyEvent.VK_E);
        menuFileEncipher.addActionListener(this::onEncipherAction);

        // Menu item item "File" -> "Decipher"
        JMenuItem menuFileDecipher = new JMenuItem("Decipher");
        menuFileDecipher.setMnemonic(KeyEvent.VK_D);
        menuFileDecipher.addActionListener(this::onDecipherAction);

        // Menu item item "File" -> "Exit"
        JMenuItem menuFileExit = new JMenuItem("Exit");
        menuFileExit.setMnemonic(KeyEvent.VK_X);
        menuFileExit.addActionListener(this::onExitAction);

        // Menu hierarchy
        menuFile.add(menuFileEncipher);
        menuFile.add(menuFileDecipher);
        menuFile.add(menuFileExit);
        menuBar.add(menuFile);

        return menuBar;
    }

    /**
     * Creates a GridLayout(1x3) main panel of the following panel layout:
     * <p>
     * Encipher_panel | Options_panel | Decipher_panel
     *
     * @return the main panel object
     */
    private JPanel createMainPanel() {
        JPanel mainPanel = new JPanel();

        mainPanel.setBorder(new EmptyBorder(COMMON_MARGIN, COMMON_MARGIN, COMMON_MARGIN, COMMON_MARGIN));
        mainPanel.setLayout(new GridLayout(1, 3, 1, 0));

        mainPanel.add(createEncipherPanel());
        mainPanel.add(createOptionsPanel());
        mainPanel.add(createDecipherPanel());

        return mainPanel;
    }

    /**
     * Creates the "encipher" panel of the following layout:
     * <p>
     * Encipher_text_field
     * -------------------
     * Encipher_button
     *
     * @return the encipher panel object
     */
    private JPanel createEncipherPanel() {
        JPanel encipherPanel = new JPanel();
        encipherPanel.setLayout(new BoxLayout(encipherPanel, BoxLayout.Y_AXIS));

        encipherField = createTextArea("ENTER TEXT TO ENCIPHER");

        JButton encipherButton = new JButton("Encipher");
        encipherButton.addActionListener(this::onEncipherAction);
        encipherButton.setAlignmentX(Component.CENTER_ALIGNMENT);

        encipherPanel.add(encipherField);
        encipherPanel.add(Box.createRigidArea(new Dimension(0, COMMON_MARGIN)));
        encipherPanel.add(encipherButton);

        return encipherPanel;
    }

    /**
     * Creates the "cipher options" panel of the following layout:
     * <p>
     * Shift_label
     * ------------
     * Shift_button
     *
     * @return the options panel object
     */
    private JPanel createOptionsPanel() {
        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.Y_AXIS));

        JLabel shiftLabel = new JLabel("Shifting by: " + cipher.getLeftShift());
        shiftLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        JButton shiftButton = new JButton("Adjust shift");
        shiftButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        shiftButton.addActionListener(e -> {
            String input = JOptionPane.showInputDialog(this, "Please input a new LEFT shift:");
            int newShift;

            try {
                newShift = Integer.parseInt(input);
            } catch (NumberFormatException ex) {
                // Also happens if the number > Integer.MAX_VALUE obviously; functionality not required
                JOptionPane.showMessageDialog(this, "Invalid shift value (must be an integer)!");
                return;
            }

            cipher.setLeftShift(newShift);
            shiftLabel.setText("Shifting by: " + cipher.getLeftShift());
        });

        optionsPanel.add(Box.createVerticalGlue());
        optionsPanel.add(shiftLabel);
        optionsPanel.add(shiftButton);
        optionsPanel.add(Box.createVerticalGlue());

        return optionsPanel;
    }

    /**
     * Creates the "decipher" panel of the following layout:
     * <p>
     * Decipher_text_field
     * -------------------
     * Decipher_button
     *
     * @return the decipher panel object
     */
    private JPanel createDecipherPanel() {
        JPanel decipherPanel = new JPanel();
        decipherPanel.setLayout(new BoxLayout(decipherPanel, BoxLayout.Y_AXIS));

        decipherField = createTextArea("ENTER TEXT TO DECIPHER");

        JButton decipherButton = new JButton("Decipher");
        decipherButton.addActionListener(this::onDecipherAction);
        decipherButton.setAlignmentX(Component.CENTER_ALIGNMENT);

        decipherPanel.add(decipherField);
        decipherPanel.add(Box.createRigidArea(new Dimension(0, COMMON_MARGIN)));
        decipherPanel.add(decipherButton);

        return decipherPanel;
    }

    /**
     * Creates a default text area.
     *
     * @param defaultText text already displayed
     * @return the text area object
     */
    private JTextArea createTextArea(String defaultText) {
        JTextArea textArea = new JTextArea(defaultText);
        textArea.setLineWrap(true);
        textArea.setBorder(new EmptyBorder(TEXT_PADDING, TEXT_PADDING, TEXT_PADDING, TEXT_PADDING));

        return textArea;
    }

    /**
     * Changes the text of `decipherField` to the cipher of `encipherField`'s text.
     *
     * @param e ignored
     */
    private void onEncipherAction(ActionEvent e) {
        String encipherText = encipherField.getText();

        // Valid character check
        for (char c : encipherText.toCharArray()) {
            if (CaesarCipher.VALID_CHARACTERS.indexOf(c) == -1) {
                JOptionPane.showMessageDialog(
                        this,
                        "Invalid plain text (can only contain capital letters, spaces and full stops)!"
                );
                return;
            }
        }

        decipherField.setText(cipher.encipher(encipherText));
    }

    /**
     * Changes the text of `encipherField` to the cipher of `decipherField`'s text.
     *
     * @param e ignored
     */
    private void onDecipherAction(ActionEvent e) {
        String decipherText = decipherField.getText();

        // Valid character check
        for (char c : decipherText.toCharArray()) {
            if (CaesarCipher.VALID_CHARACTERS.indexOf(c) == -1) {
                JOptionPane.showMessageDialog(
                        this,
                        "Invalid cipher text (can only contain capital letters, spaces and full stops)!"
                );
                return;
            }
        }

        encipherField.setText(cipher.decipher(decipherField.getText()));
    }

    /**
     * Exits the window.
     *
     * @param e ignored
     */
    private void onExitAction(ActionEvent e) {
        setVisible(false);
        dispose();
    }
}
