package cz.upol.jj2.swing;

import cz.upol.jj2.utils.Sequence;
import cz.upol.jj2.utils.SequenceCalculator;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.math.BigInteger;

/**
 * Defines the main window for this application.
 */
public class SequenceWindow extends JFrame {
    private static final int COMMON_MARGIN = 5;
    private static final String STATUS_PREFIX = "Calculation status: ";

    private SequenceCalculator sequenceCalculator;
    private Sequence currentSequence;
    private CalcWorker calcWorker;

    private JComboBox<String> sequenceTypeComboBox;
    private JTextField sequenceNumberTextField;
    private JButton calculateButton;
    private JButton cancelButton;
    private JLabel statusLabel;
    private JList<String> resultsList;

    public SequenceWindow() {
        this.sequenceCalculator = new SequenceCalculator();
        this.currentSequence = Sequence.FIBONACCI;

        this.setTitle("Generalized Fibonacci sequences");
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        this.setPreferredSize(new Dimension(520, 390));

        this.setContentPane(createMainPanel());

        this.pack();
    }

    /* ---------------------------------------------------------------- */

    /**
     * Creates and returns the window's main panel.
     * Layout:
     * inputPanel
     * ------------
     * actionPanel
     * ------------
     * resultsPanel
     */
    private JPanel createMainPanel() {
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.setBorder(new EmptyBorder(COMMON_MARGIN, COMMON_MARGIN, COMMON_MARGIN, COMMON_MARGIN));

        JPanel inputPanel = createInputPanel();
        JPanel actionPanel = createActionPanel();
        JPanel resultsPanel = createResultsPanel();

        mainPanel.add(inputPanel, BorderLayout.NORTH);
        mainPanel.add(actionPanel, BorderLayout.CENTER);
        mainPanel.add(resultsPanel, BorderLayout.SOUTH);

        return mainPanel;
    }

    /* ---------------------------------------------------------------- */

    /**
     * Creates and returns an input panel.
     * Layout:
     * typeLabel   |    sequenceTypeComboBox
     * -------------------------------------
     * numberLabel | sequenceNumberTextField
     */
    private JPanel createInputPanel() {
        JPanel panel = new JPanel(new GridLayout(2, 2, COMMON_MARGIN, COMMON_MARGIN));

        JLabel typeLabel = new JLabel("Select a sequence:");
        sequenceTypeComboBox = new JComboBox<>(Sequence.getAllNames());
        sequenceTypeComboBox.setSelectedIndex(currentSequence.ordinal());
        sequenceTypeComboBox.addActionListener(this::changeSequenceAction);

        JLabel numberLabel = new JLabel("Enter a number's index:");
        sequenceNumberTextField = new JTextField();

        panel.add(typeLabel);
        panel.add(sequenceTypeComboBox);
        panel.add(numberLabel);
        panel.add(sequenceNumberTextField);

        return panel;
    }

    /* ---------------------------------------------------------------- */

    /**
     * Creates and returns an action panel.
     * Layout:
     * calculateButton | cancelButton
     * ------------------------------
     * statusLabel
     */
    private JPanel createActionPanel() {
        // Action panel
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

        // Buttons
        JPanel buttons = new JPanel();
        buttons.setLayout(new BoxLayout(buttons, BoxLayout.X_AXIS));

        calculateButton = new JButton("Calculate");
        calculateButton.addActionListener(this::calculateAction);
        cancelButton = new JButton("Cancel");
        cancelButton.setEnabled(false);
        cancelButton.addActionListener(this::cancelAction);

        buttons.add(Box.createHorizontalGlue());
        buttons.add(calculateButton);
        buttons.add(Box.createRigidArea(new Dimension(COMMON_MARGIN, 0)));
        buttons.add(cancelButton);
        buttons.add(Box.createHorizontalGlue());

        // Status
        JPanel status = new JPanel();
        status.setLayout(new BoxLayout(status, BoxLayout.X_AXIS));

        statusLabel = new JLabel(STATUS_PREFIX + "-");

        status.add(Box.createHorizontalGlue());
        status.add(statusLabel);
        status.add(Box.createHorizontalGlue());

        // Action panel
        panel.add(Box.createVerticalGlue());
        panel.add(buttons);
        panel.add(Box.createRigidArea(new Dimension(0, COMMON_MARGIN)));
        panel.add(status);
        panel.add(Box.createVerticalGlue());

        return panel;
    }

    /* ---------------------------------------------------------------- */

    /**
     * Creates and returns a results panel.
     * Layout:
     * resultsLabel
     * --------------------
     * resultsScrollingPane
     */
    private JPanel createResultsPanel() {
        JPanel panel = new JPanel(new BorderLayout());

        resultsList = new JList<>(new DefaultListModel<>());
        JScrollPane scrollPane = new JScrollPane(resultsList);

        JLabel resultsLabel = new JLabel("Results");
        resultsLabel.setHorizontalAlignment(SwingConstants.CENTER);

        panel.add(resultsLabel, BorderLayout.NORTH);
        panel.add(scrollPane, BorderLayout.CENTER);

        return panel;
    }

    /* ---------------------------------------------------------------- */

    /**
     * Called upon pressing the `calculateButton`.
     */
    private void calculateAction(ActionEvent ignored) {
        int arg;

        // Format check
        try {
            arg = Integer.parseInt(sequenceNumberTextField.getText());
        } catch (NumberFormatException e) {
            JOptionPane.showMessageDialog(this, "Invalid sequence number index (must be an integer)!");
            return;
        }

        // Worker
        calcWorker = new CalcWorker(arg);
        calcWorker.execute();

        // Update & enable/disable elements
        sequenceTypeComboBox.setEnabled(false);
        sequenceNumberTextField.setEnabled(false);
        calculateButton.setEnabled(false);
        cancelButton.setEnabled(true);
        statusLabel.setText(STATUS_PREFIX + "RUNNING");
    }

    /* ---------------------------------------------------------------- */

    /**
     * Called upon pressing the `cancelButton`.
     */
    private void cancelAction(ActionEvent ignored) {
        calcWorker.cancel(true);
    }

    /* ---------------------------------------------------------------- */

    /**
     * Changes `currentSequence` according to ComboBox.
     */
    private void changeSequenceAction(ActionEvent ignored) {
        currentSequence = Sequence.fromName((String) sequenceTypeComboBox.getSelectedItem());
    }

    /* ---------------------------------------------------------------- */

    /**
     * Called after the current worker thread finishes.
     */
    private void displayResult(int index, BigInteger value) {
        DefaultListModel<String> listModel = (DefaultListModel<String>) resultsList.getModel();
        listModel.addElement(currentSequence.getName() + "(" + index + ") = " + value);

        // Enable/disable elements
        sequenceTypeComboBox.setEnabled(true);
        sequenceNumberTextField.setEnabled(true);
        calculateButton.setEnabled(true);
        cancelButton.setEnabled(false);
        statusLabel.setText(STATUS_PREFIX + "FINISHED");
    }

    /* ---------------------------------------------------------------- */

    /**
     * Called after the current worker thread gets cancelled without finishing.
     */
    private void displayCancel() {
        // Enable/disable elements
        sequenceTypeComboBox.setEnabled(true);
        sequenceNumberTextField.setEnabled(true);
        calculateButton.setEnabled(true);
        cancelButton.setEnabled(false);
        statusLabel.setText(STATUS_PREFIX + "CANCELLED");
    }

    /* ---------------------------------------------------------------- */

    /**
     * Single thread worker class for `sequenceCalculator` calculations.
     */
    private class CalcWorker extends SwingWorker<BigInteger, Void> {
        private final int arg;

        public CalcWorker(int arg) {
            this.arg = arg;
        }

        @Override
        protected BigInteger doInBackground() {
            return sequenceCalculator.getNumberAtIndex(this, currentSequence, arg);
        }

        @Override
        protected void done() {
            try {
                if (this.isCancelled()) displayCancel();
                else displayResult(arg, this.get());
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}
