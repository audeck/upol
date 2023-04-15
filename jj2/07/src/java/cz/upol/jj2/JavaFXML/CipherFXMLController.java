package cz.upol.jj2.JavaFXML;

import cz.upol.jj2.Ciphers.CaesarCipher;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonType;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextInputDialog;
import javafx.scene.text.Text;

import java.util.Optional;

public class CipherFXMLController {
  private final CaesarCipher cipher = new CaesarCipher();
  @FXML private Text lblShift;
  @FXML private TextArea txtEncipher;
  @FXML private TextArea txtDecipher;

  @FXML
  public void changeShiftAction() {
    TextInputDialog inputDialog = new TextInputDialog();
    inputDialog.setContentText("Enter a new LEFT shift: ");
    Optional<String> result = inputDialog.showAndWait();

    if (result.isPresent() && result.get().length() > 0) {
      try {
        int newShift = Integer.parseInt(result.get());
        this.cipher.setLeftShift(newShift);
        lblShift.setText("Shifting by: " + this.cipher.getLeftShift());
      }
      catch (NumberFormatException ignored) {
        Alert alert = new Alert(Alert.AlertType.NONE);
        alert.setContentText("Invalid shift value (must be an integer!)");
        alert.getButtonTypes().add(ButtonType.OK);
        alert.show();
      }
    }
  }

  @FXML public void encipherAction() {
    cipherAction(txtEncipher, txtDecipher);
  }

  @FXML public void decipherAction() {
    cipherAction(txtDecipher, txtEncipher);
  }

  @FXML private void cipherAction(TextArea input, TextArea output) {
    String inputText = input.getText();

    // Check if text of input is valid invalid
    if (!this.cipher.isValidText(inputText)) {
      // Show alert & return
      Alert alert = new Alert(Alert.AlertType.NONE);
      alert.setTitle("ERROR: Invalid cipher text");
      alert.setContentText("Cipher and decipher texts should only contain capital letters, spaces and full stops!");
      alert.getButtonTypes().add(ButtonType.OK);
      alert.show();

      return;
    }

    output.setText(this.cipher.encipher(inputText));
  }

  @FXML private void exitAction() {
    Platform.exit();
  }
}
