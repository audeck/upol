package cz.upol.jj2.JavaFX;

import cz.upol.jj2.Ciphers.CaesarCipher;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.stage.Stage;

import java.util.Optional;



public class CipherApplicationFX extends Application {
  private CaesarCipher cipher;
  private TextArea txtEncipher;
  private TextArea txtDecipher;



  @Override
  public void start(Stage stage) {
    stage.setTitle("Caesar cipher");
    this.cipher = new CaesarCipher();

    MenuBar menuBar = createMenuBar();
    HBox content = createContentPanel();

    BorderPane root = new BorderPane();
    root.setTop(menuBar);
    root.setCenter(content);

    stage.setScene(new Scene(root, 480, 270));
    stage.show();
  }



  /**
   * Creates a menu bar of the following hierarchy:
   * <p>
   * <p>
   * <p>"File"
   * <p>|- "Encipher"
   * <p>|- "Decipher"
   * <p>|- "Exit"
   * <p>
   *
   * @return a menu bar object
   */
  private MenuBar createMenuBar() {
    // File->Encipher item
    MenuItem mnuEncipher = new MenuItem("_Encipher");
    mnuEncipher.setAccelerator(new KeyCodeCombination(KeyCode.E, KeyCombination.ALT_DOWN));
    mnuEncipher.setOnAction(this::encipherAction);

    // File->Decipher item
    MenuItem mnuDecipher = new MenuItem("_Decipher");
    mnuDecipher.setAccelerator(new KeyCodeCombination(KeyCode.D, KeyCombination.ALT_DOWN));
    mnuDecipher.setOnAction(this::decipherAction);

    // File->Exit item
    MenuItem mnuExit = new MenuItem("E_xit");
    mnuExit.setAccelerator(new KeyCodeCombination(KeyCode.X, KeyCombination.ALT_DOWN));
    mnuExit.setOnAction(e -> Platform.exit());

    // File tab
    Menu menuFile = new Menu("_File");
    menuFile.getItems().addAll(mnuEncipher, mnuDecipher, mnuExit);

    return new MenuBar(menuFile);
  }



  /**
   * Creates a content panel of the following panel layout:
   * <p>
   * <p>
   * <p> Encipher_panel | Options_panel | Decipher_panel
   * <p>
   *
   * @return a content panel object
   */
  private HBox createContentPanel() {
    // Create content panel
    HBox contentPanel = new HBox(3);
    contentPanel.setAlignment(Pos.CENTER);
    contentPanel.setSpacing(16);

    // Add children
    contentPanel.getChildren().addAll(
            createEncipherPanel(),
            createOptionsPanel(),
            createDecipherPanel()
    );

    return contentPanel;
  }



  /**
   * Creates an "encipher" panel of the following layout:
   * <p>
   * <p>
   * <p>  Encipher_text_area
   * <p>  ------------------
   * <p>  Encipher_button
   * <p>
   *
   * @return an encipher panel object
   */
  private VBox createEncipherPanel() {
    // Create encipher panel
    VBox encipherPanel = new VBox(3);
    encipherPanel.setAlignment(Pos.CENTER);

    // Create children
    Text lbEncipher = new Text("Enter text to encipher below");
    this.txtEncipher = new TextArea();
    this.txtEncipher.setWrapText(true);
    Button btnEncipher = new Button("Encipher!");
    btnEncipher.setOnAction(this::encipherAction);

    // Add children
    encipherPanel.getChildren().addAll(lbEncipher, txtEncipher, btnEncipher);

    return encipherPanel;
  }



  /**
   * Creates a "cipher options" panel of the following layout:
   * <p>
   * <p>
   * <p>  Shift_label
   * <p>  ------------
   * <p>  Shift_button
   * <p>
   *
   * @return an options panel object
   */
  private VBox createOptionsPanel() {
    // Create options panel
    VBox optionsPanel = new VBox(2);
    optionsPanel.setAlignment(Pos.CENTER);

    // Create children
    Text lbShift = new Text("Shifting by: " + this.cipher.getLeftShift());
    Button btnShift = new Button("Adjust shift");
    btnShift.setOnAction(e -> {
      TextInputDialog inputDialog = new TextInputDialog();
      inputDialog.setContentText("Enter a new LEFT shift: ");
      Optional<String> result = inputDialog.showAndWait();

      if (result.isPresent() && result.get().length() > 0) {
        try {
          int newShift = Integer.parseInt(result.get());
          this.cipher.setLeftShift(newShift);
          lbShift.setText("Shifting by: " + this.cipher.getLeftShift());
        }
        catch (NumberFormatException ignored) {
          Alert alert = new Alert(Alert.AlertType.NONE);
          alert.setContentText("Invalid shift value (must be an integer!)");
          alert.getButtonTypes().add(ButtonType.OK);
          alert.show();
        }
      }
    });

    // Add children
    optionsPanel.getChildren().addAll(lbShift, btnShift);

    return optionsPanel;
  }



  /**
   * Creates a "decipher" panel of the following layout:
   * <p>
   * <p>
   * <p>  Decipher_text_area
   * <p>  -------------------
   * <p>  Decipher_button
   * <p>
   *
   * @return a decipher panel object
   */
  private VBox createDecipherPanel() {
    // Create decipher panel
    VBox decipherPanel = new VBox(3);
    decipherPanel.setAlignment(Pos.CENTER);

    // Create children
    Text lbDecipher = new Text("Enter text to decipher below");
    this.txtDecipher = new TextArea();
    this.txtDecipher.setWrapText(true);
    Button btnDecipher = new Button("Decipher!");
    btnDecipher.setOnAction(this::decipherAction);

    // Add children
    decipherPanel.getChildren().addAll(lbDecipher, txtDecipher, btnDecipher);

    return decipherPanel;
  }



  /** Encipher action wrapper */
  private void encipherAction(ActionEvent ignored) {
    cipherAction(this.txtEncipher, this.txtDecipher);
  }



  /** Decipher action wrapper */
  private void decipherAction(ActionEvent ignored) {
    cipherAction(this.txtDecipher, this.txtEncipher);
  }



  /**
   * Sets the text of `input` to the cipher of the text of `output`.
   *
   * @param input input text area
   * @param output output text area
   */
  private void cipherAction(TextArea input, TextArea output) {
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



  public static void main(String[] args) {
    launch(args);
  }
}
