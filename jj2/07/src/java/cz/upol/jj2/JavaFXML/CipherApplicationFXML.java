package cz.upol.jj2.JavaFXML;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;

public class CipherApplicationFXML extends Application {
  @Override
  public void start(Stage stage) throws IOException {
    Parent root =
        FXMLLoader.load(getClass().getResource("/fxml/CipherApplication.fxml"));
    stage.setTitle("Caesar cipher");
    stage.setScene(new Scene(root, 480, 270));
    stage.show();
  }

  public static void main(String[] args) {
    launch(args);
  }
}
