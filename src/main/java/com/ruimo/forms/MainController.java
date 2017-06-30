package com.ruimo.forms;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.ObservableSet;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.ListView;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

public class MainController implements Initializable {
    ImageTable imageTable = new ImageTable();
    Stage stage;

    void setStage(Stage stage) {
        this.stage = stage;
        stage.setWidth(1024);
        stage.setHeight(800);
    }

    @FXML
    ListView imageListView;

    @FXML
    ImageView imageView;

    @FXML
    public void exitMenuClicked(ActionEvent event) {
        System.err.println("exitMenuClicked()");
        Platform.exit();
    }

    @FXML
    public void imageOpenMenuClicked(ActionEvent event) {
        FileChooser fc = new FileChooser();
        fc.setTitle("Select image file");
        fc.getExtensionFilters().addAll(new FileChooser.ExtensionFilter("Image Files", "*.png"));
        List<File> files = fc.showOpenMultipleDialog(stage);
        if (files != null) {
            imageTable.addFiles(files);
        }
    }

    @FXML
    public void imageSelected(MouseEvent e) {
        File file = (File)((ListView)e.getSource()).getSelectionModel().getSelectedItem();
        System.err.println("Image selected " + file);
        Image img = new Image(file.toURI().toString());
        imageView.setImage(img);
    }

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        imageTable.addChangeListener(new ImageTableListener() {
            @Override
            public void onFilesChanged(Set<File> before, Set<File> after) {
                ObservableList<File> items = FXCollections.observableList(new ArrayList(after));
                imageListView.setItems(items);
            }
        });
    }
}
