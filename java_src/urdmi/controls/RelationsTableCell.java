package urdmi.controls;

import javafx.beans.value.ObservableValue;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.util.StringConverter;
import org.apache.commons.lang3.reflect.FieldUtils;

/**
 * Created by QAston on 2015-08-08.
 */
public class RelationsTableCell<S> extends TextFieldTableCell<S, String> {

    public RelationsTableCell(StringConverter<String> converter) {
        super(converter);
    }
    @Override
    public void startEdit() {
        try {
            TextField oldTextField = (TextField) FieldUtils.readField(this,"textField", true);
            super.startEdit();
            TextField newTextField = (TextField) FieldUtils.readField(this,"textField", true);
            if (oldTextField != newTextField) {
                newTextField.focusedProperty().addListener(
                        (ObservableValue<? extends Boolean> arg0,
                         Boolean arg1, Boolean arg2) -> {
                            if (!arg2) {
                                commitEdit(newTextField.getText());
                            }
                        });
            }
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
    }
}