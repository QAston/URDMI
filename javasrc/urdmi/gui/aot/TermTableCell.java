package urdmi.gui.aot;

import javafx.scene.control.Cell;
import javafx.util.converter.DefaultStringConverter;
import org.apache.commons.lang3.reflect.FieldUtils;

/**
 * (ns urdmi.gui.aot
 * (:import (javafx.util.converter DefaultStringConverter)
 * (org.apache.commons.lang3.reflect FieldUtils)
 * (javafx.scene.control TextField)
 * (clojure.lang IFn)))
 * <p>
 * #_(gen-class
 * :name "urdmi.gui.aot.TermTableCell"
 * :extends javafx.scene.control.cell.TextFieldTableCell
 * :state "editfn"
 * :init "init"
 * :exposes-methods {startEdit parentStartEdit}
 * :constructors {[Object] [javafx.util.StringConverter]}
 * :prefix "ttc-")
 * <p>
 * #_(defn ttc-init [start-edit-fn]
 * [[(DefaultStringConverter.)] start-edit-fn])
 * <p>
 * #_(defn ttc-startEdit [this]
 * (let [oldTextField (FieldUtils/readField this "textField", true)
 * _ (.parentStartEdit this)
 * ^TextField newTextField (FieldUtils/readField this "textField", true)]
 * ((.editfn this) this oldTextField newTextField)
 * ))
 */
public class TermTableCell<S> extends javafx.scene.control.cell.TextFieldTableCell<S,String> {
    public final clojure.lang.IFn ifn;

    public TermTableCell(clojure.lang.IFn ifn) {
        super(new DefaultStringConverter());
        this.ifn = ifn;
    }
    @Override public void startEdit() {
        try {
            Object oldTextField = FieldUtils.readField(this, "textField", true);
            super.startEdit();
            Object newTextField = FieldUtils.readField(this, "textField", true);
            ifn.invoke(this, oldTextField, newTextField);
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
    }

    public static Cell fact (clojure.lang.IFn ifn ) {
        return new urdmi.gui.aot.TermTableCell(ifn);
    }
}
