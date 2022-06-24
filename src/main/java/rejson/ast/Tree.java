package rejson.ast;

import java.util.*;

public class Tree {

    public static class JsValWithId {
        final String identity;
        final JsVal value;

        public JsValWithId(String identity, JsVal value) {
            this.identity = identity;
            this.value = value;
        }

        public JsValWithId identity(String id){
            return new JsValWithId(id, this.value);
        }

        @Override
        public String toString() {
            return "JsValWithId{" +
                    "identity='" + identity + '\'' +
                    ", value=" + value +
                    '}';
        }
    }

    public static interface JsVal {
    }

    public static class JsBoolean implements JsVal {
        final boolean value;
        public JsBoolean(boolean value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return "JsBoolean{" +
                    "value=" + value +
                    '}';
        }
    }
    public static class JsString implements JsVal {
        final String value;
        public JsString(String value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return "JsString{" +
                    "value='" + value + '\'' +
                    '}';
        }
    }
    public static class JsNumber implements JsVal {
        final double value;
        public JsNumber(double value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return "JsNumber{" +
                    "value=" + value +
                    '}';
        }
    }
    public static class JsNull implements JsVal {
        @Override
        public String toString() {
            return "null";
        }
    }
    public static class JsArr implements  JsVal {
        final List<JsValWithId> value = new ArrayList<>();
        public JsArr(List<JsValWithId> value) {
            this.value.addAll(value);
        }

        @Override
        public String toString() {
            return "JsArr{" +
                    "value=" + value +
                    '}';
        }
    }
    public static class JsAny implements JsVal {
        final String groundType;
        public JsAny(String groundType) {
            this.groundType = groundType;
        }
    }
    public static class JsObj implements JsVal {
        final Map<String, JsValWithId> value = new HashMap<>();
        public JsObj(Map<String, JsValWithId> value) {
            this.value.putAll(value);
        }

        @Override
        public String toString() {
            return "JsObj{" +
                    "value=" + value +
                    '}';
        }
    }

    public static <T> List<T> append(List<T> list, T element) {
        ArrayList<T> result = new ArrayList<>();
        result.addAll(list);
        result.add(element);
        return result;
    }

    public static <K,V>Map<K,V> append(Map<K,V> map, Map<K,V> adds) {
        map.putAll(adds);
        return map;
    }

    public static <K,V>Map<K,V> append(Map<K,V> map, K k, V v) {
        map.put(k,v);
        return map;
    }

}
