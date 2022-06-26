package wjson.patterns.ast;

import java.util.*;

public class Tree {

    public interface Pattern {
        default PatternWithName bind(String name) {
            return new PatternWithName(name, this);
        }
    }
    public static class PatternWithName {
        public final String name;
        public final Pattern pattern;

        public PatternWithName(String name, Pattern pattern) {
            this.name = name;
            this.pattern = pattern;
        }

        @Override
        public String toString() {
            return "PatternWithName{" + "name='" + name + '\'' + ", pattern=" + pattern + '}';
        }
    }
    public static class BooleanPattern implements Pattern {
        public final boolean value;
        public BooleanPattern(boolean value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return "BooleanPattern{" +  value + '}';
        }
    }
    public static class StringPattern implements Pattern {
        public final String value;
        public StringPattern(String value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return "StringPattern{'" + value + "\'}";
        }
    }
    public static class NumberPattern implements Pattern {
        public final double value;
        public NumberPattern(double value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return "JsNumber{" + value + '}';
        }
    }
    public static class NullPattern implements Pattern {
        @Override
        public String toString() {
            return "NullPattern";
        }
    }
    public static class ArrayPattern implements  Pattern {
        public final List<PatternWithName> value = new ArrayList<>();
        public ArrayPattern(List<PatternWithName> value) {
            this.value.addAll(value);
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("ArrayPattern{");
            for(PatternWithName p : value) {
                sb.append(p.toString());
                sb.append(",");
            }
            sb.append("}");
            return sb.toString();
        }
    }
    public static class AnyVal implements Pattern {
        public final String groundType; // boolean, number, string, any
        public AnyVal(String groundType) {
            this.groundType = groundType;
        }

        @Override
        public String toString() {
            return "AnyVal{" +  groundType + '}';
        }
    }

    public static class TagedString implements Pattern {
        public final String tag;
        public final String value;
        public TagedString(String tag, String value) {
            this.tag = tag;
            this.value = value;
        }

        @Override
        public String toString() {
            return "TagedString{" +  tag + ":" + value + '}';
        }
    }

    public static class AnyVals implements Pattern {
        @Override
        public String toString() {
            return "AnyVals";
        }
    }
    public static class ObjPattern implements Pattern {
        public final Map<String, PatternWithName> value = new HashMap<>();
        public ObjPattern(Map<String, PatternWithName> value) {
            this.value.putAll(value);
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("ObjPattern{");
            for(Map.Entry<String, PatternWithName> e : value.entrySet()) {
                sb.append(e.getKey());
                sb.append(":");
                sb.append(e.getValue().toString());
                sb.append(",");
            }
            sb.append("}");
            return sb.toString();
        }
    }

    public static class Field {
        public final String name;
        public final PatternWithName pattern;
        public Field(String name, PatternWithName pattern) {
            this.name = name;
            this.pattern = pattern;
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
