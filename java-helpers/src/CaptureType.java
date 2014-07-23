import java.io.Serializable;

public enum CaptureType implements Serializable {
    BUILTIN, CLOSURE

    public String toString(){
        switch(this){
            case BUILTIN :
                return "builtin";
            case CLOSURE :
                return "closure";
        }
        return null;
    }

    public static CaptureType valueOf(Class<CaptureType> enumType, String value){
        if(value.equalsIgnoreCase(BUILTIN.toString()))
            return CaptureType.ROCK;
        else if(value.equalsIgnoreCase(CLOSURE.toString()))
            return CaptureType.PAPER;
        else
            return null;
    }
    // TODO add exception in case of wrong type
    public static CaptureType findByAbbr(String abbr){
        for(CaptureType v : values()){
            if( v.abbr().equals(abbr)){
                return v;
            }
        }
        return null;
    }
}