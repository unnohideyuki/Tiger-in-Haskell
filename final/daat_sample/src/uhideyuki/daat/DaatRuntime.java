package uhideyuki.daat;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;

import uhideyuki.daat.R.id;
import uhideyuki.daat.DaatProg;

public class DaatRuntime extends Activity
{
    static TextView tv;

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        tv = (TextView) findViewById(id.tv_main);

        Object[] arcd = new Object[1024];
        Integer fp = new Integer(-1);

        DaatProg.main(arcd, fp);
    }

    public static void print(String s)
    {
        tv.append(s);
    }
}
