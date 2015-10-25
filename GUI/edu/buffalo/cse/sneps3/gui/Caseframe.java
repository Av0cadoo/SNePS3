/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.buffalo.cse.sneps3.gui;

import java.util.ArrayList;
import java.util.Vector;

/**
 *
 * @author dan
 */
public class Caseframe implements Comparable{
    String name;
    String type;
    ArrayList slotnames;
    ArrayList fsymbols;
    ArrayList<Slot> slots;
    //Do we need Vector<Term> terms too?

    public Caseframe(String n, String t, ArrayList s, ArrayList fs){
        name = n;
        slotnames = s;
        type = t;
        fsymbols = fs;

        slots = new ArrayList<Slot>();

        for(int i = 0; i < s.size(); i++){
            for(Slot sl : GUI2.model.slots){
                if(sl.name.equals(s.get(i))) slots.add(sl);
            }
        }

        if(GUI2.DEBUG) System.out.println("Created Caseframe " + n + " with internal Java slot ordering " + slots +  " and function symbols " + fsymbols);
    }

    public Caseframe(String n, String t){
        slotnames = new ArrayList();
        type = t;
        name = n;
    }

    public void addSlot(Slot s){
        slots.add(s);
    }

    @Override
    public String toString(){
        return name;
    }

    public int compareTo(Object t) {
        Caseframe c = (Caseframe)t;
        return this.toString().toLowerCase().compareTo(c.toString().toLowerCase());
    }

    public boolean hasFSymbols(){
        if(fsymbols != null && !fsymbols.isEmpty()) return true;
        return false;
    }
}
