/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.buffalo.cse.sneps3.gui;

/**
 *
 * @author dan
 */
public class Slot {
    String name;
    String type;

    String posadjust;
    String negadjust;

    int min;
    int max;

    public Slot(String n, String t, String pos, String neg, int min, int max){
        name = n;
        type = t;
        posadjust = pos;
        negadjust = neg;
        this.min = min;
        this.max = max;

        //System.out.println("Slot: " + n + " min: " + min + " max: " + max);
    }

    @Override
    public String toString(){
        return name;
    }
}
