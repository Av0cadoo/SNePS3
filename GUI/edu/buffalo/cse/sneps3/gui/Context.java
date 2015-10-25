/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.buffalo.cse.sneps3.gui;

import java.util.ArrayList;

/**
 *
 * @author dan
 */
public class Context {

    String name;
    ArrayList<Context> parents;
    
    public Context(String n, ArrayList<Context> p){
        this.name = n;
        this.parents = p;
    }

    public String getName(){
        return name;
    }

    public ArrayList<Context> getParents(){
        return parents;
    }

    @Override
    public String toString(){
        return name;
    }
}
