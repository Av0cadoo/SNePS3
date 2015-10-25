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
public class SemanticType {
    ArrayList<SemanticType> parents = new ArrayList<SemanticType>();
    String name;

    public SemanticType(String name, ArrayList<SemanticType> parents){
        this.name = name;
        this.parents = parents;
    }

    public ArrayList<SemanticType> getParent(){
        return parents;
    }

    public boolean hasParent(SemanticType p){
        for(SemanticType parent : parents){
            if(parent == p || parent.hasParent(p)) return true;
        }
        return false;
    }

    public String getName(){
        return name;
    }

    @Override
    public String toString(){
        return name;
    }
}
