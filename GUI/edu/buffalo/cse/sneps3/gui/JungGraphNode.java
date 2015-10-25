/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.buffalo.cse.sneps3.gui;

import java.util.ArrayList;
import java.util.HashSet;

/**
 *
 * @author dan
 */
public class JungGraphNode implements Cloneable {

    String name;
    SemanticType type;
    double activation;
    Caseframe cf;
    boolean asserted;
    ArrayList<JungGraphEdge> upCableset = new ArrayList<JungGraphEdge>();
    ArrayList<JungGraphEdge> downCableset = new ArrayList<JungGraphEdge>();
    boolean upCSVisible = true;
    boolean dnCSVisible = true;
    public boolean visible = true;
    
    //Speeding up knowing if it's a wft node:
    private boolean wftStatusKnown = false;
    private boolean wftNode = false;

    public JungGraphNode(String name, SemanticType type, Caseframe cframe, boolean asserted, double act) {
        this.name = name;
        this.type = type;
        this.cf = cframe;
        this.activation = act;
        this.asserted = asserted;
        if (GUI2.DEBUG) {
            System.out.println("New Node Added: " + name + " of type " + (type != null ? type : "") + " with cf " + (cf != null ? cf : ""));
        }
    }

    //Debug only!
    public JungGraphNode(String name) {
        this.name = name;
    }

    public SemanticType getType() {
        return type;
    }

    public double getActivation() {
        return activation;
    }

    //TODO: Performance increase here - hash table, and write hash function
    //using name.
    public void addToUpCableset(JungGraphEdge n) {
        for (JungGraphEdge j : upCableset) {
            if (j.equals(n)) {
                return;
            }
        }
        upCableset.add(n);
    }

    public ArrayList<JungGraphEdge> getUpCableset() {
        return upCableset;
    }

    public void addToDownCableset(JungGraphEdge n) {
        for (JungGraphEdge j : downCableset) {
            if (j.equals(n)) {
                return;
            }
        }
        downCableset.add(n);
    }

    public ArrayList<JungGraphEdge> getDownCableset() {
        return downCableset;
    }

    @Override
    public String toString() {
        if (asserted) {
            return name + "!";
        }
        return name;
    }

    public String getName() {
        return name;
    }

    //Well, really, a wftnode always has a down cableset, but sneps3 allows this: (assert '(Isa (setof ) (setof)))
    //so this version is 'more correct.'
    public boolean isWftNode() {
        if(wftStatusKnown) return wftNode;

        if(cf != null && name.length() > 3 && name.substring(0, 3).equals("wft")){
            for(int i = 3; i < name.length(); i++){
                if(!Character.isDigit(name.charAt(i))){
                    wftStatusKnown = true; wftNode = false;
                    return false;
                }
                    
            }
        }
        else{
            wftStatusKnown = true; wftNode = false;
            return false;
        }
        wftStatusKnown = true; wftNode = true;
        return true;
    }


    public boolean isVisible() {
        return visible;
    }

    public boolean isDownCablesetVisible() {
        int noVis = 0;
        for (JungGraphEdge e : downCableset) {
            if (e.to.isVisible()) {
                noVis++;
            }
        }
        if (noVis == downCableset.size()) {
            return true;
        }
        return false;
    }

    public boolean isDownCablesetPartialVisible() {
        int noVis = 0;
        for (JungGraphEdge e : downCableset) {
            if (e.to.isVisible()) {
                noVis++;
            }
        }
        if (noVis > 0) {
            return true;
        }
        return false;
    }

    public int getDownCablesetVisibleCount() {
        int noVis = 0;
        for (JungGraphEdge e : downCableset) {
            if (e.to.isVisible()) {
                noVis++;
            }
        }
        return noVis;
    }

    public boolean isUpCablesetVisible() {
        int noVis = 0;
        for (JungGraphEdge e : upCableset) {
            if (e.from.isVisible()) {
                noVis++;
            }
        }
        if (noVis == upCableset.size()) {
            return true;
        }
        return false;
    }

    public boolean isUpCablesetPartialVisible() {
        int noVis = 0;
        for (JungGraphEdge e : upCableset) {
            if (e.from.isVisible()) {
                noVis++;
            }
        }
        if (noVis > 0) {
            return true;
        }
        return false;
    }

    public ArrayList<JungGraphEdge> getUpCablesetMinusFS(){
        ArrayList<JungGraphEdge> ret = new ArrayList<JungGraphEdge>();
        for (JungGraphEdge e : upCableset) {
            if(!e.from.isWftNode() || !e.from.cf.hasFSymbols() ||
                    !e.from.cf.slots.get(0).name.equals(e.toString()) || e.from.isVisible()){
            //if (!(e.from.isWftNode() && e.from.cf.hasFSymbols() && e.from.cf.slots.get(0).name.equals(e.toString()) && !e.from.isVisible())) {
                ret.add(e);
            }
        }
        if(GUI2.DEBUG) System.out.println("UP: " + ret);
        return ret;
    }

    public ArrayList<JungGraphEdge> getDownCablesetMinusFS(){
        ArrayList<JungGraphEdge> ret = new ArrayList<JungGraphEdge>();
        for (JungGraphEdge e : downCableset) {
            if (!(e.from.isWftNode() && e.from.cf.hasFSymbols() && e.from.cf.slotnames.get(0).equals(e.toString()) && !e.from.isVisible())) {
                ret.add(e);
            }
        }
        if(GUI2.DEBUG) System.out.println("DN: " + ret);
        return ret;
    }

    public int getUpCablesetVisibleCount() {
        int noVis = 0;
        for (JungGraphEdge e : upCableset) {
            if (e.from.isVisible()) {
                noVis++;
            }
            else if(JungGraphPanel.collapsed){
                for(JungGraphEdgeCollapsed c : JungGraphPanel.semantic_addedEdges){
                    if(c.getReplacedWft() == e.from){
                            noVis++;
                            break;
                    }
                }
            }
        }
        return noVis;
    }

    //Will be null if this isnt a wft node. (Bugwatch: or maybe even a molecular node)
    public Caseframe getCaseframe() {
        return cf;
    }

    public ArrayList<Caseframe> getCaseframesIn() {
        ArrayList<Caseframe> cfs = new ArrayList<Caseframe>();
        //if (cf != null) {
        //    cfs.add(cf);
        //}
        for (JungGraphEdge e : upCableset) {
            if (e.from.isWftNode()) {
                cfs.add(e.from.getCaseframe());
            }
        }
        for (JungGraphEdge e : downCableset) {
            if (e.to.isWftNode()) {
                cfs.add(e.to.getCaseframe());
            }
        }
        return cfs;
    }


    public String buildLogicalForm(){
        String out = "(";
        if(!cf.hasFSymbols()){
            out += cf.name;
        }

        for(Slot s : cf.slots){
            out += " ";
            HashSet<JungGraphNode> fillers = getSlotFillers(s);
            if(fillers.size() > 1) out += "(setof";
            for(JungGraphNode n : fillers){
                if(fillers.size() > 1) out += " ";
                if(n.isWftNode()) out += n.buildLogicalForm();
                else if(n.getName().contains(" ")) out += "\"" + n.getName() + "\"";
                else out += n.getName();
            }
            if(fillers.size() > 1) out += ")";
        }

        return out + ")";
    }

    private HashSet getSlotFillers(Slot s){
        HashSet<JungGraphNode> ret = new HashSet<JungGraphNode>();
        for(JungGraphEdge e : downCableset)
            if(e.toString().equals(s.name))
                ret.add(e.to);
        return ret;
    }

    @Override
    public JungGraphNode clone() {
        return new JungGraphNode(name, type, cf, asserted, activation);
    }
}
