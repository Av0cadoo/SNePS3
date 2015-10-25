/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.buffalo.cse.sneps3.gui;

/**
 *
 * @author dan
 */
public class JungGraphEdge {

    boolean collapsed = false;

    private String label = "";

    //private String start = "";
    //private String end = "";

    //Dont ever ever ever use these unless YOU set them!
    //They are never guranteed to be set!
    public JungGraphNode from;
    public JungGraphNode to;


    public JungGraphEdge(String label, JungGraphNode start, JungGraphNode end) {
        this.from = start;
        this.to = end;
        this.label = label;
    }

    @Override
    public String toString() { // Always good for debugging
        return label;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 89 * hash + (this.label != null ? this.label.hashCode() : 0);
        hash = 89 * hash + (this.from != null ? this.from.hashCode() : 0);
        hash = 89 * hash + (this.from != null ? this.from.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final JungGraphEdge other = (JungGraphEdge) obj;
        //System.out.println("This: " + this.from + " " + this.end + " " + this.label + "\n" + "Other: " + other.start + " " + other.end + " " + other.label);

        if(this.from.equals(other.from) && this.to.equals(other.to) && this.label.equals(other.label)) return true;

        return false;
    }


}
