/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.buffalo.cse.sneps3.gui;

/**
 *
 * @author dan
 */
public class JungGraphEdgeCollapsed extends JungGraphEdge{

    private JungGraphNode replacesWft;

    public JungGraphEdgeCollapsed(String label, JungGraphNode start, JungGraphNode end, JungGraphNode replacedWft){
        super(label,start,end);
        this.replacesWft = replacedWft;
    }

    public JungGraphNode getReplacedWft(){
        return replacesWft;
    }

}
