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
public interface IController {
    //Context updates
    public void ctUpdate(ArrayList<Context> c);
    //Current context
    public void ctCurrent(Context c);
    //SemanticType updates
    public void stUpdate(ArrayList<SemanticType> v);
    //Caseframe updates
    public void cfUpdate(ArrayList<Caseframe> cf);
    //Slot updates
    public void slotUpdate(ArrayList<Slot> slot);
    //public void recvUpdatedCaseframes();
    //public void recvUpdatedSlots();
    //public void recvUpdatedContexts();
}
