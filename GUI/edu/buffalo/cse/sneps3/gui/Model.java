/* This is the data model that all of the views of the data are based on.
 * The updates go from SNePS -> GUI... the GUI handles updates to SNePS3
 * via the REPL.
 * 
 * Author: Dan Schlegel
 * Modified: 11.26.2009
 */

package edu.buffalo.cse.sneps3.gui;

import java.util.*;

/**
 *
 * @author dan
 */
public class Model {
    ArrayList<IController> views;

    ArrayList<Slot> slots = new ArrayList<Slot>();

    ArrayList<Caseframe> caseframes = new ArrayList<Caseframe>();

    ArrayList<SemanticType> types = new ArrayList<SemanticType>();

    ArrayList<Context> contexts = new ArrayList<Context>();

    Context currentContext;

    public Model(){
        views = new ArrayList<IController>();
    }

    public void registerView(IController i){
        views.add(i);
    }

    public void unregisterView(IController i){
        views.remove(i);
    }

    public void recvUpdatedCaseframes(ArrayList<Caseframe> c){
        caseframes = c;
    }

    public void recvUpdatedSlots(ArrayList<Slot> s){
        slots = s;
    }

    public void recvUpdatedTypes(ArrayList<SemanticType> t){
        types = t;
    }

    public void selectCurrentContext(Context c){
        currentContext = c;
        for(IController i : views){
            i.ctCurrent(c);
        }
    }

    public void addNewCaseframe(Caseframe c){
        caseframes.add(c);
        for(IController i : views){
            i.cfUpdate(caseframes); //we might just want to send 1 at some point
        }
    }

    public void addNewSlot(Slot s){
        slots.add(s);
        if(GUI2.DEBUG) System.out.println("Slot added: " + s);
        for(IController i : views){
            i.slotUpdate(slots);
        }
    }

    public void addNewType(SemanticType s){
        types.add(s);
        for(IController i : views){
            i.stUpdate(types);
        }
    }

    public void addNewContext(Context c){
        contexts.add(c);
        for(IController i : views){
            i.ctUpdate(contexts);
        }
    }

    public void clearContexts(){
        //contexts = new ArrayList<Context>();
        contexts.clear();
        for(IController i : views){
            i.ctUpdate(contexts);
        }
    }

    public void clearSlots(){
        //slots = new ArrayList<Slot>();
        slots.clear();
        for(IController i : views){
            i.slotUpdate(slots);
        }
    }

    public void clearCaseframes(){
        //caseframes = new ArrayList<Caseframe>();
        caseframes.clear();
        for(IController i : views){
            i.cfUpdate(caseframes);
        }
    }

    public Caseframe getCaseframeByNameAndSlots(String name, ArrayList slots){
        for(Caseframe cf : caseframes){
            //if(GUI2.DEBUG) System.out.println("Comparing incf: " + name + slots + " to " + cf.name + cf.slotnames + " slotcompare: " + cf.slotnames.containsAll(slots));
            if(cf.name.equals(name) && cf.slotnames.containsAll(slots)){
                return cf;
            }
            if(cf.fsymbols != null && cf.slotnames.containsAll(slots)){
                for(String f : (ArrayList<String>)cf.fsymbols)
                    if(f.equals(name)) return cf;
            }
        }
        return null;
    }

    public Slot getSlotByName(String name){
        for(Slot s : slots){
            if(s.name.equals(name)) return s;
        }
        return null;
    }
}

