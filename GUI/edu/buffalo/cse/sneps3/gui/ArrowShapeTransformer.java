/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.buffalo.cse.sneps3.gui;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.visualization.util.ArrowFactory;
import java.awt.Shape;
import org.apache.commons.collections15.Transformer;


public class ArrowShapeTransformer<C, S> implements Transformer<Context<Graph<JungGraphNode,JungGraphEdge>,JungGraphEdge>,Shape> {

    public Shape transform(Context<Graph<JungGraphNode, JungGraphEdge>, JungGraphEdge> arg0) {
        JungGraphEdge e = arg0.element;
        if(e.collapsed){
            return ArrowFactory.getWedgeArrow(15, 15);
        }
        else return ArrowFactory.getNotchedArrow(8, 8, 5);
    }
}

/**
 *
 * @author dan
 */
/*public class ArrowTransformerTriangle<Context, Shape> implements Transformer {

    public Shape transform(Shape s) {
        return ArrowFactory.getWedgeArrow(10, 10);
    }

    //Transformer<Context<Graph<V,E>,E>,Shape>

}
*/