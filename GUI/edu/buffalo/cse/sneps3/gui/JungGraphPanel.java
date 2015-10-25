/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * JungGraphPanel.java
 *
 * Created on Nov 28, 2009, 7:52:32 PM
 */

package edu.buffalo.cse.sneps3.gui;

import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.event.GraphEvent.Vertex;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.graph.util.Pair;
import edu.uci.ics.jung.visualization.GraphZoomScrollPane;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.RenderContext;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.CrossoverScalingControl;
import edu.uci.ics.jung.visualization.decorators.EdgeShape;
import edu.uci.ics.jung.visualization.picking.PickedState; 
import edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode;
import edu.uci.ics.jung.visualization.control.ModalLensGraphMouse;
import edu.uci.ics.jung.visualization.control.ScalingControl;
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller;
import edu.uci.ics.jung.visualization.renderers.EdgeLabelRenderer;
import edu.uci.ics.jung.visualization.renderers.Renderer;
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position;
import edu.uci.ics.jung.visualization.transform.HyperbolicTransformer;
import edu.uci.ics.jung.visualization.transform.LayoutLensSupport;
import edu.uci.ics.jung.visualization.transform.LensSupport;
import edu.uci.ics.jung.visualization.transform.shape.GraphicsDecorator;
import edu.uci.ics.jung.graph.util.DefaultParallelEdgeIndexFunction;
import edu.uci.ics.jung.graph.util.EdgeType;
import edu.uci.ics.jung.visualization.decorators.EdgeShape.QuadCurve;
import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.awt.geom.RoundRectangle2D;
import java.util.Collection;
import java.util.ArrayList;
import java.util.HashMap;
import javax.media.j3d.*;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JToggleButton;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.vecmath.Point3d;
import org.apache.commons.collections15.Transformer;

/**
 *
 * @author dan
 */
public class JungGraphPanel extends javax.swing.JPanel {

    enum GraphType {FR3d, FR2d};

    boolean straightEdges = true;
    boolean findv2 = true;

    boolean showing_all = true;

    static boolean collapsed = false;

    GraphType type = GraphType.FR2d;

    QuadCurve quadcurve = new EdgeShape.QuadCurve<JungGraphNode, JungGraphEdge>();
    EdgeShape.Line line = new EdgeShape.Line<JungGraphNode, JungGraphEdge>();


    DirectedSparseMultigraph dsg;
    VisualizationViewer<JungGraphNode, JungGraphEdge> vv;

    //No idea what i need of these yet.
    LensSupport hyperbolicViewSupport;
    LensSupport hyperbolicLayoutSupport;

    SnepsModalGraphMouse<JungGraphNode, JungGraphEdge> graphMouse = new SnepsModalGraphMouse<JungGraphNode, JungGraphEdge>();

    //These three vectors store the data for the semantic collapse mode.
    static ArrayList<JungGraphEdgeCollapsed> semantic_addedEdges = new ArrayList<JungGraphEdgeCollapsed>();
    ArrayList<JungGraphEdge> semantic_removedEdges = new ArrayList<JungGraphEdge>();
    ArrayList<JungGraphNode> semantic_removedNodes = new ArrayList<JungGraphNode>();

    //ArrayList<JungGraphEdge> find_removedEdges;
    //ArrayList<JungGraphNode> find_removedNodes;

    ArrayList on_graph_expanded_nodenames = new ArrayList<String>();; //Stores result from a "find" call.

    //For exploring the graph.
    ArrayList<JungGraphEdge> explore_removedEdges = new ArrayList<JungGraphEdge>();
    ArrayList<JungGraphNode> explore_removedNodes = new ArrayList<JungGraphNode>();

    ArrayList<JungGraphNode> highlightedNodes = new ArrayList<JungGraphNode>();
    Color highlightedColor = new Color(135,206,250);

    edu.uci.ics.jung.algorithms.layout.AbstractLayout<JungGraphNode, JungGraphEdge> layout;

    final ScalingControl scaler = new CrossoverScalingControl();
    int lastSliderVal = 0;


    int displayAreaWidth = 700;
    int displayAreaHeight = 350;

    final float dash[] = {10.0f};
    final Stroke dashStroke = new BasicStroke(1.0f, BasicStroke.CAP_BUTT,
        BasicStroke.JOIN_MITER, 10.0f, dash, 0.0f);
    final Stroke solidStroke = new BasicStroke(1.0f);

        //public JButton exitFindMode = new JButton("Exit Find");

    public int scale_btn_click = 0;
    float scaleAmt = 1.2f;

    public static JungGraphPanel instance;

    GraphZoomScrollPane panel;

    /** Creates new form JungGraphPanel */
    public JungGraphPanel() {
        instance = this;
        initComponents();

        this.removeAll();
        setLayout(new BorderLayout());
        add(jToolBar1, BorderLayout.NORTH);
        add(jPanel1, BorderLayout.SOUTH);

        Border emptyBorder = BorderFactory.createEmptyBorder();
        jButton_scalePlus.setBorder(emptyBorder);
        jButton_scaleMinus.setBorder(emptyBorder);
        jButton_scaleReset.setBorder(emptyBorder);

        //add(jToolBar2, BorderLayout.SOUTH);

        quadcurve.setEdgeIndexFunction(DefaultParallelEdgeIndexFunction.getInstance());
        
        displayGraph(new DirectedSparseMultigraph<JungGraphNode, JungGraphEdge>());

        //DirectedSparseMultigraph temp = new DirectedSparseMultigraph();
        //dsg.addVertex(new JungGraphNode("Test", null, null));
        //dsg.addVertex(new JungGraphNode("test2", null, null));
        //dsg.addVertex(new JungGraphNode("text3", null, null));
        //dsg.addEdge(new JungGraphEdge("blah", "glah", "fgsf"), dsg.getVertices().toArray()[0], dsg.getVertices().toArray()[1]);
        //JungGraphEdge ex = new JungGraphEdge("blah", "glah", "fgsf");
        //ex.collapsed = true;
        ///dsg.addEdge(ex ,dsg.getVertices().toArray()[0], dsg.getVertices().toArray()[2]);
        //displayGraph(temp);
        /*exitFindMode.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                leaveFindMode();
                ((JButton)evt.getSource()).setVisible(false);
            }
        });

        exitFindMode.setVisible(false);*/
    }

    public VisualizationViewer<JungGraphNode, JungGraphEdge> getVV(){
        return vv;
    }

    public void setFontSize(final int size) {
        Transformer<JungGraphNode, Font> vertexFont = new Transformer<JungGraphNode, Font>() {

            public Font transform(JungGraphNode i) {
                return new Font(vv.getFont().getName(), vv.getFont().getStyle(), size);
            }
        };

        vv.setFont(new Font(vv.getFont().getName(), vv.getFont().getStyle(), size));

        vv.getRenderContext().setVertexFontTransformer(vertexFont);
    }

    public DirectedSparseMultigraph<JungGraphNode, JungGraphEdge> getGraph(){
        return dsg;
    }

    public void displayGraph(DirectedSparseMultigraph<JungGraphNode, JungGraphEdge> g){

        //if(find_removedEdges != null){
        //    leaveFindMode();
        //}
        
        if(jToggleButton_collapse.isSelected()){
            jToggleButton_collapse.setSelected(false);
            semanticExpandAll();
        }

        dsg = g;
        // The Layout<V, E> is parameterized by the vertex and edge types
        // We should probably use the FRLayout(g) which is used by the current show().
        //*****2d version******//
        if(type == GraphType.FR2d){
            layout = new edu.uci.ics.jung.algorithms.layout.FRLayout(g); //experimental.
            //layout.setSize(new Dimension(this.getWidth(), this.getHeight()));
            int layoutwidth = (this.getWidth() < 100 ? 700 : this.getWidth()-((Integer)UIManager.get("ScrollBar.width")).intValue()-15);
            int layoutheight = (this.getHeight() < 100 ? 350 : this.getHeight()-((Integer)UIManager.get("ScrollBar.width")).intValue()-40);

            layout.setSize(new Dimension(layoutwidth, layoutheight));
            //layout.setSize(new Dimension(this.getWidth()-((Integer)UIManager.get("ScrollBar.width")).intValue()-5 , this.getHeight()-((Integer)UIManager.get("ScrollBar.width")).intValue()-30));
            //layout.setSize(new Dimension(700, 350)); // sets the initial size of the space

            
            vv = new VisualizationViewer<JungGraphNode, JungGraphEdge>(layout);

            //Now that we've re-displayed the graph we need to reset the zoom back to where it was.
            if(scale_btn_click > 0)
                for(int i = 0; i < this.scale_btn_click; i++){
                    scaler.scale(vv, scaleAmt, vv.getCenter());
                }

            vv.setBackground(Color.white);

            vv.getRenderContext().setVertexLabelTransformer(new ToStringLabeller());
            vv.getRenderer().getVertexLabelRenderer().setPosition(Position.CNTR);

            if(straightEdges){
                vv.getRenderContext().setEdgeShapeTransformer(new Transformer<Context<Graph<JungGraphNode, JungGraphEdge>, JungGraphEdge>, Shape>(){
                    public Shape transform(Context<Graph<JungGraphNode, JungGraphEdge>, JungGraphEdge> i) {
                        if(i.element instanceof JungRestrictionGraphEdge){
                            //return new EdgeShape.QuadCurve<JungGraphNode, JungGraphEdge>().transform(i);
                            return quadcurve.transform(i);
                        }

                        //Check for parallel edges:
                        JungGraphNode n = i.element.from;
                        for(Object j : dsg.getOutEdges(n)){
                            JungGraphEdge e = (JungGraphEdge)j;
                            if(e.to == i.element.to && i.element != e){
                                return quadcurve.transform(i);
                            }
                        }

                        //return new EdgeShape.Line<JungGraphNode, JungGraphEdge>().transform(i);
                        return line.transform(i);
                    }
                });



                //vv.getRenderContext().setEdgeShapeTransformer(new EdgeShape.Line<JungGraphNode, JungGraphEdge>());
                vv.getRenderer().setEdgeLabelRenderer(new CustomEdgeLabelRenderer<JungGraphNode, JungGraphEdge>());
            }

            vv.getRenderContext().setEdgeStrokeTransformer(new Transformer<JungGraphEdge, Stroke>(){

                public Stroke transform(JungGraphEdge i) {
                    if(i instanceof JungRestrictionGraphEdge){
                        return dashStroke;
                    }
                    return solidStroke;
                }

            });

            //Makes the labels more closely really centered on the line.
            vv.getRenderContext().setEdgeLabelClosenessTransformer(new Transformer<edu.uci.ics.jung.graph.util.Context<Graph<JungGraphNode, JungGraphEdge>, JungGraphEdge>, Number>(){

                    public Number transform(Context<Graph<JungGraphNode, JungGraphEdge>, JungGraphEdge> i) {
                        return new Double(.5);
                    }

            });

            vv.getRenderContext().setEdgeLabelTransformer(new ToStringLabeller());

            vv.getRenderContext().setEdgeArrowTransformer(new ArrowShapeTransformer());

            vv.getRenderContext().setVertexShapeTransformer(new NodeShapeTransformer());

            //Transformer<String,Paint> vertexPaint = new Transformer<String,Paint>() {
            //     public Paint transform(String i) {
            //    return Color.YELLOW;
            // }
            //};

            //vv.getRenderContext().setVertexFillPaintTransformer(vertexPaint);


	    Transformer<JungGraphNode,Paint> vertexPaint = new Transformer<JungGraphNode,Paint>()             {
		public Paint transform(JungGraphNode i) {
		    
		    PickedState ps = vv.getPickedVertexState();

		    if (ps.isPicked(i))
			return Color.green;
                    else if(highlightedNodes.contains(i))
                        return highlightedColor;
		    else if (i.getActivation() == 1.0)
			return Color.red;
		    else if(i.getActivation() > 0.40)
			return Color.orange;
		    else if(i.getActivation() > 0)
			return Color.yellow;
		    else
			return Color.pink;
		}
	    };

	    vv.getRenderContext().setVertexFillPaintTransformer(vertexPaint); 

            //vv.getRenderContext().setVertexFillPaintTransformer(new PickableVertexPaintTransformer<JungGraphNode>(vv.getPickedVertexState(), Color.yellow, Color.green));


            Transformer<String,Paint> arrowPaint = new Transformer<String,Paint>() {
                 public Paint transform(String i) {
                return Color.white;
             }
            };


            vv.getRenderContext().setArrowFillPaintTransformer(new ArrowFillTransformer());

            //VertexLabelAsShapeRenderer<String,String> vlasr = new VertexLabelAsShapeRenderer<String,String>(vv.getRenderContext());

            //Mouse actions on the graph.
            //DefaultModalGraphMouse graphMouse = new DefaultModalGraphMouse();
            vv.setGraphMouse(graphMouse);
            //graphMouse.addItemListener(hyperbolicLayoutSupport.getGraphMouse().getModeListener());
            //graphMouse.addItemListener(hyperbolicViewSupport.getGraphMouse().getModeListener());
            graphMouse.setMode(Mode.PICKING);

            //ADDED STUFF:
         /*           hyperbolicViewSupport =
            new ViewLensSupport<String,String>(vv, new HyperbolicShapeTransformer(vv,
            		vv.getRenderContext().getMultiLayerTransformer().getTransformer(Layer.VIEW)),
                    new ModalLensGraphMouse());*/
            hyperbolicLayoutSupport =
            new LayoutLensSupport<JungGraphNode,JungGraphEdge>(vv, new HyperbolicTransformer(vv,
            		vv.getRenderContext().getMultiLayerTransformer().getTransformer(Layer.LAYOUT)),
                    new ModalLensGraphMouse());

                    //hyperbolicLayoutSupport.getLensTransformer().setLensShape(hyperbolicViewSupport.getLensTransformer().getLensShape());
                    //hyperbolicLayoutSupport.activate();

                    graphMouse.addItemListener(hyperbolicLayoutSupport.getGraphMouse().getModeListener());

            //END ADDED
                    
            //        exitFindMode.setLocation(0, 0);
            //vv.add(exitFindMode);

            
                panel = new GraphZoomScrollPane(vv);
                if(((BorderLayout)this.getLayout()).getLayoutComponent(BorderLayout.CENTER) != null)
                    this.remove(((BorderLayout)this.getLayout()).getLayoutComponent(BorderLayout.CENTER));
                add(panel, BorderLayout.CENTER);
            

            //final GraphZoomScrollPane panel = new GraphZoomScrollPane(vv);
            //jSplitPane1.setBottomComponent(panel);
        }
        //*****3d version*****//
        if(type == GraphType.FR3d){
            edu.uci.ics.jung.algorithms.layout3d.Layout<JungGraphNode, String> layout3d = new edu.uci.ics.jung.algorithms.layout3d.FRLayout(g);
            layout3d.setSize(new BoundingSphere(new Point3d(150,150,150), 50)); // sets the initial size of the space
            // The BasicVisualizationServer<V,E> is parameterized by the edge types
            //BasicVisualizationServer<String, String> vv =
            //        new BasicVisualizationServer<String, String>(layout);
            edu.uci.ics.jung.visualization3d.VisualizationViewer<JungGraphNode, String> vv3d = new edu.uci.ics.jung.visualization3d.VisualizationViewer<JungGraphNode, String>();
            vv3d.setGraphLayout(layout3d);
            vv3d.setPreferredSize(new Dimension(350, 350)); //Sets the viewing area size
            add(vv3d, BorderLayout.CENTER);
            //jSplitPane1.setBottomComponent(vv3d);
        }

        //on_graph_expanded_nodenames = new ArrayList<String>();
        on_graph_expanded_nodenames.clear();
        for(Object o : dsg.getVertices()){
            on_graph_expanded_nodenames.add(((JungGraphNode)o).getName());
        }

    }

    /*public void resetScale(){
        jSlider1.setValue(0);
    }*/

    /**
     *
     * @param nn
     * @return true if it works, false if it doesn't.
     */
    /*public String findSingleNode(String nn){
        on_graph_expanded_nodenames = new ArrayList<String>();
        on_graph_expanded_nodenames.add(nn);

        JungGraphNode found = null;
        find_removedEdges = new ArrayList<JungGraphEdge>();
        find_removedNodes = new ArrayList<JungGraphNode>();
        ArrayList<JungGraphNode> toRemove = new ArrayList<JungGraphNode>();
        ArrayList<JungGraphEdge> toRemoveE = new ArrayList<JungGraphEdge>();
        for(JungGraphNode n : (Collection<JungGraphNode>)dsg.getVertices()){
            if(n.name.equals(nn)){
                found = n;
            }
            //else{
                toRemove.add(n);
                for(Object j : dsg.getInEdges(n)){
                    JungGraphEdge e = (JungGraphEdge)j;
                    toRemoveE.add(e);
                }
            //}
        }
        if(found == null) return "No node with label: " + nn + " was found.";
        else{
            for(JungGraphEdge e : toRemoveE){
                find_removedEdges.add(e);
                dsg.removeEdge(e);
            }
            for(JungGraphNode n : toRemove){
                find_removedNodes.add(n);
                n.visible = false;
                dsg.removeVertex(n);
            }
            showNode(found);
            vv.repaint();
            return null;
        }
    }*/

    public void addVertex(JungGraphNode n){
        dsg.addVertex(n);
        n.visible = true;
        on_graph_expanded_nodenames.add(n.getName());
    }

    public void reinitialize(){
        jToggleButton_collapse.setSelected(false);
        collapsed = false;
        if(GUI2.DEBUG) System.out.println("NNNM-reinit: " + GUI2.getInstance().getNodeName_node_map());
        GUI2.getInstance().getNodeName_node_map().clear();
        if(GUI2.DEBUG) System.out.println("NNNM-reinit: " + GUI2.getInstance().getNodeName_node_map());
        hideAll();
        semantic_addedEdges.clear();
        semantic_removedEdges.clear();
        semantic_removedNodes.clear();
        on_graph_expanded_nodenames.clear();
        showing_all = true;
    }

    public void hideAll(){
        Object[] arr = dsg.getVertices().toArray();
        for(Object o : arr){
            dsg.removeVertex(o);
            ((JungGraphNode)o).visible = false;
        }
        semantic_addedEdges.clear();
        semantic_removedEdges.clear();
        semantic_removedNodes.clear();
        on_graph_expanded_nodenames.clear();
        vv.repaint();
        showing_all = false;
    }

    public void showAll(){
        if(collapsed) semanticExpandAll();
        for(JungGraphNode n : GUI2.getInstance().getNodeName_node_map().values()){
            if(!n.isVisible()){
                dsg.addVertex(n);
                n.visible = true;
            }
        }
        for(JungGraphEdge e : GUI2.getInstance().getEdge_list()){
            dsg.addEdge(e, e.from, e.to, EdgeType.DIRECTED);
        }
        on_graph_expanded_nodenames.clear();
        for(Object o : dsg.getVertices()){
            on_graph_expanded_nodenames.add(((JungGraphNode)o).getName());
        }
        if(collapsed) semanticCollapseAll();
        vv.repaint();
        showing_all = true;
    }

    public boolean isEmpty(){
        return (dsg.getVertexCount()==0);
    }

    public boolean isShowingAll(){
        return showing_all;
    }


    public String displayOnlyNodeSet(ArrayList<String> nn){
        hideAll();
        on_graph_expanded_nodenames = new ArrayList<String>();
        return displayNodeSet(nn);
    }

    public String displayNodeSet(ArrayList<String> nn) {
        ArrayList<JungGraphNode> interestingNodes = new ArrayList<JungGraphNode>();
        String fail = "";
        for (String j : nn) {
            JungGraphNode t = GUI2.getInstance().getNodeName_node_map().get(j);
            if (t != null) {
                interestingNodes.add(t);
            } else {
                fail += " \"" + j + "\"";
            }
        }

        if(!fail.equals("")) return fail;

        on_graph_expanded_nodenames.addAll(nn);

        for (JungGraphNode n : interestingNodes) {
            //If it isn't already on the graph, add it.
            if (!n.isVisible()) {
                showNode(n);
                for(JungGraphEdge e : GUI2.getInstance().getEdge_list()){
                    if(e.from == n && dsg.containsVertex(e.to)) dsg.addEdge(e, e.from, e.to, EdgeType.DIRECTED);
                }
            }
        }

        highlightedNodes.clear();
        for(JungGraphNode n : interestingNodes)
            highlightedNodes.add(n);
        //vv.repaint();

        if(collapsed) semanticCollapse(interestingNodes);
        return null;
    }


    public void semanticCollapseAll(){
        semantic_addedEdges.clear();
        semantic_removedEdges.clear();
        semantic_removedNodes.clear();
        semanticCollapse(dsg.getVertices());
    }

    /* Iterate through the nodes in the graph looking for the wft nodes. When one is found
     * check that it is a binary relation and the wft node doesnt have any other edges
     * attached to it. If this is the case the nodes may be collapsed in the following way:
     * Cat <--member-- WFT1 --class--> Animal  ==>  Cat --Isa--> Animal
     */
    public void semanticCollapse(Collection c){
        //semantic_addedEdges = new ArrayList<JungGraphEdge>();
        //semantic_removedEdges = new ArrayList<JungGraphEdge>();
        //semantic_removedNodes = new ArrayList<JungGraphNode>();

        ArrayList<JungGraphNode> toRemove = new ArrayList<JungGraphNode>();
        //Collection c = dsg.getVertices();
        for(Object i : c){
            JungGraphNode n = (JungGraphNode)i;
            if(n.isWftNode() && n.getCaseframe().type.equals("Proposition") && n.getDownCableset().size() == 2 && n.getUpCableset().isEmpty()){
                if(GUI2.DEBUG) System.out.println("Found a WFT node with " + n.getDownCableset().size() + " out edges and " + n.getUpCableset().size() + " in edges.");

                //Those conditions are out of the way. Now do the actual work.
                JungGraphEdge e1 = n.getDownCableset().get(0);
                JungGraphEdge e2 = n.getDownCableset().get(1);

                if(e1.to == e2.to) continue; //Don't have circular edges.

                toRemove.add(n);

                dsg.removeEdge(e1);
                dsg.removeEdge(e2);
                //Now connect c1 and c2 with the label as the cf of n.
                //We need a proper ordering.
                Caseframe cf = n.getCaseframe();
                JungGraphEdgeCollapsed newEdge = null;
                String newEdgeText = cf.name + (n.asserted ? "!" : "");
                if(cf.slots.get(0).toString().equals(e1.toString())){
                    newEdge = new JungGraphEdgeCollapsed(newEdgeText, e1.to, e2.to, n);
                    newEdge.collapsed = true;
                    dsg.addEdge(newEdge, e1.to, e2.to);
                }
                else{
                    newEdge = new JungGraphEdgeCollapsed(newEdgeText, e2.to, e1.to, n);
                    newEdge.collapsed = true;
                    dsg.addEdge(newEdge, e2.to, e1.to);
                }

                semantic_addedEdges.add(newEdge);
                semantic_removedEdges.add(e2);
                semantic_removedEdges.add(e1);
                semantic_removedNodes.add(n);
            }
        }

        //Handling nodes for which there is a slot in the first position of the cf and size is 3.
        for(Object i : c){
            JungGraphNode n = (JungGraphNode)i;
            if(n.isWftNode() && n.getCaseframe().type.equals("Proposition")
                    && n.getUpCableset().isEmpty() && n.getDownCableset().size() == 3
                    && GUI2.model.getSlotByName(n.cf.name) != null && n.cf.hasFSymbols()){
                if(GUI2.DEBUG) System.out.println("Found a slot-named cf to collapse: " + n.toString() + " with edges " + n.getDownCableset());
                JungGraphEdge labelEdge = null;
                JungGraphEdge e1 = null;
                JungGraphEdge e2 = null;

                for(JungGraphEdge e : n.getDownCableset()){
                    if(e.toString().equals(n.cf.name)){
                        labelEdge = e;
                    }
                    else{
                        if(e1==null) e1 = e;
                        else e2 = e;
                    }
                }

                if(e1.to == e2.to) continue; //Don't have circular edges.

                //We want to collapse a wft with 3 out edges, where one of them is an fsymbol
                //as long as the fsymbol is only pointed to by edges from other wft nodes which
                //also treat it as an fsymbol. It also cannot have out edges.
                boolean remove_slot_node = true;

                if(!labelEdge.to.getDownCableset().isEmpty()) remove_slot_node = false;
                for(JungGraphEdge e : labelEdge.to.getUpCableset()){
                    if(!e.toString().equals(labelEdge.toString()))
                        remove_slot_node = false;
                }


                if (remove_slot_node) {
                    toRemove.add(labelEdge.to);
                }
                toRemove.add(n);
                //Remove the edge we're making into the label of the new edge:
                String newEdgeLabel = "";
                if(!labelEdge.to.isWftNode()){ //Function sym.
                    newEdgeLabel = labelEdge.to.toString() + (n.asserted ? "!" : "");
                }
                else{ //Function-valued function
                    newEdgeLabel = labelEdge.to.buildLogicalForm() + (n.asserted ? "!" : "");
                }




                dsg.removeEdge(labelEdge);

                toRemove.add(n);

                dsg.removeEdge(e1);
                dsg.removeEdge(e2);
                //Now connect c1 and c2 with the label as the labelEdge name.
                //We need a proper ordering.
                Caseframe cf = n.getCaseframe();
                JungGraphEdgeCollapsed newEdge = null;
                //System.out.println("cf.slots.firstElement().toString()" + cf.slots.firstElement().toString());
                if (cf.slots.get(1).toString().equals(e1.toString())) {
                    newEdge = new JungGraphEdgeCollapsed(newEdgeLabel, e1.to, e2.to, n);
                    newEdge.collapsed = true;
                    dsg.addEdge(newEdge, e1.to, e2.to);
                } else {
                    newEdge = new JungGraphEdgeCollapsed(newEdgeLabel, e2.to, e1.to, n);
                    newEdge.collapsed = true;
                    dsg.addEdge(newEdge, e2.to, e1.to);
                }

                semantic_addedEdges.add(newEdge);
                semantic_removedEdges.add(e2);
                semantic_removedEdges.add(e1);
                semantic_removedNodes.add(n);
                semantic_removedNodes.add(labelEdge.to);
                semantic_removedEdges.add(labelEdge);

            }
        }

        for (JungGraphNode n : toRemove) {
            dsg.removeVertex(n);
            n.visible = false;
        }
        vv.repaint();
    }

    public boolean isWftReplaced(JungGraphNode wft){
        for(JungGraphEdgeCollapsed e : semantic_addedEdges){
            if(e.getReplacedWft() == wft) return true;
        }
        return false;
    }

    public void semanticExpandAll() {
        semanticExpand(new PairLR(semantic_removedNodes, semantic_removedEdges)); //This will expand everything which we've collapsed.
    }

    //If we're in Find Mode, we should uncollapse those which are actually
    //in the graph.
    //Takes in a list of removed nodes and edges which we would like to be added
    //back to the graph.
    public void semanticExpand(PairLR<ArrayList<JungGraphNode>, ArrayList<JungGraphEdge>> c) {
        ArrayList<JungGraphNode> toRemoveN = new ArrayList<JungGraphNode>();
        for (JungGraphNode n : c.getLeft()) {
            toRemoveN.add(n);
            if (on_graph_expanded_nodenames.contains(n.getName())) {
                //dsg.addVertex(n); //Comm
                showNode(n);
            }
        }

        ArrayList<JungGraphEdge> toRemoveE = new ArrayList<JungGraphEdge>();
        for (JungGraphEdge e : c.getRight()) {
            toRemoveE.add(e);
            if (dsg.containsVertex(e.from) && dsg.containsVertex(e.to)) {
                dsg.addEdge(e, e.from, e.to);
            }
        }

        ArrayList<JungGraphEdgeCollapsed> toRemoveC = new ArrayList<JungGraphEdgeCollapsed>();
        for (JungGraphEdgeCollapsed e : semantic_addedEdges) {
            if (dsg.containsVertex(e.from) && dsg.containsVertex(e.to)
                    && c.getLeft().contains(e.getReplacedWft())) {
                dsg.removeEdge(e);
                toRemoveC.add(e);
            }
        }

        //System.out.println("AE: " + semantic_addedEdges + "\nRN: " + semantic_removedNodes + "\nRE:" + semantic_removedEdges);

        semantic_addedEdges.removeAll(toRemoveC);
        semantic_removedNodes.removeAll(toRemoveN);
        semantic_removedEdges.removeAll(toRemoveE);

        //System.out.println("AE: " + semantic_addedEdges + "\nRN: " + semantic_removedNodes + "\nRE:" + semantic_removedEdges);
        vv.repaint();
    }


    public void hideNode(JungGraphNode n){
        PairLR<ArrayList<JungGraphNode>, ArrayList<JungGraphEdge>> c =
                hideNodeAggregator(n, new PairLR<ArrayList<JungGraphNode>, ArrayList<JungGraphEdge>>(new ArrayList<JungGraphNode>(), new ArrayList<JungGraphEdge>()));
        //System.out.println("hideNode pair: Left: " + c.getLeft() + "\nRight: " + c.getRight());
        if(jToggleButton_collapse.isSelected()) semanticExpand(c);
        for(JungGraphEdge e : c.getRight()){
            dsg.removeEdge(e);
            explore_removedEdges.add(e);
        }
        for(JungGraphNode n1 : c.getLeft()){
            dsg.removeVertex(n1);
            explore_removedNodes.add(n1);
            n1.visible = false;
        }
        for(JungGraphNode n1 : c.getLeft()){
            on_graph_expanded_nodenames.remove(n1.getName());
        }
        vv.repaint();
        showing_all = false;
    }

    //Gathers the list of nodes whcih need to be hidden, but perhaps cannot be yet because of the
    //state of the graph (ie. is in collapsed mode). It returns this list to hideNode which then
    //handles expanding the appropriate nodes, and hiding them.
    private PairLR<ArrayList<JungGraphNode>, ArrayList<JungGraphEdge>> hideNodeAggregator(JungGraphNode n, PairLR<ArrayList<JungGraphNode>, ArrayList<JungGraphEdge>> c){
        //parent.setAddMode(true);

        //If its a wft node, hide every edge in the up cableset, except collapsed ones.
        if(n.isWftNode()){
            c.getLeft().add(n);
            c.getRight().addAll(n.getUpCableset());
            c.getRight().addAll(n.getDownCableset());
            //c.getRight().addAll(n.getVisibleUpCablesetMinusFS());
            //c.getRight().addAll(n.getVisibleDownCablesetMinusFS());



            //System.out.println("UCS: " + n.getUpCableset() + " DCS: " + n.getDownCableset());

            for(JungGraphEdge e : n.getUpCableset()){
                if(e.from.isVisible()){
                    c = hideNodeAggregator(e.from, c);
                }
            }
        }
        //Otherwise we need to find all of the WFT nodes we're attached to and
        //hide them, since they make no sense now.
        else{
            //ArrayList<JungGraphEdge> dc = n.getUpCableset();
            ArrayList<JungGraphEdge> dc = n.getUpCablesetMinusFS();
            for(JungGraphEdge e : dc){
                if(e.from.isWftNode()){
                    c = hideNodeAggregator(e.from, c);
                }
            }
            c.getLeft().add(n);
            //Now hide this node.
            //dsg.removeVertex(n);
            //explore_removedNodes.add(n);
            //n.visible = false;
        }

        return c;

        //parent.setAddMode(false);
    }

    //When showing a node we need to be sure we don't show half a relation.
    public void showNode(JungGraphNode n){
        ArrayList<JungGraphNode> c = showNode(n, new ArrayList());
        if(jToggleButton_collapse.isSelected()) semanticCollapse(c);
        for(JungGraphNode n1 : c) 
            on_graph_expanded_nodenames.add(n1.getName());
    }

    private ArrayList<JungGraphNode> showNode(JungGraphNode n, ArrayList<JungGraphNode> c){
        //parent.setAddMode(true);

        if(n.isVisible()) return c;
        if(n.isWftNode()){
            for(JungGraphEdge e : n.getDownCableset()){
                if(!e.to.isVisible()){
                    c.addAll(showNode(e.to, c));
                }
                dsg.addEdge(e, e.from, e.to);
                explore_removedEdges.remove(e);
            }
            c.add(n);
            dsg.addVertex(n);
            n.visible = true;
            explore_removedNodes.remove(n);
        }
        else{
            c.add(n);
            dsg.addVertex(n);
            n.visible = true;
            explore_removedNodes.remove(n);
        }

        //parent.setAddMode(false);
        vv.repaint();
        return c;
    }

    public void hideUpCableset(JungGraphNode n){
        for(JungGraphEdge e : n.getUpCableset()){
            if(e.from.isVisible() || (collapsed && isWftReplaced(e.from))){
                hideNode(e.from);
            }
        }
    }

    

    public void showUpCableset(JungGraphNode n){
        for(JungGraphEdge e : n.getUpCableset()){
            if(!e.from.isVisible()){
                showNode(e.from);
            }
        }
    }

    public void hideDownCableset(JungGraphNode n){
        //Shouldn't be recursive - so this is all commented out.
        //for(JungGraphEdge e : n.getDownCableset()){
        //    if(e.to.isVisible() & e.to.isWftNode()){
        //        hideNode(e.to);
        //    }
        //}
        if(n.isVisible()) hideNode(n);
    }

    public void showDownCableset(JungGraphNode n){
        for(JungGraphEdge e : n.getDownCableset()){
            if(!e.to.isVisible()){
                showNode(e.to);
            }
        }
    }

    public HashMap<Caseframe, ArrayList<JungGraphEdge>> getHiddenUpCablesetCfs(JungGraphNode n){
        //Get the up cableset of the node, and look at the nodes these edges are from.
        //These should all be wft nodes, and from there the list of CFs can be found.

        HashMap<Caseframe, ArrayList<JungGraphEdge>> ret = new HashMap<Caseframe, ArrayList<JungGraphEdge>>();

        //n.getCaseframesIn();

        for(JungGraphEdge e : n.getUpCableset()){
            if((!e.from.isVisible() && !collapsed) || (collapsed && !isWftReplaced(e.from))){
                if(e.from.isWftNode()){
                    if(ret.get(e.from.cf) == null){
                        ArrayList<JungGraphEdge> a = new ArrayList<JungGraphEdge>();
                        a.add(e);
                        ret.put(e.from.cf, a);
                    }
                    else
                        ret.get(e.from.cf).add(e);
                }
                else
                    System.out.println("We're missing some situation in showing up cfs");
            }
        }

        return ret;
    }
    
    public HashMap<Caseframe, ArrayList<JungGraphEdge>> getVisibleUpCablesetCfs(JungGraphNode n){
        //Get the up cableset of the node, and look at the nodes these edges are from.
        //These should all be wft nodes, and from there the list of CFs can be found.

        HashMap<Caseframe, ArrayList<JungGraphEdge>> ret = new HashMap<Caseframe, ArrayList<JungGraphEdge>>();

        //n.getCaseframesIn();

        for(JungGraphEdge e : n.getUpCableset()){
            if(e.from.isVisible() || (collapsed && isWftReplaced(e.from))){
                if(e.from.isWftNode()){
                    if(ret.get(e.from.cf) == null){
                        ArrayList<JungGraphEdge> a = new ArrayList<JungGraphEdge>();
                        a.add(e);
                        ret.put(e.from.cf, a);
                    }
                    else
                        ret.get(e.from.cf).add(e);
                }
                else
                    System.out.println("We're missing some situation in showing up cfs");
            }
        }

        return ret;
    }



    protected void setStatusbarText(String t){
        jLabel_status.setText(t);
    }

/*    public Vector<JungGraphEdge> edgesOf(JungGraphNode n){
        Vector<JungGraphEdge> v = new Vector<JungGraphEdge>();
        while(dsg.getEdges().iterator().hasNext()){
            JungGraphEdge e = (JungGraphEdge)dsg.getEdges().iterator().next();
            dsg.get
        }

        return v;
    }*/

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jToolBar1 = new javax.swing.JToolBar();
        jButton_showInGraph = new javax.swing.JButton();
        jButton_hideAll = new javax.swing.JButton();
        jButton_showAll = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jComboBox1 = new javax.swing.JComboBox();
        jSeparator2 = new javax.swing.JToolBar.Separator();
        jLabel1 = new javax.swing.JLabel();
        jToggleButton_lens = new javax.swing.JToggleButton();
        jToggleButton_collapse = new javax.swing.JToggleButton();
        jPanel1 = new javax.swing.JPanel();
        jToolBar2 = new javax.swing.JToolBar();
        jLabel3 = new javax.swing.JLabel();
        jButton_scalePlus = new javax.swing.JButton();
        jButton_scaleMinus = new javax.swing.JButton();
        jButton_scaleReset = new javax.swing.JButton();
        jLabel_status = new javax.swing.JLabel();

        jToolBar1.setRollover(true);

        jButton_showInGraph.setText("Show In Graph");
        jButton_showInGraph.setFocusable(false);
        jButton_showInGraph.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton_showInGraph.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButton_showInGraph.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_showInGraphActionPerformed(evt);
            }
        });
        jToolBar1.add(jButton_showInGraph);

        jButton_hideAll.setText("Hide All");
        jButton_hideAll.setFocusable(false);
        jButton_hideAll.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton_hideAll.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButton_hideAll.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_hideAllActionPerformed(evt);
            }
        });
        jToolBar1.add(jButton_hideAll);

        jButton_showAll.setText("Show All");
        jButton_showAll.setFocusable(false);
        jButton_showAll.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton_showAll.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButton_showAll.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_showAllActionPerformed(evt);
            }
        });
        jToolBar1.add(jButton_showAll);

        jLabel2.setText("Mouse:");
        jToolBar1.add(jLabel2);

        jComboBox1.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Picking", "Transforming" }));
        jComboBox1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBox1ActionPerformed(evt);
            }
        });
        jToolBar1.add(jComboBox1);
        jToolBar1.add(jSeparator2);

        jLabel1.setText("View:");
        jToolBar1.add(jLabel1);

        jToggleButton_lens.setText("Lens");
        jToggleButton_lens.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jToggleButton_lensActionPerformed(evt);
            }
        });
        jToolBar1.add(jToggleButton_lens);

        jToggleButton_collapse.setText("Collapsed");
        jToggleButton_collapse.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jToggleButton_collapseActionPerformed(evt);
            }
        });
        jToolBar1.add(jToggleButton_collapse);

        jToolBar2.setFloatable(false);
        jToolBar2.setRollover(true);

        jLabel3.setFont(new java.awt.Font("Ubuntu", 0, 12));
        jLabel3.setText("Zoom:");
        jToolBar2.add(jLabel3);

        jButton_scalePlus.setFont(new java.awt.Font("Ubuntu", 0, 12));
        jButton_scalePlus.setText(" + ");
        jButton_scalePlus.setFocusable(false);
        jButton_scalePlus.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton_scalePlus.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButton_scalePlus.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_scalePlusActionPerformed(evt);
            }
        });
        jToolBar2.add(jButton_scalePlus);

        jButton_scaleMinus.setFont(new java.awt.Font("Ubuntu", 0, 12));
        jButton_scaleMinus.setText(" - ");
        jButton_scaleMinus.setFocusable(false);
        jButton_scaleMinus.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton_scaleMinus.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButton_scaleMinus.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_scaleMinusActionPerformed(evt);
            }
        });
        jToolBar2.add(jButton_scaleMinus);

        jButton_scaleReset.setFont(new java.awt.Font("Ubuntu", 0, 12));
        jButton_scaleReset.setText(" Reset ");
        jButton_scaleReset.setFocusable(false);
        jButton_scaleReset.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButton_scaleReset.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButton_scaleReset.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton_scaleResetActionPerformed(evt);
            }
        });
        jToolBar2.add(jButton_scaleReset);

        jLabel_status.setText(" ");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addComponent(jLabel_status)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 652, Short.MAX_VALUE)
                .addComponent(jToolBar2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jLabel_status)
            .addComponent(jToolBar2, javax.swing.GroupLayout.PREFERRED_SIZE, 18, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jToolBar1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jToolBar1, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 362, Short.MAX_VALUE)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );
    }// </editor-fold>//GEN-END:initComponents

    private void jToggleButton_collapseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jToggleButton_collapseActionPerformed
        if(jToggleButton_collapse.isSelected()){
            collapsed = true;
            semanticCollapseAll();
        }
        else{
            semanticExpandAll();
            collapsed = false;
        }
    }//GEN-LAST:event_jToggleButton_collapseActionPerformed

    private void jComboBox1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBox1ActionPerformed
        if(jComboBox1.getSelectedItem().equals("Picking")) graphMouse.setMode(Mode.PICKING);
        else graphMouse.setMode(Mode.TRANSFORMING);
    }//GEN-LAST:event_jComboBox1ActionPerformed

    private void jToggleButton_lensActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jToggleButton_lensActionPerformed
        if(jToggleButton_lens.isSelected()) hyperbolicLayoutSupport.activate();
        else hyperbolicLayoutSupport.deactivate();

        if(jComboBox1.getSelectedItem().equals("Picking")) graphMouse.setMode(Mode.PICKING);
        else graphMouse.setMode(Mode.TRANSFORMING);
    }//GEN-LAST:event_jToggleButton_lensActionPerformed

    private void jButton_scalePlusActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton_scalePlusActionPerformed
        scaler.scale(vv, scaleAmt, vv.getCenter());
        this.scale_btn_click++;
    }//GEN-LAST:event_jButton_scalePlusActionPerformed

    private void jButton_scaleMinusActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton_scaleMinusActionPerformed
        scaler.scale(vv, 1/scaleAmt, vv.getCenter());
        this.scale_btn_click--;
    }//GEN-LAST:event_jButton_scaleMinusActionPerformed

    private void jButton_scaleResetActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton_scaleResetActionPerformed
        vv.getRenderContext().getMultiLayerTransformer().getTransformer(Layer.LAYOUT).setToIdentity();
        vv.getRenderContext().getMultiLayerTransformer().getTransformer(Layer.VIEW).setToIdentity();
        this.scale_btn_click = 0;
    }//GEN-LAST:event_jButton_scaleResetActionPerformed

    private void jButton_hideAllActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton_hideAllActionPerformed
        hideAll();
    }//GEN-LAST:event_jButton_hideAllActionPerformed

    private void jButton_showAllActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton_showAllActionPerformed
        showAll();
    }//GEN-LAST:event_jButton_showAllActionPerformed

    private void jButton_showInGraphActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton_showInGraphActionPerformed
        FindQuery3 f = new FindQuery3();
        f.setVisible(true);
    }//GEN-LAST:event_jButton_showInGraphActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton_hideAll;
    private javax.swing.JButton jButton_scaleMinus;
    private javax.swing.JButton jButton_scalePlus;
    private javax.swing.JButton jButton_scaleReset;
    private javax.swing.JButton jButton_showAll;
    private javax.swing.JButton jButton_showInGraph;
    private javax.swing.JComboBox jComboBox1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel_status;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JToolBar.Separator jSeparator2;
    private javax.swing.JToggleButton jToggleButton_collapse;
    private javax.swing.JToggleButton jToggleButton_lens;
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JToolBar jToolBar2;
    // End of variables declaration//GEN-END:variables


    class NodeShapeTransformer<St, Ss> implements Transformer<JungGraphNode, Shape> {

        private DirectedSparseMultigraph d;

       // public NodeShapeTransformer(DirectedSparseMultigraph dsg){
       //     d = dsg;
       // }

        public Shape transform(JungGraphNode arg0) {
            //int width = 13*arg0.toString().length();

            int width = vv.getFontMetrics(vv.getFont()).stringWidth(arg0.toString())+10;

            return new RoundRectangle2D.Float((width/2)*-1, -7, width, 15, 5, 5);

            //return new java.awt.Rectangle((width/2)*-1, -7, width, 15);
        }

    }

    class CustomEdgeLabelRenderer<V,E> implements Renderer.EdgeLabel<V,E> {
	
	public Component prepareRenderer(RenderContext<V,E> rc, EdgeLabelRenderer graphLabelRenderer, Object value, 
			boolean isSelected, E edge) {
		return rc.getEdgeLabelRenderer().<E>getEdgeLabelRendererComponent(rc.getScreenDevice(), value, 
				rc.getEdgeFontTransformer().transform(edge), isSelected, edge);
	}
    
        public void labelEdge(RenderContext<V,E> rc, edu.uci.ics.jung.algorithms.layout.Layout<V,E> layout, E e, String label) {
            if(label == null || label.length() == 0) return;

            Graph<V,E> graph = layout.getGraph();
            // don't draw edge if either incident vertex is not drawn
            Pair<V> endpoints = graph.getEndpoints(e);
            V v1 = endpoints.getFirst();
            V v2 = endpoints.getSecond();
            if (!rc.getEdgeIncludePredicate().evaluate(Context.<Graph<V,E>,E>getInstance(graph,e)))
                return;

            if (!rc.getVertexIncludePredicate().evaluate(Context.<Graph<V,E>,V>getInstance(graph,v1)) || 
                !rc.getVertexIncludePredicate().evaluate(Context.<Graph<V,E>,V>getInstance(graph,v2)))
                return;

            Point2D p1 = layout.transform(v1);
            Point2D p2 = layout.transform(v2);
            p1 = rc.getMultiLayerTransformer().transform(Layer.LAYOUT, p1);
            p2 = rc.getMultiLayerTransformer().transform(Layer.LAYOUT, p2);
            float x1 = (float) p1.getX();
            float y1 = (float) p1.getY();
            float x2 = (float) p2.getX();
            float y2 = (float) p2.getY();

            GraphicsDecorator g = rc.getGraphicsContext();
            float distX = x2 - x1;
            float distY = y2 - y1;
            double totalLength = Math.sqrt(distX * distX + distY * distY);

            double closeness = rc.getEdgeLabelClosenessTransformer().transform(Context.<Graph<V,E>,E>getInstance(graph, e)).doubleValue();

            int posX = (int) (x1 + (closeness) * distX);
            int posY = (int) (y1 + (closeness) * distY);

            int xDisplacement = (int) (rc.getLabelOffset() * (distY / totalLength));
            int yDisplacement = (int) (rc.getLabelOffset() * (-distX / totalLength));

            Component component = prepareRenderer(rc, rc.getEdgeLabelRenderer(), label, 
                    rc.getPickedEdgeState().isPicked(e), e);

            Dimension d = component.getPreferredSize();

            Shape edgeShape = rc.getEdgeShapeTransformer().transform(Context.<Graph<V,E>,E>getInstance(graph, e));

            double parallelOffset = 1;

            //System.err.println(e.toString() + ": " + rc.getParallelEdgeIndexFunction().getIndex(graph, e));

            if(rc.getParallelEdgeIndexFunction().getIndex(graph, e) > 0){
                parallelOffset += rc.getParallelEdgeIndexFunction().getIndex(graph, e);

                parallelOffset *= d.height;
                if(edgeShape instanceof Ellipse2D) {
                    parallelOffset += edgeShape.getBounds().getHeight();
                    parallelOffset = -parallelOffset;
                }
            }
            else parallelOffset = 20;


            AffineTransform old = g.getTransform();
            AffineTransform xform = new AffineTransform(old);
            xform.translate(posX+xDisplacement, posY+yDisplacement);
            double dx = x2 - x1;
            double dy = y2 - y1;
            if(rc.getEdgeLabelRenderer().isRotateEdgeLabels()) {
                double theta = Math.atan2(dy, dx);
                if(dx < 0) {
                    theta += Math.PI;
                }
                xform.rotate(theta);
            }
            if(dx < 0) {
                parallelOffset = -parallelOffset;
            }

            xform.translate(-d.width/2, -(d.height/2-parallelOffset));
            g.setTransform(xform);
            g.draw(component, rc.getRendererPane(), 0, 0, d.width, d.height, true);

            g.setTransform(old);
        }

    }

}
