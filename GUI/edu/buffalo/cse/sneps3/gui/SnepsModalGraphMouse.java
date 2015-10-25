/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.buffalo.cse.sneps3.gui;

import edu.uci.ics.jung.algorithms.layout.GraphElementAccessor;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import javax.swing.AbstractAction;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;

/**
 *
 * @author dan
 */
public class SnepsModalGraphMouse<V, E> extends DefaultModalGraphMouse implements MouseListener, MouseMotionListener {

    JPopupMenu popup = new JPopupMenu();

    public SnepsModalGraphMouse() {
        super();
    }

    /**
     * Add support for right clicking verticies to expand them.
     * @param e
     */
    @Override
    public void mousePressed(MouseEvent e) {
        super.mousePressed(e);
        GUI2.getInstance().getGraphPanel().highlightedNodes.clear();
        if (e.getButton() != MouseEvent.BUTTON3) {
            popup.setVisible(false);
        }
        if (e.getButton() == MouseEvent.BUTTON3) {
            VisualizationViewer<V, E> vv = (VisualizationViewer) e.getSource();
            GraphElementAccessor<V, E> pickSupport = vv.getPickSupport();
            Layout<V, E> layout = vv.getGraphLayout();
            Point2D loc = e.getPoint();
            V vertex = pickSupport.getVertex(layout, loc.getX(), loc.getY());
            final JungGraphNode node = (JungGraphNode) vertex;
            if (vertex == null) {
                popup.setVisible(false);
                return;
            } else {
                popup.setVisible(false);
                popup.removeAll();
                int dncsvis = node.getDownCablesetVisibleCount();
                int upcsvis = node.getUpCablesetVisibleCount();
                if (upcsvis < node.getUpCableset().size()) {
                    popup.add(new AbstractAction("Show All In Edges (" + (node.getUpCableset().size() - upcsvis) + " edges)") {

                        public void actionPerformed(ActionEvent e) {
                            JungGraphPanel.instance.showUpCableset(node);
                        }
                    });

                    popup.add(new AbstractAction("Show In Edges By Relation") {

                        public void actionPerformed(ActionEvent e) {
                            HashMap<Caseframe, ArrayList<JungGraphEdge>> hm = JungGraphPanel.instance.getHiddenUpCablesetCfs(node);
                            CaseframeBasedShowHideDialog cd = new CaseframeBasedShowHideDialog(GUI2.getInstance(), new ArrayList(hm.keySet()));
                            cd.setHelpText("   Select the Caseframes you        wish to show in the graph.");
                            cd.setVisible(true);

                            for (Caseframe c : cd.getResult()) {
                                for (JungGraphEdge je : hm.get(c)) {
                                    JungGraphPanel.instance.showNode(je.from);
                                }
                            }

                        }
                    });

                    /*JMenu submenu = new JMenu("Show In Relations");
                    final HashMap<Caseframe, ArrayList<JungGraphEdge>> hm = JungGraphPanel.instance.getHiddenUpCablesetCfs(node);
                    for(final Caseframe cf : hm.keySet()){
                    submenu.add(new AbstractAction(cf.toString()) {
                    public void actionPerformed(ActionEvent e) {
                    for(JungGraphEdge je : hm.get(cf))
                    JungGraphPanel.instance.showNode(je.from);
                    }
                    });
                    }
                    popup.add(submenu);*/
                }
                if (dncsvis < node.getDownCableset().size()) {
                    popup.add(new AbstractAction("Show All Out Edges (" + (node.getDownCableset().size() - dncsvis) + " edges)") {

                        public void actionPerformed(ActionEvent e) {
                            JungGraphPanel.instance.showDownCableset(node);
                        }
                    });
                }

                if (upcsvis > 0) {
                    popup.add(new AbstractAction("Hide All In Edges (" + upcsvis + " edges)") {

                        public void actionPerformed(ActionEvent e) {
                            JungGraphPanel.instance.hideUpCableset(node);
                        }
                    });



                    popup.add(new AbstractAction("Hide In Edges By Relation") {

                        public void actionPerformed(ActionEvent e) {
                            HashMap<Caseframe, ArrayList<JungGraphEdge>> hm = JungGraphPanel.instance.getVisibleUpCablesetCfs(node);
                            CaseframeBasedShowHideDialog cd = new CaseframeBasedShowHideDialog(GUI2.getInstance(), new ArrayList(hm.keySet()));

                            cd.setHelpText("   Select the Caseframes you       wish to hide from the graph.");
                            cd.setVisible(true);

                            for (Caseframe c : cd.getResult()) {
                                for (JungGraphEdge je : hm.get(c)) {
                                    JungGraphPanel.instance.hideNode(je.from);
                                }
                            }

                        }
                    });

                }
                if (dncsvis > 0 && !node.isWftNode()) {
                    popup.add(new AbstractAction("Hide All Out Edges (" + dncsvis + " edges)") {

                        public void actionPerformed(ActionEvent e) {
                            JungGraphPanel.instance.hideDownCableset(node);
                        }
                    });
                }

                popup.add(new AbstractAction("Hide Node") {

                    public void actionPerformed(ActionEvent e) {
                        JungGraphPanel.instance.hideNode(node);
                    }
                });
                if (node.isWftNode() && !node.asserted) {
                    popup.add(new AbstractAction("Assert") {

                        public void actionPerformed(ActionEvent e) {
                            GUI2.getInstance().makeLispCall("(ct::add-to-context '" + node.name + " (currentContext))");
                            node.asserted = true;
                            GUI2.getInstance().getGraphPanel().vv.repaint();
                        }
                    });
                }

                popup.show(vv, e.getX(), e.getY());
            }
        }
    }

    /**
     * Add support for mouse over popups.
     * @param e
     */
    @Override
    public void mouseMoved(MouseEvent e) {
        super.mouseMoved(e);
        VisualizationViewer<V, E> vv = (VisualizationViewer) e.getSource();
        GraphElementAccessor<V, E> pickSupport = vv.getPickSupport();
        Layout<V, E> layout = vv.getGraphLayout();
        Point2D loc = e.getPoint();
        V vertex = pickSupport.getVertex(layout, loc.getX(), loc.getY());
        final JungGraphNode node = (JungGraphNode) vertex;
        if (vertex == null) {
            if (!GUI2.getInstance().showNewAssertions) {
                GUI2.getInstance().getGraphPanel().setStatusbarText("New assertions are not shown in the graph.");
            } else {
                GUI2.getInstance().getGraphPanel().setStatusbarText("");
            }
            return;
        }
        String sbtext = "   " + node.toString();

        if(node.isWftNode()){
            sbtext += " " + node.buildLogicalForm();
        }
        if(!node.upCableset.isEmpty()){
            sbtext += " is in relation" + (node.upCableset.size() == 1 ? "" : "s") + ": " + new HashSet(node.getCaseframesIn()); //Use a HashSet to eliminate duplicates.
        }
        GUI2.getInstance().getGraphPanel().setStatusbarText(sbtext);
    }
}
