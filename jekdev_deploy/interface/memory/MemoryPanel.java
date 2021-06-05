package memory;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import javax.swing.*;
import java.awt.*;

/**
 * <p>The memory monitor panel.</p>
 *
 * @author Copyright 2012, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.3 (a fast and small prolog interpreter)
 */
final class MemoryPanel extends JPanel {
    private final static int VALUES_LIMIT = 200;

    private final ChartPanel chartPanel;
    private int count;

    /**
     * <p>Add values to the chart.</p>
     *
     * @param when   The time point.
     * @param values The values.
     */
    void addValues(double when, double[] values) {
        JFreeChart chart = chartPanel.getChart();
        XYPlot plot = chart.getXYPlot();
        XYSeriesCollection dataset = (XYSeriesCollection) plot.getDataset();
        for (int i = 0; i < dataset.getSeriesCount(); i++) {
            XYSeries series = dataset.getSeries(i);
            series.add(when, values[i]);
        }
        count++;
        while (count >= VALUES_LIMIT) {
            for (int i = 0; i < dataset.getSeriesCount(); i++) {
                XYSeries series = dataset.getSeries(i);
                series.delete(0, 0);
            }
            count--;
        }
    }

    /**
     * <p>Create a free chart panel.</p>
     * <p>The layout is basically:</p>
     * <pre>
     *     +--+-----------------------------------+
     *     |N |                                   |
     *     |uA|                                   |
     *     |mx|                                   |
     *     |bi|             XYArea                |
     *     |es|                                   |
     *     |r |                                   |
     *     +--+-----------------------------------+
     *     |  |           NumberAxis              |
     *     +--+-----------------------------------+
     * </pre>
     *
     * @param n The numnber of time series.
     */
    MemoryPanel(int n) {
        XYSeriesCollection dataset = new XYSeriesCollection();
        for (int i = 0; i < n; i++) {
            XYSeries series = new XYSeries(Integer.toString(i));
            dataset.addSeries(series);
        }

        JFreeChart chart = ChartFactory.createXYAreaChart(
                null,
                null,
                null,
                dataset,
                PlotOrientation.VERTICAL,
                false,
                false,
                false
        );

        XYPlot plot = chart.getXYPlot();
        NumberAxis domainAxis = new NumberAxis("secs");
        domainAxis.setAutoRangeIncludesZero(false);
        domainAxis.setLowerMargin(0);
        domainAxis.setUpperMargin(0);
        NumberAxis rangeAxis = new NumberAxis("m Bytes");
        plot.setDomainAxis(domainAxis);
        plot.setRangeAxis(rangeAxis);

        chartPanel = new ChartPanel(chart);
        setLayout(new BorderLayout());
        add(chartPanel, BorderLayout.CENTER);
    }

}
