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
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
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
