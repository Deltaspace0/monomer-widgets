# Graph
![Screenshot](graph-widget.png)

This widget renders a Cartesian coordinate system and plots function graphs by connecting provided points. Coordinate system can be dragged and scaled. It is possible to render single points too.

This widget can receive `GraphMsg` messages:
- `GraphSetTranslation Point`
- `GraphSetScale Point`
- `GraphReset`

## Usage

- `graph points`
- `graph_ points configs`
- `graphWithColors colorPoints`
- `graphWithColors_ colorPoints configs`
- `graphWithData graphData`
- `graphWithData_ graphData configs`

For example:

- `graph [[(1,2), (1,3)], [(0,0), (1,1)]]`
- `graphWithColors [(red, [(1,2), (1,3)]), (blue, [(0,0), (1,1)])]`
- `graphWithData [[graphPoint (0, 0), graphColor red]]`

Here is the [example](/examples/graph/UI.hs) of an app using this widget.

## GraphData

- `graphPoint (x, y)`. Render single point.
- `graphPoints points`. Use multiple points.
- `graphColor color`. Set the color (if this option is not used then the graph will not be rendered).
- `graphHoverColor color`. Set the color of hovered point (if this option is not used then the color set by `graphColor` is used).
- `graphActiveColor color`. Set the color of dragged point (if this option is not used then the color set by `graphColor` is used).
- `graphWidth width`. Width of the line connecting provided points. If only single point is rendered then its radius will be twice the width.
- `graphSeparate`. Do not connect the points and render them separately. Used when all points in the collection must have the same color.
- `graphFill`. Fill the area surrounded by provided points with the color.
- `graphFillAlpha`. Transparency level of the filled area.
- `graphOnChange f`. Raises an event when a point is dragged by passing its index and new coordinates. This option is ignored if `graphSeparate` is not enabled.
- `graphOnChangeReq f`. Generates a `WidgetRequest` when a point is dragged by passing its index and new coordinates. This option is ignored if `graphSeparate` is not enabled.

## Configuration

- `[wheelRate r]`. Speed of scaling.
- `[minScaleX v]`. Minimum scale along X-axis.
- `[maxScaleX v]`. Maximum scale along X-axis.
- `[minScaleY v]`. Minimum scale along Y-axis.
- `[maxScaleY v]`. Maximum scale along Y-axis.
- `[lockX]`. Lock X-axis (scale only Y-axis).
- `[lockY]`. Lock Y-axis (scale only X-axis).
- `[hideMinorGridlines]`. Hide minor gridlines.
- `[hideAxisNumbers]`. Hide axis numbers.
- `[hideGrid]`. Hide all gridlines and axis numbers.
- `[graphColors colors]`. List of colors which are used to plot graphs. This list is then cycled when plotting graphs (in case there are more graphs than provided colors). It is ignored when using `graphWithColors` or `graphWithData`.
- `[onRightClick f]`. Event to raise on right click.
- `[onRightClickReq f]`. `WidgetRequest` to generate on right click.
