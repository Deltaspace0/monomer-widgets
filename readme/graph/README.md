# Graph
![Screenshot](graph-widget.png)

This widget renders a Cartesian coordinate system and plots function graphs by connecting provided points. Coordinate system can be dragged and scaled. It is possible to render single points too.

This widget can receive `GraphMsg` messages:
- `GraphSetTranslation Point`
- `GraphSetScale Point`
- `GraphReset`
- `GraphStopAnimations`
- `GraphFinished Int Millisecond`

The last message is used internally.

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
- `graphBorderColor color`. Set the color of point border. The width of the border will be half the width set by `graphWidth`.
- `graphWidth width`. Width of the line connecting provided points. If only single point is rendered then its radius will be twice the width.
- `graphRadius radius`. Radius of the provided points if they are rendered separately. The difference between `graphWidth` and `graphRadius` is that the former is given in pixels and renders the same when the scale changes while the latter is given in units of the Cartesian coordinate system and hence the points become bigger or smaller when the scale changes.
- `graphSeparate`. Do not connect the points and render them separately. Used when all points in the collection must have the same color.
- `graphFill`. Fill the area surrounded by provided points with the color.
- `graphFillAlpha`. Transparency level of the filled area.
- `graphDuration ms`. How long the animation lasts in ms. Animation starts when graph data changes (for example, point positions or color).
- `graphAnimationTwist f`. Function which receives current animation progress (from 0 to 1) and returns modified progress. By default, previous graph data values change with uniform speed to new values during the animation (for example, if the radius is changed from 2 to 6 then at 0.5 animation progress the radius equals to 4).
- `graphKey text`. Identifier of a graph data. It can be used to correctly run an animation when the order of graph datas is changed during merge.
- `graphOnFinished e`. Raises an event when animation is complete.
- `graphOnFinishedReq req`. Generates a `WidgetRequest` when animation is complete.
- `graphOnChange f`. Raises an event when a point is dragged by passing its index and new coordinates. This option is ignored if `graphSeparate` is not enabled.
- `graphOnChangeReq f`. Generates a `WidgetRequest` when a point is dragged by passing its index and new coordinates. This option is ignored if `graphSeparate` is not enabled.
- `graphOnEnter f`. Raises an event when mouse enters point area by passing its index. This option is ignored if `graphSeparate` is not enabled.
- `graphOnEnterReq f`. Generates a `WidgetRequest` when mouse enters point area by passing its index. This option is ignored if `graphSeparate` is not enabled.
- `graphOnLeave f`. Raises an event when mouse leaves point area by passing its index. This option is ignored if `graphSeparate` is not enabled.
- `graphOnLeaveReq f`. Generates a `WidgetRequest` when mouse leaves point area by passing its index. This option is ignored if `graphSeparate` is not enabled.
- `graphOnClick f`. Raises an event when a point is clicked by passing its index. This option is ignored if `graphSeparate` is not enabled.
- `graphOnClickReq f`. Generates a `WidgetRequest` when a point is clicked by passing its index. This option is ignored if `graphSeparate` is not enabled.

## Configuration

- `[wheelRate r]`. Speed of scaling.
- `[limitX (a, b)]`. Limits along X-axis.
- `[limitY (a, b)]`. Limits along Y-axis.
- `[minimumX v]`. Left limit along X-axis.
- `[maximumX v]`. Right limit along X-axis.
- `[minimumY v]`. Bottom limit along Y-axis.
- `[maximumY v]`. Top limit along Y-axis.
- `[minScale v]`. Minimum scale along both X-axis and Y-axis.
- `[maxScale v]`. Maximum scale along both X-axis and Y-axis.
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
