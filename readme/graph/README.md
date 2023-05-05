# Graph
![Screenshot](graph-widget.png)

This widget renders a Cartesian coordinate system and plots function graphs by connecting provided points. Coordinate system can be dragged and scaled.

This widget can receive `GraphMsg` messages:
- `GraphSetTranslation Point`
- `GraphSetScale Point`
- `GraphReset`

## Usage

- `graph points`
- `graph_ points configs`

Here is the [example](/examples/graph/UI.hs).

## Configuration

- `[wheelRate r]`. Speed of scaling.
- `[lockX]`. Lock X-axis (scale only Y-axis).
- `[lockY]`. Lock Y-axis (scale only X-axis).
- `[graphColors colors]`. List of colors which are used to plot graphs. This list is then cycled when plotting graphs (in case there are more graphs than provided colors).
