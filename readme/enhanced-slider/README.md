# Enhanced slider
![Screenshot](enhanced-slider-widget.png)

This is a slider with a label, which shows current value, and buttons to increase and decrease value.

## Usage

- `enhancedSlider field min max`
- `enhancedSlider_ field min max configs`

Here is the [example](/examples/enhanced-slider/UI.hs).

## Configuration

- `enhancedSlider_ field min max [titleCaption title]`. By default the label only shows current value. `titleCaption` can be used to provide title for the value.
- `enhancedSlider_ field min max [titleMethod f]`, where type of `f` is `a -> Text`. Should be used if the title depends on the value
or different formatting is needed.
- `enhancedSlider_ field min max [hideLabel]`. Should be used when the label with the current value is not needed.
- `... [alignLeft]`. Put horizontal slider to the left of the buttons. Default behavior.
- `... [alignCenter]`. Put horizontal slider between the buttons.
- `... [alignRight]`. Put horizontal slider to the right of the buttons.
- `... [alignTop]`. Put vertical slider to the top of the buttons.
- `... [alignMiddle]`. Put vertical slider between the buttons.
- `... [alignBottom]`. Put vertical slider to the bottom of the buttons.
- `... [onChange f]`, where type of `f` is `a -> e`: `a` is changed value and `e` is the event.
- `... [onFocus f]`, where type of `f` is `Path -> e`: event to raise when the composite receives focus.
- `... [onBlur f]`, where type of `f` is `Path -> e`: event to raise when the composite loses focus.
