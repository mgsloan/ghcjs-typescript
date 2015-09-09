{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Raw where
import qualified GHCJS.TypeScript as TS
import GHCJS.TypeScript (type (:|:), type (:::), type (::?))
import qualified GHCJS.Marshal as GHCJS
import qualified GHCJS.Types as GHCJS
import qualified Data.Typeable
newtype Event = Event (GHCJS.JSRef Event)
newtype HTMLElement = HTMLElement (GHCJS.JSRef HTMLElement)
newtype Function = Function (GHCJS.JSRef Function)

newtype HighchartsAnimation = HighchartsAnimation (GHCJS.JSRef (HighchartsAnimation))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAnimation =
  ('[ "duration" ::? TS.Number
    , "easing" ::? TS.String
    ])

newtype HighchartsAreaChart = HighchartsAreaChart (GHCJS.JSRef (HighchartsAreaChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAreaChart =
  ('[ "allowPointSelect" ::? TS.Boolean
    , "animation" ::? TS.Boolean
    , "color" ::? TS.String
    , "connectEnds" ::? TS.Boolean
    , "connectNulls" ::? TS.Boolean
    , "cropThreshold" ::? TS.Number
    , "cursor" ::? TS.String
    , "dashStyle" ::? TS.String
    , "dataLabels" ::? HighchartsDataLabels
    , "enableMouseTracking" ::? TS.Boolean
    , "events" ::? HighchartsPlotEvents
    , "fillColor" ::? (TS.String :|: HighchartsGradient)
    , "fillOpacity" ::? TS.Number
    , "linkedTo" ::? TS.String
    , "lineColor" ::? TS.String
    , "lineWidth" ::? TS.Number
    , "marker" ::? HighchartsMarker
    , "negativeColor" ::? TS.String
    , "negativeFillColor" ::? TS.String
    , "point" ::? TS.Obj
        ('[ "events" ::: HighchartsPointEvents
          ])
    , "pointInterval" ::? TS.Number
    , "pointPlacement" ::? (TS.String :|: TS.Number)
    , "pointStart" ::? TS.Number
    , "selected" ::? TS.Boolean
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "showCheckbox" ::? TS.Boolean
    , "showInLegend" ::? TS.Boolean
    , "stacking" ::? TS.String
    , "states" ::? TS.Obj
        ('[ "hover" ::: HighchartsAreaStates
          ])
    , "step" ::? TS.String
    , "stickyTracking" ::? TS.Boolean
    , "threshold" ::? TS.Number
    , "tooltip" ::? HighchartsTooltipOptions
    , "trackByArea" ::? TS.Boolean
    , "turboThreshold" ::? TS.Number
    , "visible" ::? TS.Boolean
    , "zIndex" ::? TS.Number
    ])

newtype HighchartsAreaChartSeriesOptions = HighchartsAreaChartSeriesOptions (GHCJS.JSRef (HighchartsAreaChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAreaChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsAreaChart]
  ('[   ])

newtype HighchartsAreaCheckboxEvent = HighchartsAreaCheckboxEvent (GHCJS.JSRef (HighchartsAreaCheckboxEvent))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAreaCheckboxEvent = TS.Extends '[Event]
  ('[ "checked" ::: TS.Boolean
    ])

newtype HighchartsAreaClickEvent = HighchartsAreaClickEvent (GHCJS.JSRef (HighchartsAreaClickEvent))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAreaClickEvent = TS.Extends '[Event]
  ('[ "point" ::: HighchartsPointObject
    ])

newtype HighchartsAreaRangeChart = HighchartsAreaRangeChart (GHCJS.JSRef (HighchartsAreaRangeChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAreaRangeChart =
  ('[ "allowPointSelect" ::? TS.Boolean
    , "animation" ::? TS.Boolean
    , "color" ::? TS.String
    , "connectNulls" ::? TS.Boolean
    , "cropThreshold" ::? TS.Number
    , "cursor" ::? TS.String
    , "dashStyle" ::? TS.String
    , "dataLabels" ::? HighchartsRangeDataLabels
    , "enableMouseTracking" ::? TS.Boolean
    , "events" ::? HighchartsPlotEvents
    , "fillColor" ::? (TS.String :|: HighchartsGradient)
    , "fillOpacity" ::? TS.Number
    , "lineColor" ::? TS.String
    , "lineWidth" ::? TS.Number
    , "linkedTo" ::? TS.String
    , "negativeColor" ::? TS.String
    , "negativeFillColor" ::? TS.String
    , "point" ::? TS.Obj
        ('[ "events" ::: HighchartsPointEvents
          ])
    , "pointInterval" ::? TS.Number
    , "pointPlacement" ::? (TS.String :|: TS.Number)
    , "pointStart" ::? TS.Number
    , "selected" ::? TS.Boolean
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "showCheckbox" ::? TS.Boolean
    , "showInLegend" ::? TS.Boolean
    , "stacking" ::? TS.String
    , "states" ::? TS.Obj
        ('[ "hover" ::: HighchartsAreaStates
          ])
    , "step" ::? TS.String
    , "stickyTracking" ::? TS.Boolean
    , "tooltip" ::? HighchartsTooltipOptions
    , "threshold" ::? TS.Number
    , "trackByArea" ::? TS.Boolean
    , "turboThreshold" ::? TS.Number
    , "visible" ::? TS.Boolean
    ])

newtype HighchartsAreaRangeChartSeriesOptions = HighchartsAreaRangeChartSeriesOptions (GHCJS.JSRef (HighchartsAreaRangeChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAreaRangeChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsAreaRangeChart]
  ('[   ])

newtype HighchartsAreaSplineChart = HighchartsAreaSplineChart (GHCJS.JSRef (HighchartsAreaSplineChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAreaSplineChart = TS.Extends '[HighchartsAreaChart]
  ('[ "connectEnds" ::? TS.Boolean
    ])

newtype HighchartsAreaSplineChartSeriesOptions = HighchartsAreaSplineChartSeriesOptions (GHCJS.JSRef (HighchartsAreaSplineChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAreaSplineChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsAreaSplineChart]
  ('[   ])

newtype HighchartsAreaSplineRangeChart = HighchartsAreaSplineRangeChart (GHCJS.JSRef (HighchartsAreaSplineRangeChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAreaSplineRangeChart = TS.Extends '[HighchartsAreaRangeChart]
  ('[   ])

newtype HighchartsAreaSplineRangeChartSeriesOptions = HighchartsAreaSplineRangeChartSeriesOptions (GHCJS.JSRef (HighchartsAreaSplineRangeChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAreaSplineRangeChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsAreaSplineRangeChart]
  ('[   ])

newtype HighchartsAreaStates = HighchartsAreaStates (GHCJS.JSRef (HighchartsAreaStates))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAreaStates =
  ('[ "enabled" ::? TS.Boolean
    , "lineWidth" ::? TS.Number
    , "marker" ::? HighchartsMarker
    ])

newtype HighchartsAxisEvent = HighchartsAxisEvent (GHCJS.JSRef (HighchartsAxisEvent))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAxisEvent = TS.Extends '[Event]
  ('[ "min" ::: TS.Number
    , "max" ::: TS.Number
    ])

newtype HighchartsAxisLabels = HighchartsAxisLabels (GHCJS.JSRef (HighchartsAxisLabels))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAxisLabels =
  ('[ "align" ::? TS.String
    , "enabled" ::? TS.Boolean
    , "formatter" ::? (TS.String)
    , "overflow" ::? TS.String
    , "rotation" ::? TS.Number
    , "staggerLines" ::? TS.Number
    , "step" ::? TS.Number
    , "style" ::? HighchartsCSSObject
    , "useHTML" ::? TS.Boolean
    , "x" ::? TS.Number
    , "y" ::? TS.Number
    ])

newtype HighchartsAxisObject = HighchartsAxisObject (GHCJS.JSRef (HighchartsAxisObject))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAxisObject =
  ('[ "addPlotBand" ::: TS.Fun (HighchartsPlotBands -> TS.Void)
    , "addPlotLine" ::: TS.Fun (HighchartsPlotLines -> TS.Void)
    , "getExtremes" ::: TS.Fun (HighchartsExtremes)
    , "remove" ::: TS.Fun (TS.Optional (TS.Boolean) -> TS.Void)
    , "removePlotBand" ::: TS.Fun (TS.String -> TS.Void)
    , "removePlotLine" ::: TS.Fun (TS.String -> TS.Void)
    , "setCategories" ::: TS.Fun ((TS.Array TS.String) -> TS.Void)
    , "setCategories" ::: TS.Fun ((TS.Array TS.String) -> TS.Boolean -> TS.Void)
    , "setExtremes" ::: TS.Fun (TS.Number -> TS.Number -> TS.Void)
    , "setExtremes" ::: TS.Fun (TS.Number -> TS.Number -> TS.Boolean -> TS.Void)
    , "setExtremes" ::: TS.Fun (TS.Number -> TS.Number -> TS.Boolean -> (TS.Boolean :|: HighchartsAnimation) -> TS.Void)
    , "setTitle" ::: TS.Fun (HighchartsAxisTitle -> TS.Optional (TS.Boolean) -> TS.Void)
    , "toPixels" ::: TS.Fun (TS.Number -> TS.Optional (TS.Boolean) -> TS.Number)
    , "toValue" ::: TS.Fun (TS.Number -> TS.Optional (TS.Boolean) -> TS.Number)
    , "update" ::: TS.Fun (HighchartsAxisOptions -> TS.Optional (TS.Boolean) -> TS.Void)
    ])

newtype HighchartsAxisOptions = HighchartsAxisOptions (GHCJS.JSRef (HighchartsAxisOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAxisOptions =
  ('[ "allowDecimals" ::? TS.Boolean
    , "alternateGridColor" ::? TS.String
    , "categories" ::? (TS.Array TS.String)
    , "dateTimeLabelFormats" ::? HighchartsDateTimeFormats
    , "endOnTick" ::? TS.Boolean
    , "events" ::? TS.Obj
        ('[ "afterSetExtremes" ::? (HighchartsAxisEvent -> TS.Void)
          , "setExtremes" ::? (HighchartsAxisEvent -> TS.Void)
          ])
    , "gridLineColor" ::? TS.String
    , "gridLineDashStyle" ::? TS.String
    , "gridLineWidth" ::? TS.Number
    , "id" ::? TS.String
    , "labels" ::? HighchartsAxisLabels
    , "lineColor" ::? TS.String
    , "lineWidth" ::? TS.Number
    , "linkedTo" ::? TS.Number
    , "max" ::? TS.Number
    , "maxPadding" ::? TS.Number
    , "maxZoom" ::? TS.Number
    , "min" ::? TS.Number
    , "minPadding" ::? TS.Number
    , "minRange" ::? TS.Number
    , "minTickInterval" ::? TS.Number
    , "minorTickColor" ::? TS.String
    , "minorTickInterval" ::? (TS.Number :|: TS.String)
    , "minorTickLength" ::? TS.Number
    , "minorTickPosition" ::? TS.String
    , "minorTickWidth" ::? TS.Number
    , "offset" ::? TS.Number
    , "opposite" ::? TS.Boolean
    , "plotBands" ::? HighchartsPlotBands
    , "plotLines" ::? HighchartsPlotLines
    , "reversed" ::? TS.Boolean
    , "showEmpty" ::? TS.Boolean
    , "showFirstLabel" ::? TS.Boolean
    , "showLastLabel" ::? TS.Boolean
    , "startOfWeek" ::? TS.Number
    , "startOnTick" ::? TS.Boolean
    , "tickColor" ::? TS.String
    , "tickInterval" ::? TS.Number
    , "tickLength" ::? TS.Number
    , "tickPixelInterval" ::? TS.Number
    , "tickPosition" ::? TS.String
    , "tickWidth" ::? TS.Number
    , "tickmarkPlacement" ::? TS.String
    , "title" ::? HighchartsAxisTitle
    , "type" ::? TS.String
    ])

newtype HighchartsAxisTitle = HighchartsAxisTitle (GHCJS.JSRef (HighchartsAxisTitle))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsAxisTitle =
  ('[ "align" ::? TS.String
    , "margin" ::? TS.Number
    , "offset" ::? TS.Number
    , "rotation" ::? TS.Number
    , "style" ::? HighchartsCSSObject
    , "text" ::? TS.String
    ])

newtype HighchartsBarChart = HighchartsBarChart (GHCJS.JSRef (HighchartsBarChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsBarChart =
  ('[ "allowPointSelect" ::? TS.Boolean
    , "animation" ::? TS.Boolean
    , "borderColor" ::? TS.String
    , "borderRadius" ::? TS.Number
    , "borderWidth" ::? TS.Number
    , "color" ::? TS.String
    , "colorByPoint" ::? TS.Boolean
    , "cropThreshold" ::? TS.Number
    , "colors" ::? (TS.Array TS.String)
    , "cursor" ::? TS.String
    , "dataLabels" ::? HighchartsDataLabels
    , "depth" ::? TS.Number
    , "edgeColor" ::? TS.String
    , "edgeWidth" ::? TS.Number
    , "enableMouseTracking" ::? TS.Boolean
    , "events" ::? HighchartsPlotEvents
    , "groupPadding" ::? TS.Number
    , "groupZPadding" ::? TS.Number
    , "grouping" ::? TS.Boolean
    , "linkedTo" ::? TS.String
    , "minPointLength" ::? TS.Number
    , "negativeColor" ::? TS.String
    , "point" ::? TS.Obj
        ('[ "events" ::: HighchartsPointEvents
          ])
    , "pointInterval" ::? TS.Number
    , "pointPadding" ::? TS.Number
    , "pointPlacement" ::? TS.String
    , "pointRange" ::? TS.Number
    , "pointStart" ::? TS.Number
    , "pointWidth" ::? TS.Number
    , "selected" ::? TS.Boolean
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "showCheckbox" ::? TS.Boolean
    , "showInLegend" ::? TS.Boolean
    , "stacking" ::? TS.String
    , "states" ::? TS.Obj
        ('[ "hover" ::: HighchartsBarStates
          ])
    , "stickyTracking" ::? TS.Boolean
    , "threshold" ::? TS.Number
    , "tooltip" ::? HighchartsTooltipOptions
    , "turboThreshold" ::? TS.Number
    , "visible" ::? TS.Boolean
    ])

newtype HighchartsBarChartSeriesOptions = HighchartsBarChartSeriesOptions (GHCJS.JSRef (HighchartsBarChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsBarChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsBarChart]
  ('[   ])

newtype HighchartsBarStates = HighchartsBarStates (GHCJS.JSRef (HighchartsBarStates))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsBarStates = TS.Extends '[HighchartsAreaStates]
  ('[ "brightness" ::? TS.Number
    ])

newtype HighchartsBoxPlotChart = HighchartsBoxPlotChart (GHCJS.JSRef (HighchartsBoxPlotChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsBoxPlotChart =
  ('[ "allowPointSelect" ::? TS.Boolean
    , "color" ::? TS.String
    , "colorByPoint" ::? TS.Boolean
    , "colors" ::? (TS.Array TS.String)
    , "cursor" ::? TS.String
    , "depth" ::? TS.Number
    , "edgecolor" ::? TS.String
    , "edgewidth" ::? TS.Number
    , "enableMouseTracking" ::? TS.Boolean
    , "events" ::? HighchartsPlotEvents
    , "fillColor" ::? TS.String
    , "groupPadding" ::? TS.Number
    , "groupZPadding" ::? TS.Number
    , "grouping" ::? TS.Boolean
    , "lineWidth" ::? TS.Number
    , "linkedTo" ::? TS.String
    , "medianColor" ::? TS.String
    , "negativeColor" ::? TS.String
    , "point" ::? TS.Obj
        ('[ "events" ::: HighchartsPointEvents
          ])
    , "pointInterval" ::? TS.Number
    , "pointPadding" ::? TS.Number
    , "pointPlacement" ::? (TS.String :|: TS.Number)
    , "pointRange" ::? TS.Number
    , "pointStart" ::? TS.Number
    , "pointWidth" ::? TS.Number
    , "selected" ::? TS.Boolean
    , "showCheckbox" ::? TS.Boolean
    , "showInLegend" ::? TS.Boolean
    , "states" ::? TS.Obj
        ('[ "hover" ::: HighchartsBarStates
          ])
    , "stemColor" ::? TS.String
    , "stemDashStyle" ::? TS.String
    , "stemWidth" ::? TS.Number
    , "stickyTracking" ::? TS.Boolean
    , "tooltip" ::? HighchartsTooltipOptions
    , "turboThreshold" ::? TS.Number
    , "visible" ::? TS.Boolean
    , "whiskerColor" ::? TS.String
    , "whiskerLength" ::? (TS.Number :|: TS.String)
    , "whiskerWidth" ::? TS.Number
    ])

newtype HighchartsBoxPlotChartSeriesOptions = HighchartsBoxPlotChartSeriesOptions (GHCJS.JSRef (HighchartsBoxPlotChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsBoxPlotChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsBoxPlotChart]
  ('[   ])

newtype HighchartsBubbleChart = HighchartsBubbleChart (GHCJS.JSRef (HighchartsBubbleChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsBubbleChart =
  ('[ "allowPointSelect" ::? TS.Boolean
    , "animation" ::? TS.Boolean
    , "color" ::? TS.String
    , "cropThreshold" ::? TS.Number
    , "cursor" ::? TS.String
    , "dashStyle" ::? TS.String
    , "dataLabels" ::? HighchartsRangeDataLabels
    , "displayNegative" ::? TS.Boolean
    , "enableMouseTracking" ::? TS.Boolean
    , "events" ::? HighchartsPlotEvents
    , "lineWidth" ::? TS.Number
    , "linkedTo" ::? TS.String
    , "marker" ::? HighchartsMarker
    , "maxSize" ::? TS.String
    , "minSize" ::? TS.String
    , "negativeColor" ::? TS.String
    , "point" ::? TS.Obj
        ('[ "events" ::: HighchartsPointEvents
          ])
    , "pointInterval" ::? TS.Number
    , "pointStart" ::? TS.Number
    , "selected" ::? TS.Boolean
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "showCheckbox" ::? TS.Boolean
    , "showInLegend" ::? TS.Boolean
    , "sizeBy" ::? TS.String
    , "states" ::? TS.Obj
        ('[ "hover" ::: HighchartsBarStates
          ])
    , "stickyTracking" ::? TS.Boolean
    , "tooltip" ::? HighchartsTooltipOptions
    , "turboThreshold" ::? TS.Number
    , "visible" ::? TS.Boolean
    , "zMax" ::? TS.Number
    , "zMin" ::? TS.Number
    , "zThreshold" ::? TS.Number
    ])

newtype HighchartsBubbleChartSeriesOptions = HighchartsBubbleChartSeriesOptions (GHCJS.JSRef (HighchartsBubbleChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsBubbleChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsBubbleChart]
  ('[   ])

newtype HighchartsButton = HighchartsButton (GHCJS.JSRef (HighchartsButton))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsButton =
  ('[ "align" ::? TS.String
    , "backgroundColor" ::? (TS.String :|: HighchartsGradient)
    , "borderColor" ::? TS.String
    , "borderRadius" ::? TS.Number
    , "borderWidth" ::? TS.Number
    , "verticalAlign" ::? TS.String
    , "enabled" ::? TS.Boolean
    , "height" ::? TS.Number
    , "hoverBorderColor" ::? TS.String
    , "hoverSymbolFill" ::? TS.String
    , "hoverSimbolStroke" ::? TS.String
    , "menuItems" ::? (TS.Array HighchartsMenuItem)
    , "onclick" ::? (TS.Void)
    , "symbol" ::? TS.String
    , "simbolFill" ::? TS.String
    , "simbolSize" ::? TS.Number
    , "symbolStroke" ::? TS.String
    , "symbolStrokeWidth" ::? TS.Number
    , "symbolX" ::? TS.Number
    , "symbolY" ::? TS.Number
    , "width" ::? TS.Number
    , "x" ::? TS.Number
    , "y" ::? TS.Number
    ])

newtype HighchartsCSSObject = HighchartsCSSObject (GHCJS.JSRef (HighchartsCSSObject))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsCSSObject =
  ('[ "background" ::? TS.String
    , "border" ::? TS.String
    , "color" ::? TS.String
    , "cursor" ::? TS.String
    , "font" ::? TS.String
    , "fontSize" ::? TS.String
    , "fontWeight" ::? TS.String
    , "left" ::? TS.String
    , "opacity" ::? TS.Number
    , "padding" ::? TS.String
    , "position" ::? TS.String
    , "top" ::? TS.String
    ])

newtype HighchartsChart = HighchartsChart (GHCJS.JSRef (HighchartsChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsChart =
  ('[ TS.Constructor (HighchartsOptions -> HighchartsChartObject)
    , TS.Constructor (HighchartsOptions -> (HighchartsChartObject -> TS.Void) -> HighchartsChartObject)
    ])

newtype HighchartsChartEvents = HighchartsChartEvents (GHCJS.JSRef (HighchartsChartEvents))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsChartEvents =
  ('[ "addSeries" ::? (TS.Optional (Event) -> TS.Boolean)
    , "click" ::? (TS.Optional (Event) -> TS.Void)
    , "load" ::? (TS.Optional (Event) -> TS.Void)
    , "redraw" ::? (Event -> TS.Void)
    , "selection" ::? (HighchartsSelectionEvent -> TS.Void)
    ])

newtype HighchartsChartObject = HighchartsChartObject (GHCJS.JSRef (HighchartsChartObject))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsChartObject =
  ('[ "addAxis" ::: TS.Fun (HighchartsAxisOptions -> TS.Optional (TS.Boolean) -> TS.Optional (TS.Boolean) -> TS.Optional ((TS.Boolean :|: HighchartsAnimation)) -> HighchartsAxisObject)
    , "addSeries" ::: TS.Fun (TS.Any {- forall t. (t TS.:= HighchartsIndividualSeriesOptions) => t -> TS.Optional (TS.Boolean) -> TS.Optional ((TS.Boolean :|: HighchartsAnimation)) -> t -})
    , "addSeriesAsDrilldown" ::: TS.Fun (HighchartsPointObject -> HighchartsSeriesOptions -> TS.Void)
    , "container" ::: HTMLElement
    , "destroy" ::: TS.Fun (TS.Void)
    , "drillUp" ::: TS.Fun (TS.Void)
    , "exportChart" ::: TS.Fun (TS.Void)
    , "exportChart" ::: TS.Fun (HighchartsExportingOptions -> TS.Void)
    , "exportChart" ::: TS.Fun (HighchartsExportingOptions -> HighchartsChartOptions -> TS.Void)
    , "get" ::: TS.Fun (TS.String -> (HighchartsAxisObject :|: (HighchartsSeriesObject :|: HighchartsPointObject)))
    , "getSVG" ::: TS.Fun (TS.String)
    , "getSVG" ::: TS.Fun (HighchartsChartOptions -> TS.String)
    , "getSelectedPoints" ::: TS.Fun ((TS.Array HighchartsPointObject))
    , "getSelectedSeries" ::: TS.Fun ((TS.Array HighchartsSeriesObject))
    , "hideLoading" ::: TS.Fun (TS.Void)
    , "options" ::: HighchartsChartOptions
    , "print" ::: TS.Fun (TS.Void)
    , "redraw" ::: TS.Fun (TS.Void)
    , "reflow" ::: TS.Fun (TS.Void)
    , "series" ::: (TS.Array HighchartsSeriesObject)
    , "setSize" ::: TS.Fun (TS.Number -> TS.Number -> TS.Void)
    , "setSize" ::: TS.Fun (TS.Number -> TS.Number -> TS.Boolean -> TS.Void)
    , "setSize" ::: TS.Fun (TS.Number -> TS.Number -> HighchartsAnimation -> TS.Void)
    , "setTitle" ::: TS.Fun (HighchartsTitleOptions -> TS.Void)
    , "setTitle" ::: TS.Fun (HighchartsTitleOptions -> HighchartsSubtitleOptions -> TS.Void)
    , "setTitle" ::: TS.Fun (HighchartsTitleOptions -> HighchartsSubtitleOptions -> TS.Boolean -> TS.Void)
    , "showLoading" ::: TS.Fun (TS.Void)
    , "showLoading" ::: TS.Fun (TS.String -> TS.Void)
    , "xAxis" ::: (TS.Array HighchartsAxisObject)
    , "yAxis" ::: (TS.Array HighchartsAxisObject)
    , "renderer" ::: HighchartsRendererObject
    ])

newtype HighchartsChartOptions = HighchartsChartOptions (GHCJS.JSRef (HighchartsChartOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsChartOptions =
  ('[ "alignTicks" ::? TS.Boolean
    , "animation" ::? (TS.Boolean :|: HighchartsAnimation)
    , "backgroundColor" ::? (TS.String :|: HighchartsGradient)
    , "borderColor" ::? TS.String
    , "borderRadius" ::? TS.Number
    , "borderWidth" ::? TS.Number
    , "className" ::? TS.String
    , "defaultSeriesType" ::? TS.String
    , "events" ::? HighchartsChartEvents
    , "height" ::? TS.Number
    , "ignoreHiddenSeries" ::? TS.Boolean
    , "inverted" ::? TS.Boolean
    , "margin" ::? (TS.Array TS.Number)
    , "marginBottom" ::? TS.Number
    , "marginLeft" ::? TS.Number
    , "marginRight" ::? TS.Number
    , "marginTop" ::? TS.Number
    , "plotBackGroundColor" ::? (TS.String :|: HighchartsGradient)
    , "plotBackGroundImage" ::? TS.String
    , "plotBorderColor" ::? TS.String
    , "plotBorderWidth" ::? TS.Number
    , "plotShadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "polar" ::? TS.Boolean
    , "reflow" ::? TS.Boolean
    , "renderTo" ::? TS.Any
    , "resetZoomButton" ::? HighchartsChartResetZoomButton
    , "selectionMarkerFill" ::? TS.String
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "showAxes" ::? TS.Boolean
    , "spacingBottom" ::? TS.Number
    , "spacingLeft" ::? TS.Number
    , "spacingRight" ::? TS.Number
    , "spacingTop" ::? TS.Number
    , "style" ::? HighchartsCSSObject
    , "type" ::? TS.String
    , "width" ::? TS.Number
    , "zoomType" ::? TS.String
    ])

newtype HighchartsChartResetZoomButton = HighchartsChartResetZoomButton (GHCJS.JSRef (HighchartsChartResetZoomButton))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsChartResetZoomButton =
  ('[ "position" ::: HighchartsPosition
    , "relativeTo" ::? TS.String
    , "theme" ::? HighchartsChartResetZoomButtonTheme
    ])

newtype HighchartsChartResetZoomButtonTheme = HighchartsChartResetZoomButtonTheme (GHCJS.JSRef (HighchartsChartResetZoomButtonTheme))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsChartResetZoomButtonTheme =
  ('[ "fill" ::? TS.String
    , "stroke" ::? TS.String
    , "r" ::? TS.Number
    , "states" ::? TS.Any
    , "display" ::? TS.String
    ])

newtype HighchartsColumnChart = HighchartsColumnChart (GHCJS.JSRef (HighchartsColumnChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsColumnChart = TS.Extends '[HighchartsBarChart]
  ('[   ])

newtype HighchartsColumnChartSeriesOptions = HighchartsColumnChartSeriesOptions (GHCJS.JSRef (HighchartsColumnChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsColumnChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsColumnChart]
  ('[   ])

newtype HighchartsColumnRangeChart = HighchartsColumnRangeChart (GHCJS.JSRef (HighchartsColumnRangeChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsColumnRangeChart =
  ('[ "allowPointSelect" ::? TS.Boolean
    , "animation" ::? TS.Boolean
    , "borderColor" ::? TS.String
    , "borderRadius" ::? TS.Number
    , "borderWidth" ::? TS.Number
    , "color" ::? TS.String
    , "colorByPoint" ::? TS.Boolean
    , "colors" ::? (TS.Array TS.String)
    , "cropThreshold" ::? TS.Number
    , "cursor" ::? TS.String
    , "dataLabels" ::? HighchartsRangeDataLabels
    , "enableMouseTracking" ::? TS.Boolean
    , "events" ::? HighchartsPlotEvents
    , "groupPadding" ::? TS.Number
    , "grouping" ::? TS.Boolean
    , "linkedTo" ::? TS.String
    , "minPointLength" ::? TS.Number
    , "negativeColor" ::? TS.String
    , "point" ::? TS.Obj
        ('[ "events" ::: HighchartsPointEvents
          ])
    , "pointInterval" ::? TS.Number
    , "pointPadding" ::? TS.Number
    , "pointPlacement" ::? (TS.String :|: TS.Number)
    , "pointRange" ::? TS.Number
    , "pointStart" ::? TS.Number
    , "pointWidth" ::? TS.Number
    , "selected" ::? TS.Boolean
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "showCheckbox" ::? TS.Boolean
    , "showInLegend" ::? TS.Boolean
    , "stacking" ::? TS.String
    , "states" ::? TS.Obj
        ('[ "hover" ::: HighchartsBarStates
          ])
    , "stickyTracking" ::? TS.Boolean
    , "tooltip" ::? HighchartsTooltipOptions
    , "turboThreshold" ::? TS.Number
    , "visible" ::? TS.Boolean
    ])

newtype HighchartsColumnRangeChartSeriesOptions = HighchartsColumnRangeChartSeriesOptions (GHCJS.JSRef (HighchartsColumnRangeChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsColumnRangeChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsColumnRangeChart]
  ('[   ])

newtype HighchartsCreditsOptions = HighchartsCreditsOptions (GHCJS.JSRef (HighchartsCreditsOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsCreditsOptions =
  ('[ "enabled" ::? TS.Boolean
    , "href" ::? TS.String
    , "position" ::? HighchartsPosition
    , "style" ::? HighchartsCSSObject
    , "text" ::? TS.String
    ])

newtype HighchartsCrosshairObject = HighchartsCrosshairObject (GHCJS.JSRef (HighchartsCrosshairObject))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsCrosshairObject =
  ('[ "color" ::? TS.String
    , "width" ::? TS.Number
    , "dashStyle" ::? TS.String
    , "zIndex" ::? TS.Number
    ])

newtype HighchartsDataLabels = HighchartsDataLabels (GHCJS.JSRef (HighchartsDataLabels))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsDataLabels =
  ('[ "align" ::? TS.String
    , "backgroundColor" ::? (TS.String :|: HighchartsGradient)
    , "borderColor" ::? TS.String
    , "borderRadius" ::? TS.Number
    , "borderWidth" ::? TS.Number
    , "color" ::? TS.String
    , "crop" ::? TS.Boolean
    , "enabled" ::? TS.Boolean
    , "formatter" ::? (TS.Any)
    , "overflow" ::? TS.String
    , "padding" ::? TS.Number
    , "rotation" ::? TS.Number
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "staggerLines" ::? TS.Any
    , "step" ::? TS.Any
    , "style" ::? HighchartsCSSObject
    , "useHTML" ::? TS.Boolean
    , "verticalAlign" ::? TS.String
    , "x" ::? TS.Number
    , "y" ::? TS.Number
    ])

newtype HighchartsDataPoint = HighchartsDataPoint (GHCJS.JSRef (HighchartsDataPoint))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsDataPoint =
  ('[ "color" ::? TS.String
    , "dataLabels" ::? HighchartsDataLabels
    , "events" ::? HighchartsPointEvents
    , "id" ::? TS.String
    , "legendIndex" ::? TS.Number
    , "marker" ::? HighchartsMarker
    , "name" ::? TS.String
    , "sliced" ::? TS.Boolean
    , "x" ::? TS.Number
    , "y" ::? TS.Number
    ])

newtype HighchartsDateTimeFormats = HighchartsDateTimeFormats (GHCJS.JSRef (HighchartsDateTimeFormats))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsDateTimeFormats =
  ('[ "millisecond" ::? TS.String
    , "second" ::? TS.String
    , "minute" ::? TS.String
    , "hour" ::? TS.String
    , "day" ::? TS.String
    , "week" ::? TS.String
    , "month" ::? TS.String
    , "year" ::? TS.String
    ])

newtype HighchartsDial = HighchartsDial (GHCJS.JSRef (HighchartsDial))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsDial =
  ('[ "backgroundColor" ::? TS.String
    , "baseLength" ::? TS.String
    , "baseWidth" ::? TS.Number
    , "borderColor" ::? TS.String
    , "borderWidth" ::? TS.Number
    , "radius" ::? TS.String
    , "rearLength" ::? TS.String
    , "topWidth" ::? TS.Number
    ])

newtype HighchartsElementObject = HighchartsElementObject (GHCJS.JSRef (HighchartsElementObject))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsElementObject =
  ('[ "add" ::: TS.Fun (HighchartsElementObject)
    , "add" ::: TS.Fun (HighchartsElementObject -> HighchartsElementObject)
    , "animate" ::: TS.Fun (TS.Any -> TS.Optional (TS.Any) -> HighchartsElementObject)
    , "attr" ::: TS.Fun (TS.Any -> HighchartsElementObject)
    , "css" ::: TS.Fun (HighchartsCSSObject -> HighchartsElementObject)
    , "destroy" ::: TS.Fun (TS.Void)
    , "getBBox" ::: TS.Fun (TS.Obj
        ('[ "x" ::: TS.Number
          , "y" ::: TS.Number
          , "height" ::: TS.Number
          , "width" ::: TS.Number
          ])
      )
    , "on" ::: TS.Fun (TS.String -> (TS.Void) -> HighchartsElementObject)
    , "toFront" ::: TS.Fun (HighchartsElementObject)
    ])

newtype HighchartsErrorBarChart = HighchartsErrorBarChart (GHCJS.JSRef (HighchartsErrorBarChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsErrorBarChart =
  ('[   ])

newtype HighchartsErrorBarChartSeriesOptions = HighchartsErrorBarChartSeriesOptions (GHCJS.JSRef (HighchartsErrorBarChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsErrorBarChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsErrorBarChart]
  ('[   ])

newtype HighchartsExportingOptions = HighchartsExportingOptions (GHCJS.JSRef (HighchartsExportingOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsExportingOptions =
  ('[ "buttons" ::? TS.Obj
        ('[ "exportButton" ::? HighchartsButton
          , "printButton" ::? HighchartsButton
          ])
    , "enableImages" ::? TS.Boolean
    , "enabled" ::? TS.Boolean
    , "filename" ::? TS.String
    , "type" ::? TS.String
    , "url" ::? TS.String
    , "width" ::? TS.Number
    ])

newtype HighchartsExtremes = HighchartsExtremes (GHCJS.JSRef (HighchartsExtremes))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsExtremes =
  ('[ "dataMax" ::: TS.Number
    , "dataMin" ::: TS.Number
    , "max" ::: TS.Number
    , "min" ::: TS.Number
    ])

newtype HighchartsFunnelChart = HighchartsFunnelChart (GHCJS.JSRef (HighchartsFunnelChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsFunnelChart =
  ('[   ])

newtype HighchartsFunnelChartSeriesOptions = HighchartsFunnelChartSeriesOptions (GHCJS.JSRef (HighchartsFunnelChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsFunnelChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsFunnelChart]
  ('[   ])

newtype HighchartsGaugeChart = HighchartsGaugeChart (GHCJS.JSRef (HighchartsGaugeChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsGaugeChart =
  ('[ "animation" ::? TS.Boolean
    , "color" ::? TS.String
    , "cursor" ::? TS.String
    , "datalabels" ::? HighchartsDataLabels
    , "dial" ::? HighchartsDial
    , "enableMouseTracking" ::? TS.Boolean
    , "events" ::? HighchartsPlotEvents
    , "id" ::? TS.String
    , "pivot" ::? HighchartsPivot
    , "point" ::? TS.Obj
        ('[ "events" ::: HighchartsPointEvents
          ])
    , "selected" ::? TS.Boolean
    , "showCheckbox" ::? TS.Boolean
    , "showInLegend" ::? TS.Boolean
    , "states" ::? TS.Obj
        ('[ "hover" ::: HighchartsAreaStates
          ])
    , "stickyTracking" ::? TS.Boolean
    , "tooltip" ::? HighchartsTooltipOptions
    , "visible" ::? TS.Boolean
    , "zIndex" ::? TS.Number
    ])

newtype HighchartsGaugeChartSeriesOptions = HighchartsGaugeChartSeriesOptions (GHCJS.JSRef (HighchartsGaugeChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsGaugeChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsGaugeChart]
  ('[   ])

newtype HighchartsGlobalObject = HighchartsGlobalObject (GHCJS.JSRef (HighchartsGlobalObject))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsGlobalObject =
  ('[ "Date" ::? TS.Any
    , "VMLRadialGradientURL" ::? TS.String
    , "canvasToolsURL" ::? TS.String
    , "timezoneOffset" ::? TS.Number
    , "useUTC" ::? TS.Boolean
    ])

newtype HighchartsGlobalOptions = HighchartsGlobalOptions (GHCJS.JSRef (HighchartsGlobalOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsGlobalOptions = TS.Extends '[HighchartsOptions]
  ('[ "global" ::? HighchartsGlobalObject
    , "lang" ::? HighchartsLangObject
    ])

newtype HighchartsGradient = HighchartsGradient (GHCJS.JSRef (HighchartsGradient))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsGradient =
  ('[ "linearGradient" ::? TS.Obj
        ('[ "x1" ::: TS.Number
          , "y1" ::: TS.Number
          , "x2" ::: TS.Number
          , "y2" ::: TS.Number
          ])
    , "radialGradient" ::? TS.Obj
        ('[ "cx" ::: TS.Number
          , "cy" ::: TS.Number
          , "r" ::: TS.Number
          ])
    , "stops" ::? (TS.Array (TS.Array TS.Any))
    , "brighten" ::? TS.Fun (TS.Number -> (TS.String :|: HighchartsGradient))
    , "get" ::? TS.Fun (TS.String -> TS.String)
    ])

newtype HighchartsHeatMapChart = HighchartsHeatMapChart (GHCJS.JSRef (HighchartsHeatMapChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsHeatMapChart =
  ('[   ])

newtype HighchartsHeatMapSeriesOptions = HighchartsHeatMapSeriesOptions (GHCJS.JSRef (HighchartsHeatMapSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsHeatMapSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsHeatMapChart]
  ('[   ])

newtype HighchartsIndividualSeriesOptions = HighchartsIndividualSeriesOptions (GHCJS.JSRef (HighchartsIndividualSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsIndividualSeriesOptions =
  ('[ "data" ::? ((TS.Array TS.Number) :|: ((TS.Array (TS.Number, TS.Number)) :|: (TS.Array HighchartsDataPoint)))
    , "id" ::? TS.String
    , "index" ::? TS.Number
    , "legendIndex" ::? TS.Number
    , "name" ::? TS.String
    , "stack" ::? TS.Any
    , "type" ::? TS.String
    , "xAxis" ::? (TS.String :|: TS.Number)
    , "yAxis" ::? (TS.String :|: TS.Number)
    ])

newtype HighchartsLabelItem = HighchartsLabelItem (GHCJS.JSRef (HighchartsLabelItem))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsLabelItem =
  ('[ "html" ::: TS.String
    , "style" ::: HighchartsCSSObject
    ])

newtype HighchartsLabelsOptions = HighchartsLabelsOptions (GHCJS.JSRef (HighchartsLabelsOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsLabelsOptions =
  ('[ "items" ::? (TS.Array HighchartsLabelItem)
    , "style" ::? HighchartsCSSObject
    ])

newtype HighchartsLangObject = HighchartsLangObject (GHCJS.JSRef (HighchartsLangObject))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsLangObject =
  ('[ "contextButtonTitle" ::? TS.String
    , "decimalPoint" ::? TS.String
    , "downloadJPEG" ::? TS.String
    , "downloadPDF" ::? TS.String
    , "downloadPNG" ::? TS.String
    , "downloadSVG" ::? TS.String
    , "drillUpText" ::? TS.String
    , "loading" ::? TS.String
    , "months" ::? (TS.Array TS.String)
    , "noData" ::? TS.String
    , "numericSymbols" ::? (TS.Array TS.String)
    , "printChart" ::? TS.String
    , "resetZoom" ::? TS.String
    , "resetZoomTitle" ::? TS.String
    , "shortMonths" ::? (TS.Array TS.String)
    , "thousandsSep" ::? TS.String
    , "weekdays" ::? (TS.Array TS.String)
    ])

newtype HighchartsLegendNavigationOptions = HighchartsLegendNavigationOptions (GHCJS.JSRef (HighchartsLegendNavigationOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsLegendNavigationOptions =
  ('[ "activeColor" ::? TS.String
    , "animation" ::? (TS.Boolean :|: HighchartsAnimation)
    , "arrowSize" ::? TS.Number
    , "inactiveColor" ::? TS.String
    , "style" ::? HighchartsCSSObject
    ])

newtype HighchartsLegendOptions = HighchartsLegendOptions (GHCJS.JSRef (HighchartsLegendOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsLegendOptions =
  ('[ "align" ::? TS.String
    , "backgroundColor" ::? (TS.String :|: HighchartsGradient)
    , "borderColor" ::? TS.String
    , "borderRadius" ::? TS.Number
    , "borderWidth" ::? TS.Number
    , "enabled" ::? TS.Boolean
    , "floating" ::? TS.Boolean
    , "itemHiddenStyle" ::? HighchartsCSSObject
    , "itemHoverStyle" ::? HighchartsCSSObject
    , "itemMarginBottom" ::? TS.Number
    , "itemMarginTop" ::? TS.Number
    , "itemStyle" ::? HighchartsCSSObject
    , "itemWidth" ::? TS.Number
    , "labelFormatter" ::? (TS.String)
    , "layout" ::? TS.String
    , "lineHeight" ::? TS.String
    , "margin" ::? TS.Number
    , "maxHeight" ::? TS.Number
    , "navigation" ::? HighchartsLegendNavigationOptions
    , "padding" ::? TS.Number
    , "reversed" ::? TS.Boolean
    , "rtl" ::? TS.Boolean
    , "verticalAlign" ::? TS.String
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "style" ::? HighchartsCSSObject
    , "symbolPadding" ::? TS.Number
    , "symbolWidth" ::? TS.Number
    , "useHTML" ::? TS.Boolean
    , "width" ::? TS.Number
    , "x" ::? TS.Number
    , "y" ::? TS.Number
    ])

newtype HighchartsLineChart = HighchartsLineChart (GHCJS.JSRef (HighchartsLineChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsLineChart =
  ('[ "allowPointSelect" ::? TS.Boolean
    , "animation" ::? TS.Boolean
    , "color" ::? TS.String
    , "connectEnds" ::? TS.Boolean
    , "connectNulls" ::? TS.Boolean
    , "cropThreshold" ::? TS.Number
    , "cursor" ::? TS.String
    , "dashStyle" ::? TS.String
    , "dataLabels" ::? HighchartsDataLabels
    , "enableMouseTracking" ::? TS.Boolean
    , "events" ::? HighchartsPlotEvents
    , "id" ::? TS.String
    , "lineWidth" ::? TS.Number
    , "marker" ::? HighchartsMarker
    , "point" ::? TS.Obj
        ('[ "events" ::: HighchartsPointEvents
          ])
    , "pointInterval" ::? TS.Number
    , "pointPlacement" ::? TS.String
    , "pointStart" ::? TS.Number
    , "selected" ::? TS.Boolean
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "showCheckbox" ::? TS.Boolean
    , "showInLegend" ::? TS.Boolean
    , "stacking" ::? TS.String
    , "states" ::? TS.Obj
        ('[ "hover" ::: HighchartsAreaStates
          ])
    , "step" ::? (TS.Boolean :|: TS.String)
    , "stickyTracking" ::? TS.Boolean
    , "tooltip" ::? HighchartsTooltipOptions
    , "turboThreshold" ::? TS.Number
    , "visible" ::? TS.Boolean
    , "zIndex" ::? TS.Number
    ])

newtype HighchartsLineChartSeriesOptions = HighchartsLineChartSeriesOptions (GHCJS.JSRef (HighchartsLineChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsLineChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsLineChart]
  ('[   ])

newtype HighchartsLoadingOptions = HighchartsLoadingOptions (GHCJS.JSRef (HighchartsLoadingOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsLoadingOptions =
  ('[ "hideDuration" ::? TS.Number
    , "labelStyle" ::? HighchartsCSSObject
    , "showDuration" ::? TS.Number
    , "style" ::? HighchartsCSSObject
    ])

newtype HighchartsMarker = HighchartsMarker (GHCJS.JSRef (HighchartsMarker))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsMarker = TS.Extends '[HighchartsMarkerState]
  ('[ "states" ::? TS.Obj
        ('[ "hover" ::? HighchartsMarkerState
          , "select" ::? HighchartsMarkerState
          ])
    , "symbol" ::? TS.String
    ])

newtype HighchartsMarkerState = HighchartsMarkerState (GHCJS.JSRef (HighchartsMarkerState))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsMarkerState =
  ('[ "enabled" ::? TS.Boolean
    , "fillColor" ::? TS.String
    , "lineColor" ::? TS.String
    , "lineWidth" ::? TS.Number
    , "radius" ::? TS.Number
    ])

newtype HighchartsMenuItem = HighchartsMenuItem (GHCJS.JSRef (HighchartsMenuItem))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsMenuItem =
  ('[ "text" ::: TS.String
    , "onclick" ::: (TS.Void)
    ])

newtype HighchartsMousePlotEvents = HighchartsMousePlotEvents (GHCJS.JSRef (HighchartsMousePlotEvents))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsMousePlotEvents =
  ('[ "click" ::? (TS.Optional (Event) -> TS.Void)
    , "mouseover" ::? (TS.Optional (Event) -> TS.Void)
    , "mouseout" ::? (TS.Optional (Event) -> TS.Void)
    , "mousemove" ::? (TS.Optional (Event) -> TS.Void)
    ])

newtype HighchartsNavigationOptions = HighchartsNavigationOptions (GHCJS.JSRef (HighchartsNavigationOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsNavigationOptions =
  ('[ "buttonOptions" ::? HighchartsButton
    , "menuItemHoverStyle" ::? HighchartsCSSObject
    , "menuItemStyle" ::? HighchartsCSSObject
    , "menuStyle" ::? HighchartsCSSObject
    ])

newtype HighchartsOptions = HighchartsOptions (GHCJS.JSRef (HighchartsOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsOptions =
  ('[ "chart" ::? HighchartsChartOptions
    , "colors" ::? (TS.Array TS.String)
    , "credits" ::? HighchartsCreditsOptions
    , "data" ::? TS.Any
    , "drilldown" ::? TS.Any
    , "exporting" ::? HighchartsExportingOptions
    , "labels" ::? HighchartsLabelsOptions
    , "legend" ::? HighchartsLegendOptions
    , "loading" ::? HighchartsLoadingOptions
    , "navigation" ::? HighchartsNavigationOptions
    , "noData" ::? TS.Any
    , "pane" ::? HighchartsPaneOptions
    , "plotOptions" ::? HighchartsPlotOptions
    , "series" ::? (TS.Array HighchartsIndividualSeriesOptions)
    , "subtitle" ::? HighchartsSubtitleOptions
    , "title" ::? HighchartsTitleOptions
    , "tooltip" ::? HighchartsTooltipOptions
    , "xAxis" ::? HighchartsAxisOptions
    , "yAxis" ::? HighchartsAxisOptions
    ])

newtype HighchartsPaneBackground = HighchartsPaneBackground (GHCJS.JSRef (HighchartsPaneBackground))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPaneBackground =
  ('[ "backgroundColor" ::: (TS.String :|: HighchartsGradient)
    , "borderColor" ::? TS.String
    , "borderWidth" ::? TS.Number
    , "innerRadius" ::? TS.String
    , "outerRadius" ::? TS.String
    ])

newtype HighchartsPaneOptions = HighchartsPaneOptions (GHCJS.JSRef (HighchartsPaneOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPaneOptions =
  ('[ "background" ::? (TS.Array HighchartsPaneBackground)
    , "center" ::? ((TS.Number :|: TS.String), (TS.Number :|: TS.String))
    , "endAngle" ::? TS.Number
    , "size" ::? (TS.Number :|: TS.String)
    , "startAngle" ::? TS.Number
    ])

newtype HighchartsPieChart = HighchartsPieChart (GHCJS.JSRef (HighchartsPieChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPieChart =
  ('[ "allowPointSelect" ::? TS.Boolean
    , "animation" ::? TS.Boolean
    , "borderColor" ::? TS.String
    , "borderWidth" ::? TS.Number
    , "center" ::? (TS.Array TS.String)
    , "color" ::? TS.String
    , "cursor" ::? TS.String
    , "dataLabels" ::? HighchartsDataLabels
    , "enableMouseTracking" ::? TS.Boolean
    , "events" ::? HighchartsPlotEvents
    , "id" ::? TS.String
    , "ignoreHiddenPoint" ::? TS.Boolean
    , "innerSize" ::? (TS.Number :|: TS.String)
    , "lineWidth" ::? TS.Number
    , "marker" ::? HighchartsMarker
    , "point" ::? TS.Obj
        ('[ "events" ::: HighchartsPointEvents
          ])
    , "pointPlacement" ::? TS.String
    , "selected" ::? TS.Boolean
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "showInLegend" ::? TS.Boolean
    , "size" ::? (TS.Number :|: TS.String)
    , "slicedOffset" ::? TS.Number
    , "states" ::? TS.Obj
        ('[ "hover" ::: HighchartsAreaStates
          ])
    , "stickyTracking" ::? TS.Boolean
    , "tooltip" ::? HighchartsTooltipOptions
    , "visible" ::? TS.Boolean
    , "zIndex" ::? TS.Number
    ])

newtype HighchartsPieChartSeriesOptions = HighchartsPieChartSeriesOptions (GHCJS.JSRef (HighchartsPieChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPieChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsPieChart]
  ('[   ])

newtype HighchartsPivot = HighchartsPivot (GHCJS.JSRef (HighchartsPivot))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPivot =
  ('[ "backgroundColor" ::? TS.String
    , "borderColor" ::? TS.String
    , "borderWidth" ::? TS.Number
    , "radius" ::? TS.Number
    ])

newtype HighchartsPlotBands = HighchartsPlotBands (GHCJS.JSRef (HighchartsPlotBands))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPlotBands =
  ('[ "color" ::? TS.String
    , "events" ::? HighchartsMousePlotEvents
    , "from" ::? TS.Number
    , "id" ::? TS.String
    , "label" ::? HighchartsPlotLabel
    , "to" ::? TS.Number
    , "zIndex" ::? TS.Number
    ])

newtype HighchartsPlotEvents = HighchartsPlotEvents (GHCJS.JSRef (HighchartsPlotEvents))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPlotEvents =
  ('[ "checkboxClick" ::? (HighchartsAreaCheckboxEvent -> TS.Boolean)
    , "click" ::? (HighchartsAreaClickEvent -> TS.Void)
    , "hide" ::? (TS.Void)
    , "legendItemClick" ::? (Event -> TS.Boolean)
    , "mouseOut" ::? (Event -> TS.Void)
    , "mouseOver" ::? (Event -> TS.Void)
    , "show" ::? (TS.Void)
    ])

newtype HighchartsPlotLabel = HighchartsPlotLabel (GHCJS.JSRef (HighchartsPlotLabel))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPlotLabel =
  ('[ "align" ::? TS.String
    , "rotation" ::? TS.Number
    , "style" ::? HighchartsCSSObject
    , "text" ::? TS.String
    , "textAlign" ::? TS.String
    , "x" ::? TS.Number
    , "y" ::? TS.Number
    ])

newtype HighchartsPlotLines = HighchartsPlotLines (GHCJS.JSRef (HighchartsPlotLines))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPlotLines =
  ('[ "color" ::? TS.String
    , "dashStyle" ::? TS.String
    , "events" ::? HighchartsMousePlotEvents
    , "id" ::? TS.String
    , "label" ::? HighchartsPlotLabel
    , "value" ::? TS.Number
    , "width" ::? TS.Number
    , "zIndex" ::? TS.Number
    ])

newtype HighchartsPlotOptions = HighchartsPlotOptions (GHCJS.JSRef (HighchartsPlotOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPlotOptions =
  ('[ "area" ::? HighchartsAreaChart
    , "arearange" ::? HighchartsAreaRangeChart
    , "areaspline" ::? HighchartsAreaSplineChart
    , "areasplinerange" ::? HighchartsAreaSplineRangeChart
    , "bar" ::? HighchartsBarChart
    , "boxplot" ::? HighchartsBoxPlotChart
    , "bubble" ::? HighchartsBubbleChart
    , "column" ::? HighchartsColumnChart
    , "columnrange" ::? HighchartsColumnRangeChart
    , "errorbar" ::? HighchartsErrorBarChart
    , "funnel" ::? HighchartsFunnelChart
    , "gauge" ::? HighchartsGaugeChart
    , "heatmap" ::? HighchartsHeatMapChart
    , "line" ::? HighchartsLineChart
    , "pie" ::? HighchartsPieChart
    , "polygon" ::? HighchartsPolygonChart
    , "pyramid" ::? HighchartsPyramidChart
    , "scatter" ::? HighchartsScatterChart
    , "series" ::? HighchartsSeriesChart
    , "solidgauge" ::? HighchartsSolidGaugeChart
    , "spline" ::? HighchartsSplineChart
    , "treemap" ::? HighchartsTreeMapChart
    , "waterfall" ::? HighchartsWaterFallChart
    ])

newtype HighchartsPlotPoint = HighchartsPlotPoint (GHCJS.JSRef (HighchartsPlotPoint))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPlotPoint =
  ('[ "plotX" ::: TS.Number
    , "plotY" ::: TS.Number
    ])

newtype HighchartsPointEvents = HighchartsPointEvents (GHCJS.JSRef (HighchartsPointEvents))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPointEvents =
  ('[ "click" ::? (Event -> TS.Boolean)
    , "mouseOut" ::? (Event -> TS.Void)
    , "mouseOver" ::? (Event -> TS.Void)
    , "remove" ::? (Event -> TS.Boolean)
    , "select" ::? (Event -> TS.Boolean)
    , "unselect" ::? (Event -> TS.Boolean)
    , "update" ::? (Event -> TS.Boolean)
    ])

newtype HighchartsPointObject = HighchartsPointObject (GHCJS.JSRef (HighchartsPointObject))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPointObject =
  ('[ "category" ::: (TS.String :|: TS.Number)
    , "percentage" ::: TS.Number
    , "remove" ::: TS.Fun (TS.Optional (TS.Boolean) -> TS.Optional ((TS.Boolean :|: HighchartsAnimation)) -> TS.Void)
    , "select" ::: TS.Fun (TS.Void)
    , "select" ::: TS.Fun (TS.Boolean -> TS.Void)
    , "select" ::: TS.Fun (TS.Boolean -> TS.Boolean -> TS.Void)
    , "selected" ::: TS.Boolean
    , "series" ::: HighchartsSeriesObject
    , "slice" ::: TS.Fun (TS.Optional (TS.Boolean) -> TS.Optional (TS.Boolean) -> TS.Optional ((TS.Boolean :|: HighchartsAnimation)) -> TS.Void)
    , "total" ::: TS.Number
    , "update" ::: TS.Fun ((TS.Number :|: ((TS.Number, TS.Number) :|: HighchartsDataPoint)) -> TS.Optional (TS.Boolean) -> TS.Optional ((TS.Boolean :|: HighchartsAnimation)) -> TS.Void)
    , "x" ::: TS.Number
    , "y" ::: TS.Number
    ])

newtype HighchartsPolygonChart = HighchartsPolygonChart (GHCJS.JSRef (HighchartsPolygonChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPolygonChart =
  ('[   ])

newtype HighchartsPolygonChartSeriesOptions = HighchartsPolygonChartSeriesOptions (GHCJS.JSRef (HighchartsPolygonChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPolygonChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsPolygonChart]
  ('[   ])

newtype HighchartsPosition = HighchartsPosition (GHCJS.JSRef (HighchartsPosition))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPosition =
  ('[ "align" ::? TS.String
    , "verticalAlign" ::? TS.String
    , "x" ::? TS.Number
    , "y" ::? TS.Number
    ])

newtype HighchartsPyramidChart = HighchartsPyramidChart (GHCJS.JSRef (HighchartsPyramidChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPyramidChart =
  ('[   ])

newtype HighchartsPyramidChartSeriesOptions = HighchartsPyramidChartSeriesOptions (GHCJS.JSRef (HighchartsPyramidChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsPyramidChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsPyramidChart]
  ('[   ])

newtype HighchartsRangeDataLabels = HighchartsRangeDataLabels (GHCJS.JSRef (HighchartsRangeDataLabels))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsRangeDataLabels =
  ('[ "align" ::? TS.String
    , "backgroundColor" ::? (TS.String :|: HighchartsGradient)
    , "borderColor" ::? TS.String
    , "borderRadius" ::? TS.Number
    , "borderWidth" ::? TS.Number
    , "color" ::? TS.String
    , "crop" ::? TS.Boolean
    , "defer" ::? TS.Boolean
    , "enabled" ::? TS.Boolean
    , "format" ::? TS.String
    , "formatter" ::? (TS.Any)
    , "inside" ::? TS.Boolean
    , "overflow" ::? TS.String
    , "padding" ::? TS.Number
    , "rotation" ::? TS.Number
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "style" ::? HighchartsCSSObject
    , "useHTML" ::? TS.Boolean
    , "verticalAlign" ::? TS.String
    , "xHigh" ::? TS.Number
    , "xLow" ::? TS.Number
    , "yHigh" ::? TS.Number
    , "yLow" ::? TS.Number
    , "zIndex" ::? TS.Number
    ])

newtype HighchartsRenderer = HighchartsRenderer (GHCJS.JSRef (HighchartsRenderer))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsRenderer =
  ('[ TS.Constructor (HTMLElement -> TS.Number -> TS.Number -> HighchartsRendererObject)
    ])

newtype HighchartsRendererObject = HighchartsRendererObject (GHCJS.JSRef (HighchartsRendererObject))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsRendererObject =
  ('[ "arc" ::: TS.Fun (TS.Number -> TS.Number -> TS.Number -> TS.Number -> TS.Number -> TS.Number -> HighchartsElementObject)
    , "circle" ::: TS.Fun (TS.Number -> TS.Number -> TS.Number -> HighchartsElementObject)
    , "g" ::: TS.Fun (TS.String -> HighchartsElementObject)
    , "image" ::: TS.Fun (TS.String -> TS.Number -> TS.Number -> TS.Number -> TS.Number -> HighchartsElementObject)
    , "path" ::: TS.Fun ((TS.Array TS.Any) -> HighchartsElementObject)
    , "rect" ::: TS.Fun (TS.Number -> TS.Number -> TS.Number -> TS.Number -> TS.Number -> HighchartsElementObject)
    , "text" ::: TS.Fun (TS.String -> TS.Number -> TS.Number -> HighchartsElementObject)
    ])

newtype HighchartsScatterChart = HighchartsScatterChart (GHCJS.JSRef (HighchartsScatterChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsScatterChart =
  ('[ "allowPointSelect" ::? TS.Boolean
    , "animation" ::? TS.Boolean
    , "color" ::? TS.String
    , "connectNulls" ::? TS.Boolean
    , "cropThreshold" ::? TS.Number
    , "cursor" ::? TS.String
    , "dashStyle" ::? TS.String
    , "dataLabels" ::? HighchartsDataLabels
    , "enableMouseTracking" ::? TS.Boolean
    , "events" ::? HighchartsPlotEvents
    , "id" ::? TS.String
    , "lineWidth" ::? TS.Number
    , "marker" ::? HighchartsMarker
    , "point" ::? TS.Obj
        ('[ "events" ::: HighchartsPointEvents
          ])
    , "pointInterval" ::? TS.Number
    , "pointPlacement" ::? TS.String
    , "pointStart" ::? TS.Number
    , "selected" ::? TS.Boolean
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "showCheckbox" ::? TS.Boolean
    , "showInLegend" ::? TS.Boolean
    , "states" ::? TS.Obj
        ('[ "hover" ::: HighchartsAreaStates
          ])
    , "stickyTracking" ::? TS.Boolean
    , "tooltip" ::? HighchartsTooltipOptions
    , "turboThreshold" ::? TS.Number
    , "visible" ::? TS.Boolean
    , "zIndex" ::? TS.Number
    ])

newtype HighchartsScatterChartSeriesOptions = HighchartsScatterChartSeriesOptions (GHCJS.JSRef (HighchartsScatterChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsScatterChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsScatterChart]
  ('[   ])

newtype HighchartsSelectionEvent = HighchartsSelectionEvent (GHCJS.JSRef (HighchartsSelectionEvent))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsSelectionEvent = TS.Extends '[Event]
  ('[ "xAxis" ::: (TS.Array HighchartsAxisOptions)
    , "yAxis" ::: (TS.Array HighchartsAxisOptions)
    ])

newtype HighchartsSeriesChart = HighchartsSeriesChart (GHCJS.JSRef (HighchartsSeriesChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsSeriesChart =
  ('[ "allowPointSelect" ::? TS.Boolean
    , "animation" ::? TS.Boolean
    , "color" ::? TS.String
    , "connectEnds" ::? TS.Boolean
    , "connectNulls" ::? TS.Boolean
    , "cropThreshold" ::? TS.Number
    , "cursor" ::? TS.String
    , "dashStyle" ::? TS.String
    , "dataLabels" ::? HighchartsDataLabels
    , "enableMouseTracking" ::? TS.Boolean
    , "events" ::? HighchartsPlotEvents
    , "lineWidth" ::? TS.Number
    , "marker" ::? HighchartsMarker
    , "point" ::? TS.Obj
        ('[ "events" ::: HighchartsPointEvents
          ])
    , "pointInterval" ::? TS.Number
    , "pointPlacement" ::? TS.String
    , "pointStart" ::? TS.Number
    , "selected" ::? TS.Boolean
    , "shadow" ::? (TS.Boolean :|: HighchartsShadow)
    , "showCheckbox" ::? TS.Boolean
    , "showInLegend" ::? TS.Boolean
    , "stacking" ::? TS.String
    , "states" ::? TS.Obj
        ('[ "hover" ::: HighchartsAreaStates
          ])
    , "stickyTracking" ::? TS.Boolean
    , "tooltip" ::? HighchartsTooltipOptions
    , "turboThreshold" ::? TS.Number
    , "visible" ::? TS.Boolean
    , "zIndex" ::? TS.Number
    ])

newtype HighchartsSeriesObject = HighchartsSeriesObject (GHCJS.JSRef (HighchartsSeriesObject))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsSeriesObject =
  ('[ "addPoint" ::: TS.Fun ((TS.Number :|: ((TS.Number, TS.Number) :|: HighchartsDataPoint)) -> TS.Optional (TS.Boolean) -> TS.Optional (TS.Boolean) -> TS.Optional ((TS.Boolean :|: HighchartsAnimation)) -> TS.Void)
    , "chart" ::: HighchartsChartObject
    , "data" ::: (TS.Array HighchartsPointObject)
    , "hide" ::: TS.Fun (TS.Void)
    , "name" ::: TS.String
    , "options" ::: HighchartsSeriesOptions
    , "remove" ::: TS.Fun (TS.Optional (TS.Boolean) -> TS.Void)
    , "select" ::: TS.Fun (TS.Optional (TS.Boolean) -> TS.Void)
    , "selected" ::: TS.Boolean
    , "setData" ::: TS.Fun (((TS.Array TS.Number) :|: ((TS.Array (TS.Array TS.Number)) :|: (TS.Array HighchartsDataPoint))) -> TS.Optional (TS.Boolean) -> TS.Optional ((TS.Boolean :|: HighchartsAnimation)) -> TS.Optional (TS.Boolean) -> TS.Void)
    , "setVisible" ::: TS.Fun (TS.Boolean -> TS.Optional (TS.Boolean) -> TS.Void)
    , "show" ::: TS.Fun (TS.Void)
    , "type" ::: TS.String
    , "update" ::: TS.Fun (HighchartsSeriesOptions -> TS.Optional (TS.Boolean) -> TS.Void)
    , "visible" ::: TS.Boolean
    , "xAxis" ::: HighchartsAxisObject
    , "yAxis" ::: HighchartsAxisObject
    ])

newtype HighchartsSeriesOptions = HighchartsSeriesOptions (GHCJS.JSRef (HighchartsSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsSeriesOptions = TS.Extends '[HighchartsSeriesChart]
  ('[ "type" ::? TS.String
    , "data" ::? ((TS.Array TS.Number) :|: ((TS.Array (TS.Array TS.Number)) :|: (TS.Array HighchartsDataPoint)))
    , "index" ::? TS.Number
    , "legendIndex" ::? TS.Number
    , "name" ::? TS.String
    , "stack" ::? (TS.String :|: TS.Number)
    , "xAxis" ::? (TS.String :|: TS.Number)
    , "yAxis" ::? (TS.String :|: TS.Number)
    ])

newtype HighchartsShadow = HighchartsShadow (GHCJS.JSRef (HighchartsShadow))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsShadow =
  ('[ "color" ::? TS.String
    , "offsetX" ::? TS.Number
    , "offsetY" ::? TS.Number
    , "opacity" ::? TS.Number
    , "width" ::? TS.Number
    ])

newtype HighchartsSolidGaugeChart = HighchartsSolidGaugeChart (GHCJS.JSRef (HighchartsSolidGaugeChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsSolidGaugeChart =
  ('[   ])

newtype HighchartsSolidGaugeChartSeriesOptions = HighchartsSolidGaugeChartSeriesOptions (GHCJS.JSRef (HighchartsSolidGaugeChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsSolidGaugeChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsSolidGaugeChart]
  ('[   ])

newtype HighchartsSplineChart = HighchartsSplineChart (GHCJS.JSRef (HighchartsSplineChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsSplineChart = TS.Extends '[HighchartsSeriesChart]
  ('[   ])

newtype HighchartsSplineChartSeriesOptions = HighchartsSplineChartSeriesOptions (GHCJS.JSRef (HighchartsSplineChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsSplineChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsSplineChart]
  ('[   ])

newtype HighchartsStatic = HighchartsStatic (GHCJS.JSRef (HighchartsStatic))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsStatic =
  ('[ "Chart" ::: HighchartsChart
    , "Renderer" ::: HighchartsRenderer
    , "Color" ::: TS.Fun ((TS.String :|: HighchartsGradient) -> (TS.String :|: HighchartsGradient))
    , "dateFormat" ::: TS.Fun (TS.String -> TS.Optional (TS.Number) -> TS.Optional (TS.Boolean) -> TS.String)
    , "numberFormat" ::: TS.Fun (TS.Number -> TS.Optional (TS.Number) -> TS.Optional (TS.String) -> TS.Optional (TS.String) -> TS.String)
    , "setOptions" ::: TS.Fun (HighchartsGlobalOptions -> HighchartsOptions)
    , "getOptions" ::: TS.Fun (HighchartsOptions)
    , "map" ::: TS.Fun ((TS.Array TS.Any) -> Function -> (TS.Array TS.Any))
    ])

newtype HighchartsSubtitleOptions = HighchartsSubtitleOptions (GHCJS.JSRef (HighchartsSubtitleOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsSubtitleOptions =
  ('[ "align" ::? TS.String
    , "verticalAlign" ::? TS.String
    , "floating" ::? TS.Boolean
    , "style" ::? HighchartsCSSObject
    , "text" ::? TS.String
    , "useHTML" ::? TS.Boolean
    , "x" ::? TS.Number
    , "y" ::? TS.Number
    ])

newtype HighchartsTitleOptions = HighchartsTitleOptions (GHCJS.JSRef (HighchartsTitleOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsTitleOptions = TS.Extends '[HighchartsSubtitleOptions]
  ('[ "margin" ::? TS.Number
    ])

newtype HighchartsTooltipOptions = HighchartsTooltipOptions (GHCJS.JSRef (HighchartsTooltipOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsTooltipOptions =
  ('[ "animation" ::? TS.Boolean
    , "backgroundColor" ::? (TS.String :|: HighchartsGradient)
    , "borderColor" ::? TS.String
    , "borderRadius" ::? TS.Number
    , "borderWidth" ::? TS.Number
    , "crosshairs" ::? (TS.Boolean :|: ((TS.Boolean, TS.Boolean) :|: (HighchartsCrosshairObject :|: (HighchartsCrosshairObject, HighchartsCrosshairObject))))
    , "enabled" ::? TS.Boolean
    , "footerFormat" ::? TS.String
    , "formatter" ::? (TS.Any)
    , "pointFormat" ::? TS.String
    , "positioner" ::? (TS.Number -> TS.Number -> HighchartsPlotPoint -> TS.Obj
        ('[ "x" ::: TS.Number
          , "y" ::: TS.Number
          ])
      )
    , "shadow" ::? TS.Boolean
    , "shared" ::? TS.Boolean
    , "snap" ::? TS.Number
    , "style" ::? HighchartsCSSObject
    , "useHTML" ::? TS.Boolean
    , "valueDecimals" ::? TS.Number
    , "valuePrefix" ::? TS.String
    , "valueSuffix" ::? TS.String
    , "xDateFormat" ::? TS.String
    ])

newtype HighchartsTreeMapChart = HighchartsTreeMapChart (GHCJS.JSRef (HighchartsTreeMapChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsTreeMapChart =
  ('[   ])

newtype HighchartsTreeMapChartSeriesOptions = HighchartsTreeMapChartSeriesOptions (GHCJS.JSRef (HighchartsTreeMapChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsTreeMapChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsTreeMapChart]
  ('[   ])

newtype HighchartsWaterFallChart = HighchartsWaterFallChart (GHCJS.JSRef (HighchartsWaterFallChart))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsWaterFallChart =
  ('[   ])

newtype HighchartsWaterFallChartSeriesOptions = HighchartsWaterFallChartSeriesOptions (GHCJS.JSRef (HighchartsWaterFallChartSeriesOptions))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members HighchartsWaterFallChartSeriesOptions = TS.Extends '[HighchartsIndividualSeriesOptions, HighchartsWaterFallChart]
  ('[   ])

newtype JQuery = JQuery (GHCJS.JSRef (JQuery))
  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)
type instance TS.Members JQuery =
  ('[ "highcharts" ::: TS.Fun (HighchartsChartObject)
    , "highcharts" ::: TS.Fun (HighchartsOptions -> JQuery)
    , "highcharts" ::: TS.Fun (HighchartsOptions -> (HighchartsChartObject -> TS.Void) -> JQuery)
    ])
