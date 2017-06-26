module Templates exposing (layouts, ssLayouts, templates)

import Dict exposing (Dict)
import Layout.Standard as Standard
import Layout.ServerWrapper as SSWrapper
import Template.ProductOverview as ProductOverview
import Template.Article as Article
import Renderer.Flexi exposing (Layout, Template)
import Config exposing (Config)


layouts : Dict String (Layout msg)
layouts =
    Dict.empty
        |> Dict.insert "standard" Standard.layout


ssLayouts : Config -> Dict String (Layout msg)
ssLayouts config =
    Dict.map (\key -> \value -> SSWrapper.wrapper config.applicationContextRoot value) layouts


templates : Dict String (Template msg)
templates =
    Dict.empty
        |> Dict.insert "article" Article.template
        |> Dict.insert "product-overview" ProductOverview.template
