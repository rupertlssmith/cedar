module ModelUtils exposing (asMarkdown, withMarkdown, asUUID)

import Model exposing (ContentModel(..), Panel(..), Titled(..), MdContent(..))


asMarkdown : ContentModel -> String
asMarkdown model =
    case model of
        MdContentAsContentModel (MdContent mdcontent) ->
            .markdown mdcontent

        PanelAsContentModel (Panel mdcontent) ->
            .markdown mdcontent

        _ ->
            ""


withMarkdown : ContentModel -> String -> ContentModel
withMarkdown model newValue =
    case model of
        MdContentAsContentModel (MdContent mdcontent) ->
            MdContentAsContentModel (MdContent { mdcontent | markdown = newValue })

        PanelAsContentModel (Panel mdcontent) ->
            PanelAsContentModel (Panel { mdcontent | markdown = newValue })

        other ->
            other


asUUID : ContentModel -> String
asUUID model =
    case model of
        TitledAsContentModel (Titled titled) ->
            .uuid titled

        MdContentAsContentModel (MdContent mdcontent) ->
            .uuid mdcontent

        PanelAsContentModel (Panel mdcontent) ->
            .uuid mdcontent
