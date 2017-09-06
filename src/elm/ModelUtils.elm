module ModelUtils exposing (asMarkdown, withMarkdown, asUUID)

{-| ModelUtils provides some helper functions for working with the content model.

@docs asMarkdown, withMarkdown, asUUID

-}

import Model exposing (ContentModel(..), Panel(..), Titled(..), MdContent(..))


{-| Extracts the markdown from a content model.
-}
asMarkdown : ContentModel -> String
asMarkdown model =
    case model of
        MdContentAsContentModel (MdContent mdcontent) ->
            .markdown mdcontent

        PanelAsContentModel (Panel mdcontent) ->
            .markdown mdcontent

        _ ->
            ""


{-| Updates a content model with replacement markdown.
-}
withMarkdown : ContentModel -> String -> ContentModel
withMarkdown model newValue =
    case model of
        MdContentAsContentModel (MdContent mdcontent) ->
            MdContentAsContentModel (MdContent { mdcontent | markdown = newValue })

        PanelAsContentModel (Panel mdcontent) ->
            PanelAsContentModel (Panel { mdcontent | markdown = newValue })

        other ->
            other


{-| Extracts the UUID from a content model.
-}
asUUID : ContentModel -> String
asUUID model =
    case model of
        TitledAsContentModel (Titled titled) ->
            .uuid titled

        MdContentAsContentModel (MdContent mdcontent) ->
            .uuid mdcontent

        PanelAsContentModel (Panel mdcontent) ->
            .uuid mdcontent
