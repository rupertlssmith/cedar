module Renderer.ContentAsTree exposing
    ( containerTreeToContent
    , contentContainerToTree
    , contentRelationshipsToTree
    )

import Maybe.Extra exposing (unwrap)
import Model exposing (Content(..), ContentModel, Relationship(..))
import MultiwayTree as Tree exposing (Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)
import TreeUtils exposing ((&>), updateTree)


contentRelationshipsToTree : (Content -> a) -> Content -> Tree a
contentRelationshipsToTree makeNode content =
    let
        innerContentToTree ((Content content) as contentRecord) =
            case content.relationships of
                Nothing ->
                    Tree
                        (makeNode contentRecord)
                        []

                Just relationships ->
                    Tree
                        (makeNode contentRecord)
                        (List.foldr
                            (\(Relationship relationship) ->
                                \children ->
                                    case relationship.object of
                                        Nothing ->
                                            children

                                        Just content ->
                                            innerContentToTree content :: children
                            )
                            []
                            relationships
                        )
    in
    innerContentToTree content


contentContainerToTree : (Content -> a) -> Content -> Tree a
contentContainerToTree makeNode content =
    let
        innerContentToTree ((Content content) as contentRecord) =
            case content.container of
                Nothing ->
                    Tree
                        (makeNode contentRecord)
                        []

                Just container ->
                    Tree
                        (makeNode contentRecord)
                        (List.foldr
                            (\content ->
                                \children ->
                                    innerContentToTree content :: children
                            )
                            []
                            container
                        )
    in
    innerContentToTree content


containerTreeToContent : Tree Content -> Content
containerTreeToContent tree =
    let
        (Content node) =
            Tree.datum tree
    in
    Content
        { node
            | container =
                List.map
                    (\child -> containerTreeToContent child)
                    (Tree.children tree)
                    |> Just
        }
