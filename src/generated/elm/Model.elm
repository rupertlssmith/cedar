module Model exposing(..)

import Set exposing (Set)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (..)
import Json.Decode.Extra exposing ((|:), withDefault)
import Json.Encode as Encode exposing (..)
import Exts.Maybe exposing (catMaybes)
type PredicateType =
    PredicateType
    {
    name : String
    , id : Maybe String
    }

unwrapPredicateType (PredicateType predicateType) =
    predicateType

predicateTypeEncoder : PredicateType -> Encode.Value
predicateTypeEncoder (PredicateType model) =
        [
        Just ( "name", Encode.string model.name )
        , Maybe.map (\id -> ( "id", Encode.string id )) model.id
        ]
          |> catMaybes
          |> Encode.object


predicateTypeDecoder : Decoder PredicateType
predicateTypeDecoder =
    (Decode.succeed
        (\name id ->
            PredicateType
                {
                name = name
                , id = id
                }
        )
    )
        |: field "name" Decode.string
        |: Decode.maybe (field "id" Decode.int |> Decode.map toString)


type ContentModel =
    TitledAsContentModel Titled
    | MdContentAsContentModel MdContent
    | PanelAsContentModel Panel

contentModelEncoder : ContentModel -> Encode.Value
contentModelEncoder model =
  case model of
    TitledAsContentModel titled -> titledEncoder titled
    MdContentAsContentModel mdContent -> mdContentEncoder mdContent
    PanelAsContentModel panel -> panelEncoder panel

contentModelDecoder : Decoder ContentModel
contentModelDecoder =
  let
    toContentModel typeName =
      case typeName of
        "Titled" -> map TitledAsContentModel titledDecoder
        "MdContent" -> map MdContentAsContentModel mdContentDecoder
        "Panel" -> map PanelAsContentModel panelDecoder
        _ -> Decode.fail ("unknown type: " ++ typeName)
  in
    field "@type" Decode.string
      |> andThen toContentModel



type Base =
    Base
    {
    lastUpdated : Maybe String
    }

unwrapBase (Base base) =
    base

baseEncoder : Base -> Encode.Value
baseEncoder (Base model) =
        [
        Maybe.map (\lastUpdated -> ( "lastUpdated", Encode.string lastUpdated )) model.lastUpdated
        ]
          |> catMaybes
          |> Encode.object


baseDecoder : Decoder Base
baseDecoder =
    (Decode.succeed
        (\lastUpdated ->
            Base
                {
                lastUpdated = lastUpdated
                }
        )
    )
        |: Decode.maybe (field "lastUpdated" Decode.string)


type Titled =
    Titled
    {
    uuid : String
    , title : String
    }

unwrapTitled (Titled titled) =
    titled

titledEncoder : Titled -> Encode.Value
titledEncoder (Titled model) =
        [
        Just ( "@type", Encode.string "Titled" ),
        Just ( "uuid", Encode.string model.uuid )
        , Just ( "title", Encode.string model.title )
        ]
          |> catMaybes
          |> Encode.object


titledDecoder : Decoder Titled
titledDecoder =
    (Decode.succeed
        (\uuid title ->
            Titled
                {
                uuid = uuid
                ,title = title
                }
        )
    )
        |: field "uuid" Decode.string
        |: field "title" Decode.string


type MdContent =
    MdContent
    {
    uuid : String
    , markdown : String
    }

unwrapMdContent (MdContent mdContent) =
    mdContent

mdContentEncoder : MdContent -> Encode.Value
mdContentEncoder (MdContent model) =
        [
        Just ( "@type", Encode.string "MdContent" ),
        Just ( "uuid", Encode.string model.uuid )
        , Just ( "markdown", Encode.string model.markdown )
        ]
          |> catMaybes
          |> Encode.object


mdContentDecoder : Decoder MdContent
mdContentDecoder =
    (Decode.succeed
        (\uuid markdown ->
            MdContent
                {
                uuid = uuid
                ,markdown = markdown
                }
        )
    )
        |: field "uuid" Decode.string
        |: field "markdown" Decode.string


type Panel =
    Panel
    {
    uuid : String
    , markdown : String
    , title : String
    , imageId : Maybe String
    }

unwrapPanel (Panel panel) =
    panel

panelEncoder : Panel -> Encode.Value
panelEncoder (Panel model) =
        [
        Just ( "@type", Encode.string "Panel" ),
        Just ( "uuid", Encode.string model.uuid )
        , Just ( "markdown", Encode.string model.markdown )
        , Just ( "title", Encode.string model.title )
        , Maybe.map (\imageId -> ( "imageId", Encode.string imageId )) model.imageId
        ]
          |> catMaybes
          |> Encode.object


panelDecoder : Decoder Panel
panelDecoder =
    (Decode.succeed
        (\uuid markdown title imageId ->
            Panel
                {
                uuid = uuid
                ,markdown = markdown
                ,title = title
                ,imageId = imageId
                }
        )
    )
        |: field "uuid" Decode.string
        |: field "markdown" Decode.string
        |: field "title" Decode.string
        |: Decode.maybe (field "imageId" Decode.string)


type Attachment =
    Attachment
    {
    externalId : Maybe String
    , fileName : Maybe String
    , resourceName : Maybe String
    , mimeType : Maybe String
    , id : Maybe String
    }

unwrapAttachment (Attachment attachment) =
    attachment

attachmentEncoder : Attachment -> Encode.Value
attachmentEncoder (Attachment model) =
        [
        Maybe.map (\externalId -> ( "externalId", Encode.string externalId )) model.externalId
        , Maybe.map (\fileName -> ( "fileName", Encode.string fileName )) model.fileName
        , Maybe.map (\resourceName -> ( "resourceName", Encode.string resourceName )) model.resourceName
        , Maybe.map (\mimeType -> ( "mimeType", Encode.string mimeType )) model.mimeType
        , Maybe.map (\id -> ( "id", Encode.string id )) model.id
        ]
          |> catMaybes
          |> Encode.object


attachmentDecoder : Decoder Attachment
attachmentDecoder =
    (Decode.succeed
        (\externalId fileName resourceName mimeType id ->
            Attachment
                {
                externalId = externalId
                ,fileName = fileName
                ,resourceName = resourceName
                ,mimeType = mimeType
                , id = id
                }
        )
    )
        |: Decode.maybe (field "externalId" Decode.string)
        |: Decode.maybe (field "fileName" Decode.string)
        |: Decode.maybe (field "resourceName" Decode.string)
        |: Decode.maybe (field "mimeType" Decode.string)
        |: Decode.maybe (field "id" Decode.int |> Decode.map toString)


type Content =
    Content
    {
    slug : Maybe String
    , path : Maybe String
    , contentType : Maybe (ContentType)
    , model : ContentModel
    , relationships : Maybe (List Relationship)
    , container : Maybe (List Content)
    , id : Maybe String
    }

unwrapContent (Content content) =
    content

contentEncoder : Content -> Encode.Value
contentEncoder (Content model) =
        [
        Maybe.map (\slug -> ( "slug", Encode.string slug )) model.slug
        , Maybe.map (\path -> ( "path", Encode.string path )) model.path
        , Maybe.map (\contentType -> ( "contentType", contentTypeEncoder contentType )) model.contentType
        , Just ( "model", contentModelEncoder model.model )
        , Maybe.map (\relationships -> ( "relationships", relationships |> List.map relationshipEncoder |> Encode.list )) model.relationships
        , Maybe.map (\container -> ( "container", container |> List.map contentEncoder |> Encode.list )) model.container
        , Maybe.map (\id -> ( "id", Encode.string id )) model.id
        ]
          |> catMaybes
          |> Encode.object


contentDecoder : Decoder Content
contentDecoder =
    (Decode.succeed
        (\slug path contentType model relationships container id ->
            Content
                {
                slug = slug
                ,path = path
                ,contentType = contentType
                ,model = model
                ,relationships = relationships
                ,container = container
                , id = id
                }
        )
    )
        |: Decode.maybe (field "slug" Decode.string)
        |: Decode.maybe (field "path" Decode.string)
        |: Decode.maybe (field "contentType" (Decode.lazy (\_ -> contentTypeDecoder)))
        |: field "model" (Decode.lazy (\_ -> contentModelDecoder))
        |: ((field "relationships" (Decode.maybe (Decode.list (Decode.lazy (\_ -> relationshipDecoder))))) |> withDefault Nothing)
        |: ((field "container" (Decode.maybe (Decode.list (Decode.lazy (\_ -> contentDecoder))))) |> withDefault Nothing)
        |: Decode.maybe (field "id" Decode.int |> Decode.map toString)


type Relationship =
    Relationship
    {
    subject : Maybe (Content)
    , predicate : Maybe PredicateType
    , object : Maybe (Content)
    , id : Maybe String
    }

unwrapRelationship (Relationship relationship) =
    relationship

relationshipEncoder : Relationship -> Encode.Value
relationshipEncoder (Relationship model) =
        [
        Maybe.map (\subject -> ( "subject", contentEncoder subject )) model.subject
        , Maybe.map (\predicate -> ( "predicate", predicateTypeEncoder predicate )) model.predicate
        , Maybe.map (\object -> ( "object", contentEncoder object )) model.object
        , Maybe.map (\id -> ( "id", Encode.string id )) model.id
        ]
          |> catMaybes
          |> Encode.object


relationshipDecoder : Decoder Relationship
relationshipDecoder =
    (Decode.succeed
        (\subject predicate object id ->
            Relationship
                {
                subject = subject
                ,predicate = predicate
                ,object = object
                , id = id
                }
        )
    )
        |: Decode.maybe (field "subject" (Decode.lazy (\_ -> contentDecoder)))
        |: Decode.maybe (field "predicate" predicateTypeDecoder)
        |: Decode.maybe (field "object" (Decode.lazy (\_ -> contentDecoder)))
        |: Decode.maybe (field "id" Decode.int |> Decode.map toString)


type ContentType =
    ContentType
    {
    name : String
    , template : String
    , layout : String
    , id : Maybe String
    }

unwrapContentType (ContentType contentType) =
    contentType

contentTypeEncoder : ContentType -> Encode.Value
contentTypeEncoder (ContentType model) =
        [
        Just ( "name", Encode.string model.name )
        , Just ( "template", Encode.string model.template )
        , Just ( "layout", Encode.string model.layout )
        , Maybe.map (\id -> ( "id", Encode.string id )) model.id
        ]
          |> catMaybes
          |> Encode.object


contentTypeDecoder : Decoder ContentType
contentTypeDecoder =
    (Decode.succeed
        (\name template layout id ->
            ContentType
                {
                name = name
                ,template = template
                ,layout = layout
                , id = id
                }
        )
    )
        |: field "name" Decode.string
        |: field "template" Decode.string
        |: field "layout" Decode.string
        |: Decode.maybe (field "id" Decode.int |> Decode.map toString)


