module RectUtils exposing (enlarge, noRect, zeroPosition, translate)

import DOM exposing (Rectangle)


-- Position and size calculations


enlarge : Float -> Rectangle -> Rectangle
enlarge px rect =
    { rect
        | top = rect.top - px
        , left = rect.left - px
        , width = rect.width + 2 * px
        , height = rect.height + 2 * px
    }


noRect : Rectangle
noRect =
    { top = 0.0, left = 0.0, width = 0.0, height = 0.0 }


zeroPosition : Rectangle -> Rectangle
zeroPosition rect =
    { rect | top = 0.0, left = 0.0 }


translate : Float -> Float -> Rectangle -> Rectangle
translate x y rect =
    { rect
        | top = rect.top + y
        , left = rect.left + x
    }
