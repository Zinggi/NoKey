module GeneratePassword exposing (main)

import Html
import Views.PasswordGenerator
import Element


main =
    Html.beginnerProgram
        { view = Views.PasswordGenerator.view identity >> Element.layout []
        , update = (\msg model -> msg)
        , model = Views.PasswordGenerator.init
        }
