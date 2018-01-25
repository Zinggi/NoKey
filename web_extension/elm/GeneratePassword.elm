module GeneratePassword exposing (main)

import Html
import Views.PasswordGenerator


main =
    Html.beginnerProgram
        { view = Views.PasswordGenerator.view identity
        , update = (\msg model -> msg)
        , model = Views.PasswordGenerator.init
        }
