module GeneratePassword exposing (main)

import Html
import Views.PasswordGenerator
import Element
import Styles


main =
    Html.beginnerProgram
        { view = Views.PasswordGenerator.view identity >> Element.layout Styles.stylesheet
        , update = (\msg model -> msg)
        , model = Views.PasswordGenerator.init
        }
