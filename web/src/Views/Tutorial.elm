module Views.Tutorial exposing (view)

import Element exposing (..)
import Elements
import Styles
import Model exposing (Msg(..))


myP =
    Elements.paragraph [ spacing (Styles.paddingScale 1) ]


view : Element Msg
view =
    column [ spacing (Styles.paddingScale 3) ]
        [ Elements.h2 "Welcome"
        , Elements.p """
            NoKey is a distributed password manager. In contrast to other password managers, it doesn't use
            a master password to secure your passwords.
            """
        , Elements.p """
        Instead, your passwords are stored on multiple devices
            you own, such that you only get access to your passwords if enough devices are present.
        """
        , Elements.h3 "Concepts"
        , Elements.h4 "Pairing"
        , Elements.p """
            To get started, you will have to pair your devices together. After pairing a device, the devices will
            be able to communicate with each other and can share all your saved passwords.
            """
        , Elements.h4 "Password Group"
        , myP
            [ Elements.text """
            You can think of a password group as a vault that contains your passwords.
            A group has a
            """
            , Elements.b "security level"
            , Elements.text """
            associated with it.
            The security level indicates how many keys you need to unlock a vault.
            Usually, every device holds one key.
            """
            , Elements.text "Password groups are referred to with the following icon:"
            , Elements.groupIcon True ( ( 2, "" ), "" )
            , Elements.text ". The icon stands for a password group with security level 2 that is currently locked."
            ]
        , Elements.text ""
        , Elements.italicText "More to come.."
        , Elements.text ""
        , Elements.primaryButton (Just DoneWithTutorial) "Ok, let's get started!"
        ]
