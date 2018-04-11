module Views.Tutorial exposing (view)

import Element exposing (..)
import Elements
import Model exposing (Msg(..))


view : Element Msg
view =
    paragraph []
        [ Elements.p """
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas eget mauris diam. Ut vel dolor eu ipsum tempor cursus eget sed turpis. Vivamus vitae dui sit amet nulla auctor commodo at ornare diam. Curabitur tempor quis felis placerat auctor. Phasellus aliquet efficitur arcu ac condimentum. Phasellus finibus justo vestibulum quam venenatis, vitae vestibulum lorem consequat. Sed mi tortor, interdum sit amet auctor vel, suscipit a leo. Maecenas congue condimentum luctus. Nam elementum tortor vitae elementum placerat. Suspendisse eu accumsan neque. Vestibulum rutrum tristique ultricies. Nulla auctor accumsan consequat. Maecenas venenatis ligula id bibendum sollicitudin. Praesent vitae arcu lobortis, venenatis augue posuere, posuere purus. Mauris aliquam interdum egestas.

Sed a venenatis libero. Morbi pellentesque at mauris et vehicula. Nullam vitae finibus velit. Duis ac facilisis magna, nec interdum urna. Etiam congue, nulla eu consectetur rutrum, purus massa malesuada lorem, eu laoreet velit augue eu ligula. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Phasellus tristique vitae velit id sollicitudin. Donec rutrum magna eu mi aliquam tempor. Nunc quis arcu dignissim, dignissim sapien ut, pharetra turpis. Vestibulum porta diam in cursus ultrices. Fusce a commodo purus. Mauris vitae sapien lorem. Mauris porttitor metus vel massa porttitor tristique.
"""
        , Elements.primaryButton (Just DoneWithTutorial) "Ok, let's get started!"
        ]
