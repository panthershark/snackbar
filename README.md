# Snackbar 
A material design snackbar written in elm.  [See the demo](https://panthershark.github.io/snackbar/demo/index.html)

# How to create a snackbar

According to Material spec, snackbars should hide themselves. Allowing them to be closed is int he Caution section. We support both options.

To accomplish this and keep consurrent timeouts staight, use the factory functions.

Assume a setup like:

```
import Snackbar exposing (Snackbar)

type alias Model = {
  snackbar: Snackbar
}

type Msg =
  SnackMessage (Snackbar.Msg Msg)
  | CustomAction


update msg model =
    case msg of
        SomeAppMessage ->
            let
                ( sb, cmd ) =
                    Snackbar.message (Snackbar.DefaultDelay) "Hello World!"
            in
            ( { model | snackbar = sb }, Cmd.map SnackMessage cmd )

        CustomAction ->
          (model, doSomething)
```

### Snackbar with no action button

```
Snackbar.message (Snackbar.CustomDelay 8000) "I'm a plain message that disappears in 8s."
```

### Snackbar with anchor for action button

```
Snackbar.link (Snackbar.CustomDelay 8000) "I'm a message with a link to a url" "YO" "http://giphy.com"
```

### Snackbar that sends a message to update for action button

In this case, the action_ref is passed to the Snackbar.Msg of `ButtonClick action_ref`. Using this action_ref, an application that could handle multiple actions can distinguish events/intent from different snackbars.

```
Snackbar.action (Snackbar.CustomDelay 8000) "I'm a message that triggers an action in the app" "WOAH" CustomAction
```

### Snackbar that shows indefinitely

```
Snackbar.message Snackbar.ShowForever "I'm a plain message that won't go away until explcitly."
```

## Customize Delays

From here - https://material.io/design/components/snackbars.html#behavior

> Snackbars appear without warning, and don't require user interaction. They automatically disappear from the screen after a minimum of four seconds, and a maximum of ten seconds.

- Show the snackbar and do not automatically hide it. `Snackbar.ShowForever`
- Show the snackbar customize the delay. ex: hide after 6s `Snackbar.CustomDelay 6000`
- Use a default value for auto-hiding the snackbar. actions have longer delay so users have time to click. `Snackbar.DefaultDelay`
  - `Snackbar.message` default hides after 4s
  - `Snackbar.action` default hides after 10s
  - `Snackbar.link` default hides after 10s


