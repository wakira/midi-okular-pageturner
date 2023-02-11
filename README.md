# midi-okular-pageturner

Music sheet page-turner for Linux. Remote control your Okular PDF reader using MIDI pedals!

## Build

1. Make sure alsa-lib and pkgconfig is installed
2. Install Haskell (GHC) and cabal-install
3. `cabal build`

## Usage

### Command line arguments:

```
$ midi-okular-pageturner CLIENT_ID PORT_ID --next PEDAL_ID --prev PEDAL_ID
```

CLIENT_ID and PORT_ID is mandatory.
You can find your MIDI device's client ID and port ID by `aconnect -i` (provided by alsa-utils).

Use `--next` and `--prev` to specify the MIDI CC number for your pedal of choice.

For example,
```
$ midi-okular-pageturner 28 0 --next 67 --prev 66
```

Here, 66 stands for Sostenuto pedal and 67 for Soft Pedal.

### Changing the target document

When midi-okular-pageturner is launched, it searches all open okular documents. If more than
one document is found, it will ask to select one document you want to control.

You can always press Enter to redetect and choose another document.
