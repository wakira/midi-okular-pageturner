# midi-okular-pageturner

Music sheet page-turner for Linux. Remote control your Okular PDF reader using MIDI pedals!

## Usage

### Command line arguments:

```
$ midi-okular-pageturner CLIENT_ID PORT_ID --next PEDAL_ID --prev PEDAL_ID
```

For example,
```
$ midi-okular-pageturner 28 0 --next 67 --prev 66
```

Query your MIDI device's client ID and port ID by `aconnect -i`.
Pedal ID is the MIDI CC number for your pedal of choice.
66 stands for Sostenuto pedal and 67 for Soft Pedal.

### Changing the target document

When midi-okular-pageturner is launched, it searches all open okular documents. If more than
one document is found, it will ask to select one document you want to control.

You can always press Enter to choose another document.
