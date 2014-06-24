# Morse Code Encoder/Decoder

Inspired by [this blog post](http://charlieharvey.org.uk/page/bodgy_haskell_morse_code). An online morse code encoder/decoder.

## Usage

You can either use the CLI or start a web server.

### CLI

```
$ morse encode "such hungry"
... .._ _._. ....  .... .._ _. __. ._. _.__
$ morse decode ".... . ._.. ._.. ___  .__ ___ ._. ._.. _.."
hello world
$ morse code "hi"
.... ..
$ morse code ".... .."
hi
```

### Web Server

To start a web server on port `3000`, do this:

```
$ morse server 3000
```

## API

There is also an API you can use. Make sure to add an `Accept` header with the value `application/json`.

### Encode

A `GET` request to:

`/encode?val=[some text to encode]`

will return a response like:

```
{
  "result": ".... . ._.. ._.. ___  .__ ___ ._. ._.. _..",
  "input": "hello world"
}
```

### Decode

A `GET` request to:

`/decode?val=[some text to decode]`

will return a response like:

```
{
  "result": "hungry",
  "input": ".... .._ _. __. ._. _.__"
}
```