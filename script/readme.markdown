The `script/` directory is for automating common tasks while developing this
package.

- `script/server` launches a development server using elm-reactor, and serves it
  up at http://0.0.0.0:8989. This makes it easy to test the device in the
  browser or on a phone.

- `script/publish-examples` will build the examples for the project and publish
  them on github pages. The project is moved to the `build/` directory and then
  committed to the [`gh-pages`
  branch](https://github.com/sch/elm-device/tree/gh-pages), which allows Github
  to serve it. This task is also what builds HTML appropriate for mobile
  devices.
