# A simple static site generator 

Avdou-hs is a simple static site generator (ssg) library, written is haskell. You can use it to build your own ssg.

## Build

* Run `stack build` to build your generator. This is needed only if the configuration of your site (in haskell) has changed.
* Run `stack exec -- site` to build you site (the name of the executable can be anything you choose -- it does not have to be 'site').
* Run `stack exec -- watch` to start the local server and see your site locally.
