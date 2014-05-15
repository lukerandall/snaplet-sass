snaplet-sass
===========

[Changelog](CHANGELOG.md)

snaplet-sass integrates [Snap](http://www.snapframework.com) with
[Sass](http://www.sass-lang.com).

Features
--------

* Compile and serve sass files on request, no need to restart the
  snap server.
* Production mode to pre-compile all sass files.
* Outputs sourcemaps to make debugging generated CSS easier.
* Writes CSS to disk to allow reading the generated source.

Example Usage
-------------

Site.hs:
```haskell
import Snap.Snaplet.Sass

routes = [..., ("/sass", with sass sassServe)]

app :: SnapletInit App App
app = makeSnaplet "app" "A snaplet example application." Nothing $ do
  s <- nestSnaplet "sass" sass initSass
  return $ App { _sass = s }
```

Application.hs:
```haskell
import Snap.Snaplet.Sass

data App = App { _sass :: Snaplet Sass }

makeLens ''App
```

Now run your application and try requesting a Sass file.

A snaplet config file will be generated at snaplets/sass/devel.cfg the
first time your application initializes the snaplet. The defaults are
the recommended ones for development.

Place your .sass or .scss files in snaplets/sass/src. Note that a default
devel.cfg will not be created if you have already created the sass
directory. If this happens to you, move snaplets/sass, start your
application, and then move the files back into snaplets/sass.

Any requests to the specified directory (in this case /sass/) will
compile the appropriate Sass file and serve it.


Thanks
-------

This borrows hugely from [snaplet-fay](https://github.com/faylang/snaplet-fay).
