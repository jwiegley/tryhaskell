{-# LANGUAGE NoOverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Data.List
import Development.Shake
-- import Development.Shake.FilePath
import System.Directory
import System.FilePath.Posix
import System.IO.Temp

version = "7.6.3-3.0a-20040418"

aquaVer = "3.0a"
aquaDmg = "Aquamacs-Emacs-" ++ aquaVer ++ ".dmg"
aquaUrl = "https://github.com/davidswelt/aquamacs-emacs/releases/download/Aquamacs-"
    ++ aquaVer ++ "/" ++ aquaDmg

ghcVer = "ghc-7.6.3"
ghcTarball = ghcVer ++ "-x86_64-apple-darwin.tar.bz2"

appRoot = "/Applications/Try Haskell.app"
ghcRoot = appRoot </> "Contents/Resources/ghc"

target = "TryHaskell-" ++ version ++ ".dmg"
ghcBin = ghcRoot </> "bin/ghc"

main :: IO ()
main = shakeArgs shakeOptions {- shakeVerbosity = Chatty -} $ do
    -- want [ target ]
    want [ ghcBin ]

    appRoot *> \out -> do
        let aquaDmg' = "deps" </> aquaDmg
        need [ aquaDmg' ]
        extractDmg aquaDmg' "Aquamacs.app" out

    ghcBin *> \_out -> do
        let ghcTarball' = "deps" </> ghcTarball
        need [ appRoot, ghcTarball' ]
        tmpDir <- liftIO getTemporaryDirectory
        path <- liftIO $ createTempDirectory tmpDir "ghc"
        () <- cmd "tar xjCf" [ path, ghcTarball' ]
        let distro = path </> ghcVer
        () <- cmd (Cwd distro) (EchoStdout True)
            "sh configure" [ "--prefix=" ++ ghcRoot ]
        () <- cmd (Cwd distro) "make install"
        liftIO $ removeDirectoryRecursive path

    target *> \out -> do
        need [ "image" ]
	cmd [ "hdiutil"
            , "create"
            , "-ov"
            , "-format", "UDBZ "
            , "-volname", "\"Try Haskell!\""
            , "-srcfolder", out
            ]

    "image" *> \out -> do
        liftIO $ createDirectory "image"
        need [ "image/README"
             , "image/NEWS"
             , "image/.background"
             -- , "TryHaskell"
             -- , "TryHaskell/Contents/Info.plist"
             -- , "TryHaskell/Contents/Resources/ghc"
             -- , "TryHaskell/Contents/Resources/site-lisp/edit-modes/haskell"
             -- , "TryHaskell/Contents/Resources/site-lisp/site-start.el"
             ]
        () <- cmd "chmod -R go+rX" [out]
	cmd "ln -sf /Applications" [out]

    ["image/README", "image/NEWS"] **> \out -> do
        let fn = takeFileName out
        need [ fn ]
        liftIO $ copyFile fn out
        () <- cmd "chflags hidden" [out]
	cmd "touch" [out]

    "image/.background" *> \out -> do
        need [ "dist/image.png", "dist/DS_Store" ]
        liftIO $ createDirectory out
        liftIO $ copyFile "dist/image.png" out
        liftIO $ copyFile "dist/DS_Store" "image/.DS_Store"
	cmd "touch" [out]

    "TryHaskell" *> \out ->
        cmd "ln -sf" [ "image/Try Haskell.app", out ]

    "TryHaskell/Contents/Info.plist" *> \out -> do
        need [ aquaDmg ]
        () <- cmd "hdiutil attach" [aquaDmg]
        () <- cmd "rsync -aE --delete"
            [ "/Volumes/Aquamacs Emacs/Aquamacs Emacs.app/"
            , "image/Try Haskell.app/"
            ]
        () <- cmd "hdiutil detach" ["/Volumes/Aquamacs Emacs"]
        cmd "touch" [out]

    {-
        Unpack and install Aquamacs

        Unpack and install GHC binary distro

        Fix cpp reference in TryHaskell bundle
                build clang-xcode5-wrapper
                change /Applications/TryHaskell.app/Contents/Resources/ghc/lib/ghc-7.6.3/settings
                    to use it instead of gcc

        Unpack cabal-install

        substitute
            DEFAULT_PREFIX="/usr/local"
        for
            DEFAULT_PREFIX="/Applications/TryHaskell.app/Contents/Resources/ghc"

        run bootstrap.sh --global

        cd into stackage-deps

        export PREFIX=/Applications/TryHaskell.app/Contents/Resources/ghc

        cabal install -j --global --prefix=$PREFIX happy alex
        cabal install -j --global --prefix=$PREFIX c2hs

        cabal install
            -j
            --only-dependencies
            --global
            --prefix=$PREFIX
            --haddock-hoogle
            --haddock-html
            --haddock-executables
            --haddock-internal
            --haddock-hyperlink-source
            --extra-include-dirs=$PREFIX/include
            --extra-lib-dirs=$PREFIX/lib
            --disable-library-profiling
    -}

extractDmg :: FilePath -> FilePath -> FilePath -> Action ()
extractDmg dmgPath sourcePath targetPath = do
    Stdout out <- cmd
        [ "hdiutil"
        , "attach"
        , "-readonly"
        , "-mountrandom", "/tmp",
          "-noverify"
        , "-noautofsck"
        , dmgPath
        ]

    let mountPoint =
          case find (isInfixOf "/tmp/dmg") (lines out) of
            Just line -> (Just . last . words) line
            Nothing   -> Nothing
    case mountPoint of
      Nothing -> error "Failed to attach disk image"
      Just dir -> do
        () <- cmd "ditto" [ dir </> sourcePath, targetPath ]
        cmd "hdiutil" [ "detach", dir, "-force" ]
