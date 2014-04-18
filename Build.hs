{-# LANGUAGE NoOverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

-- import Control.Monad
import Development.Shake
-- import Development.Shake.FilePath
import System.Directory
import System.FilePath.Posix

version = "0.1.20040418"

aquaVer = "3.0a"
aquaDmg = "Aquamacs-Emacs-" ++ aquaVer ++ ".dmg"
aquaUrl = "https://github.com/davidswelt/aquamacs-emacs/releases/download/Aquamacs-"
    ++ aquaVer ++ "/" ++ aquaDmg

main :: IO ()
main = shakeArgs shakeOptions $ do
    let target = "TryHaskell-" ++ version ++ ".dmg"
    want [ target ]

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
             , "TryHaskell"
             , "TryHaskell/Contents/Info.plist"
             , "TryHaskell/Contents/Resources/ghc"
             , "TryHaskell/Contents/Resources/site-lisp/edit-modes/haskell"
             , "TryHaskell/Contents/Resources/site-lisp/site-start.el"
             ]
        cmd "chmod -R go+rX" [out] :: Action ()
	cmd "ln -sf /Applications" [out]

    ["image/README", "image/NEWS"] **> \out -> do
        let fn = takeFileName out
        need [ fn ]
        liftIO $ copyFile fn out
	cmd "chflags hidden" [out] :: Action ()
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
        cmd "hdiutil attach" [aquaDmg] :: Action ()
        cmd "rsync -aE --delete"
            [ "/Volumes/Aquamacs Emacs/Aquamacs Emacs.app/"
            , "image/Try Haskell.app/"
            ] :: Action ()
        cmd "hdiutil detach" ["/Volumes/Aquamacs Emacs"] :: Action ()
        cmd "touch" [out]

    {-
        Fix cpp reference in TryHaskell bundle

        Unpack cabal-install

        substitute
            DEFAULT_PREFIX="/usr/local"
        for
            DEFAULT_PREFIX="/Applications/TryHaskell.app/Contents/Resources/ghc"
    -}
