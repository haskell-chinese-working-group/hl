{-# LANGUAGE OverloadedStrings #-}

-- | Downloads page view.

module HL.View.Downloads where

import Data.Monoid
import HL.Types
import HL.View
import HL.View.Template

downloadsFromMarkdown :: Html () -> FromLucid App
downloadsFromMarkdown md =
  template [] "下载"
    (\_ -> container_ (row_  (span12_ [class_ "col-sm-12"]
                                      (do h1_ (toHtml ("下载" :: String))
                                          md))))

-- | OS-specific downloads view.
downloadsForV :: OS -> Html () -> Html () -> FromLucid App
downloadsForV os autoInstall manualInstall =
  template
    [DownloadsR
    ,DownloadsForR os]
    ("下载 " <> toHuman os <> " 版本")
    (\_ ->
       container_
         (row_
            (span12_ [class_ "col-sm-12"]
               (do h1_ (toHtml ("下载用于 " <> toHuman os <> " 的版本"))
                   autoInstall
                   when (os == Linux)
                        (do h2_ "手动安装"
                            p_ "你可以通过下面的步骤来手动安装GHC和Cabal。"
                            manualInstall)))))
