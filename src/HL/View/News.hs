{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | News page view.

module HL.View.News where

import HL.View
import HL.View.Template

-- | News view.
newsV :: Html () -> FromLucid App
newsV inner =
  template []
           "新闻"
           (\_ ->
              container_
                (do row_ (span12_ [class_ "col-sm-12"] (do h1_ "新闻"))
                    inner))
