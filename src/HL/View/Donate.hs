{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Community page view.

module HL.View.Donate where

import HL.View
import HL.View.Template

import Lucid.Base

-- | Donation view.
donateV :: FromLucid App
donateV =
  templateWithBodyEnder
    []
    "向Haskell.org捐款"
    (\_ ->
       do container_
            (row_
               (span12_
                  (do h1_ "向Haskell.org捐款"
                      p_
                        "通过下面这个表单，你可以向Haskell.org发起一笔免费的捐赠。"
                      p_ (do "你可以使用任何一张信用卡，或者"
                             a_ [href_ "https://bitcoin.org/en/"] "比特币"
                             "。 此外你需要一个有效的电子邮箱用于获取收据"
                             "（如果你想要保持匿名，可以尝试使用"
                             a_ [href_ "http://maildrop.cc"] "maildrop.cc"
                             "提供的临时邮箱）。")
                      p_ (do "你的捐赠会被 "
                             a_ [href_ "https://stripe.com"] "Stripe"
                             "受理，并由它代表在纽约州注册的 §501(c)3 类型的非盈利组织"
                             a_ [href_ "http://www.spi-inc.org/"] "Software in the Public Interest"
                             " 授予Haskell.org。")
                      p_ (do "此外，你也可以通过免费的在线筹资平台 "
                             a_ [href_ "https://co.clickandpledge.com/advanced/default.aspx?wid=69561"] "Click & Pledge"
                             " 向 Haskell.org/SPI 捐款。")
                      p_ donateForm
                      p_ statusWindow))))
    (\_ url -> do script_ [src_ "https://checkout.stripe.com/checkout.js"] ""
                  script_ [src_ "https://donate.haskell.org/pubkey.js"] ""
                  scripts url [js_donate_js])

donateForm :: Html ()
donateForm =
  form_ [class_ "form-inline"] (do
    input_ [id_ "monies", type_ "number", class_ "input-large", placeholder_ "美元金额"]
    " "
    button_ [id_ "paybtn", class_ "btn btn-info", type_ "button"] "捐款")

statusWindow :: Html ()
statusWindow =
  div_ [ id_ "payment_status", class_ "alert alert-block fade", role'_ "alert" ] (do
    button_ [ id_ "pay_status_close_btn", href_ "#", type_ "button"
            , class_ "close" ] "x"
    div_ [] (do
      span_ [ id_ "status_glyph", class_ "glyphicon glyphicon-exclamation-sign"
            , aria_hidden_ "true" ] ""
      span_ [ id_ "status_title", class_ "sr-only" ] "Error:"
      span_ [ id_ "status_message" ] "  在上方留言."))

role'_ :: Text -> Attribute
role'_ = makeAttribute "role"

aria_hidden_ :: Text -> Attribute
aria_hidden_ = makeAttribute "role"
