{-# LANGUAGE OverloadedStrings #-}

-- | Mailing lists page.

module HL.Controller.MailingLists where

import HL.Controller
import HL.Controller.Markdown
import HL.View

-- | Get mailing lists.
getMailingListsR :: C (Html ())
getMailingListsR =
  markdownPage [CommunityR,MailingListsR]
               "邮件列表"
               "mailing-lists.md"
