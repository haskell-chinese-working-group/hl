{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Documentation page view.

module HL.View.Documentation where

import HL.View
import HL.View.Template

-- | Documentation view.
documentationV :: FromLucid App
documentationV =
  template
    []
    "文档"
    (\url ->
       container_
         (row_
            (span12_ [class_ "col-sm-12"]
               (do h1_ "文档"
                   books
                   courses
                   tutorials
                   online
                   manuals
                   cabal
                   library
                   report url))))

books :: Html ()
books =
  do h2_ "学习Haskell的图书"
     links bookLinks
  where
    bookLinks =
      [("Learn You a Haskell for Great Good!","http://learnyouahaskell.com/")
      ,("Real World Haskell","http://book.realworldhaskell.org/")
      ,("Haskell Programming from first principles","http://haskellbook.com")
      ,("Beginning Haskell","http://www.apress.com/9781430262503")
      ,("Thinking Functionally with Haskell","http://www.cambridge.org/us/academic/subjects/computer-science/programming-languages-and-applied-logic/thinking-functionally-haskell")
      ,("Parallel and Concurrent Programming in Haskell","http://chimera.labs.oreilly.com/books/1230000000929")
      ,("Programming in Haskell","http://www.cs.nott.ac.uk/~pszgmh/pih.html")
      ,("Haskell: The Craft of Functional Programming","http://www.haskellcraft.com/craft3e/Home.html")
      ,("The Haskell School of Music","http://haskell.cs.yale.edu/?post_type=publication&p=112")
      ,("Developing Web Applications with Haskell and Yesod","http://www.yesodweb.com/book")]

courses :: Html ()
courses =
  do h2_ "课程"
     p_ "课程材料"
     links courseLinks
  where
     courseLinks =
       [("University of Pennsylvania's CIS 194","https://www.seas.upenn.edu/~cis194/")
       ,("NICTA Functional Programming Course","https://github.com/NICTA/course")
       ,("University of Virginia's CS 1501","http://shuklan.com/haskell/")
       ,("Stanford's cs240h","http://www.scs.stanford.edu/14sp-cs240h/")]

tutorials :: Html ()
tutorials =
  do h2_ "教程"
     p_ "简短精炼但直击要害的学习途径"
     links tutorialLinks
  where
    tutorialLinks =
       [("A Gentle Introduction to Haskell","https://www.haskell.org/tutorial/")
       ,("Happy Learn Haskell Tutorial","https://www.happylearnhaskelltutorial.com/")
       ,("Yet Another Haskell Tutorial","http://en.wikibooks.org/wiki/Haskell/YAHT/Preamble")
       ,("Write Yourself a Scheme in 48 Hours","http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours")
       ,("Learning Haskell","http://learn.hfm.io")]

online :: Html ()
online =
  do h2_ "在线资源"
     p_ "Haskell用户精心整理和维护的资源："
     links resources
  where
    resources =
      [("The Haskell Wiki","http://wiki.haskell.org")
      ,("The Haskell Wikibook","http://en.wikibooks.org/wiki/Haskell")
      ,("FP Complete's School of Haskell","https://www.fpcomplete.com/school")
      ,("Stephen Diehl's What I Wish I Knew When Learning Haskell","http://dev.stephendiehl.com/hask/#cabal")
      ,("Chris Allen's List of Learning Haskell Resources","https://github.com/bitemyapp/learnhaskell")
      ,("Bob Ippolito's Getting Started with Haskell","http://bob.ippoli.to/archives/2013/01/11/getting-started-with-haskell/")
      ,("Albert Y.C. Lai's Haskell Notes and Examples","http://www.vex.net/~trebla/haskell/index.xhtml")
      ,("Learning Haskell Resources on the Haskell Wiki","https://wiki.haskell.org/Learning_Haskell")]

manuals :: Html ()
manuals = do h2_ "手册和指导"
             p_ "涵盖了Haskell常用的工具的指导："
             links tools
  where tools = [("GHC User Guide","http://www.haskell.org/ghc/docs/latest/html/users_guide/")
                ,("Cabal Homepage And Quick Links","https://www.haskell.org/cabal/")
                ,("Cabal User Guide","http://www.haskell.org/cabal/users-guide/")
                ,("Stack User Guide","https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md")
                ,("Haddock User Guide","http://www.haskell.org/haddock/doc/html/index.html")
                ,("Haskeleton: A Haskell Project Skeleton","http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/")
                ,("How to Write a Haskell Program","https://wiki.haskell.org/How_to_write_a_Haskell_program")]

cabal :: Html ()
cabal =
  do h2_ "包和依赖管理"
     p_ "Cabal的教程是一个很好的起点，但仍有很多内容要进一步学习："
     links cabalInfo
  where cabalInfo =
           [("Stephen Diehl's Cabal Quickstart","http://dev.stephendiehl.com/hask/#cabal")
           ,("An Introduction to Cabal Sandboxes","http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html")
           ,("The Storage and Interpretation of Cabal Packages","http://www.vex.net/~trebla/haskell/sicp.xhtml")
           ,("The Cabal of Cabal","http://www.vex.net/~trebla/haskell/cabal-cabal.xhtml")]

library :: Html ()
library =
  do h2_ "库的文档"
     p_ "Haskell库的文档通常可以在Hackage上获取，此外我们还有通过类型而非名字的专门用于搜索文档的工具。"
     links docs
  where docs =
         [("Hoogle API Search","http://www.haskell.org/hoogle/")
         ,("FPComplete API Search", "https://www.fpcomplete.com/hoogle")
         ,("Hayoo! API Search","http://hayoo.fh-wedel.de")
         ,("Hackage","http://hackage.haskell.org/")
         ,("The Typeclassopedia","https://wiki.haskell.org/Typeclassopedia")
         ,("Haddocks for Libraries included with GHC","https://downloads.haskell.org/~ghc/latest/docs/html/libraries/index.html")]

report :: (Route App -> Text) -> Html ()
report _ =
  do h2_ "语言报告"
     p_ (do "Haskell 2010语言报告可以在线获取 "
            a_ [href_ "//haskell.org/onlinereport/haskell2010/"] "链接"
            ". ")
     p_ (do "PDF格式的文档 "
            a_ [href_ "//haskell.org/definition/haskell2010.pdf"] "下载链接"
            ".")
     p_ "这份文档也可以从darcs仓库下载： "
     p_ (pre_ (code_ "$ darcs get http://darcs.haskell.org/haskell2010-report"))

links :: [(Text,Text)] -> Html ()
links items =
  ul_ (forM_ items
             (\(title,url) ->
                li_ (a_ [href_ url] (toHtml title))))
