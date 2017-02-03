{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Community page view.

module HL.View.Community where

import HL.View
import HL.View.Template

-- | Community view.
communityV :: FromLucid App
communityV =
  template
    []
    "社区"
    (\url ->
       container_
         (row_
            (span12_ [class_ "col-sm-12"]
               (do h1_ "社区"
                   p_
                     "在这里，来自世界各地的Haskell通过多种方式交流、讨论和合作。\
                     \这些在线社区是学习、教学、提问，以及寻求贡献和协作的地方。"
                   h2_ "在线社区和社交资源"
                   p_ "haskell用户在很多在线社区都很活跃，尤其是一下这些："
                   ul_ (online url)
                   h2_ "个人团体和聚会"
                   p_ "Haskell用户还有很多用于学习和编程的聚会，包括下面这些："
                   ul_ offline
                   h2_ "会议和活动"
                   p_ "有很多专门关注Haskell的会议和活动，其中一些专注于学术方面的问题，另一些更加关注Haskell的商业应用和其他有趣的方面。下面列出了其中的一小部分："
                   h3_ "学术会议"
                   ul_ academicConferences
                   h3_ "非学术会以"
                   ul_ commercialConferences
                   h3_ "黑客马拉松"
                   p_ "Haskell黑客马拉松有着很悠久的传统，包括大量的学习交流和社交互动。在很多方面，Haskell黑客马拉松扮演着半结构化会议的角色。下面是其中比较出名的一部分："
                   ul_ hackathons
                   h2_ "专业组"
                   ul_ sigs
                   h2_ ""))))

online :: (Route App -> Text) -> Html ()
online url =
  do li_ (a_ [href_ (url MailingListsR)] "The Haskell mailing lists")
     li_ (a_ [href_ (url IrcR)] "IRC (online chat)")
     li_ (a_ [href_ "http://stackoverflow.com/questions/tagged?tagnames=haskell"] "StackOverflow")
     li_ (a_ [href_ "https://plus.google.com/communities/104818126031270146189"] "Google+ community")
     li_ (a_ [href_ "https://www.facebook.com/groups/programming.haskell/"] "Facebook community")
     li_ (a_ [href_ "http://www.reddit.com/r/haskell"] "Reddit")
     li_ (a_ [href_ "http://www.haskell.org/haskellwiki/Haskell"] "Wiki")
     li_ (a_ [href_ "http://planet.haskell.org/"] "The blogosphere")
     li_ (a_ [href_ "https://gitter.im/haskell-chat"] "Haskell Gitter Community Chat")

offline :: Html ()
offline =
  do li_ (a_ [href_ "http://www.meetup.com/ATX-Haskell/"] "Austin Haskell Users Group")
     li_ (a_ [href_ "http://www.meetup.com/Bay-Area-Haskell-Users-Group/"] "Bay Area Haskell Users Group")
     li_ (a_ [href_ "http://www.meetup.com/Boston-Haskell/"] "Boston Haskell")
     li_ (a_ [href_ "http://www.meetup.com/berlinhug/"] "Berlin Haskell Users Group")
     li_ (a_ [href_ "http://ChicagoHaskell.com/"] "Chicago Haskell")
     li_ (a_ [href_ "http://www.meetup.com/NY-Haskell/"] "New York Haskell Users Group")
     li_ (a_ [href_ "http://www.meetup.com/London-Haskell/"] "London Haskell")
     li_ (a_ [href_ "http://www.meetup.com/seahug/"] "Seattle Area Haskell Users' Group")
     li_ (a_ [href_ "http://www.meetup.com/find/?allMeetups=true&keywords=Haskell&radius=Infinity"] "More Haskell meetups at meetup.com")

academicConferences :: Html ()
academicConferences =
  do li_ (a_ [href_ "https://www.haskell.org/haskell-symposium/"] "The Haskell Symposium")
     li_ (a_ [href_ "https://wiki.haskell.org/HaskellImplementorsWorkshop"] "Haskell Implementors' Workshop")
     li_ (a_ [href_ "http://www.icfpconference.org/"] "The International Conference on Functional Programming")
     li_ (a_ [href_ "http://popl.mpi-sws.org/"] "Symposium on Principles of Programming Languages")
     li_ (a_ [href_ "http://www.ifl-symposia.org/"] "International Symposia on Implementation and Application of Functional Languages")
     li_ (a_ [href_ "http://www.tifp.org/"] "Symposium on Trends in Functional Programming ")

commercialConferences :: Html ()
commercialConferences =
  do li_ (a_ [href_ "http://cufp.org/"] "Commercial Users of Functional Programming (Roving)")
     li_ (a_ [href_ "http://www.lambdajam.com/"] "LambdaJam (Chicago, IL, USA)")
     li_ (a_ [href_ "http://functionalconf.com/"] "Functional Conf (Bangalore, IN)")
     li_ (a_ [href_ "http://www.lambdacon.org/"] "LambdaCon (Bologna, IT)")
     li_ (a_ [href_ "http://bobkonf.de/"] "BOB (Berlin, DE)")
     li_ (a_ [href_ "http://www.iba-cg.de/hal9.html"] "HaL (Leipzig, Halle, DE)")
     li_ (a_ [href_ "https://skillsmatter.com/conferences/1907-haskell-exchange-2014"] "Skills Matter's Haskell eXchange (London, GB)")
     li_ (a_ [href_ "http://www.degoesconsulting.com/lambdaconf-2015/"] "LambdaConf (Boulder, CO, USA)")
     li_ (a_ [href_ "http://composeconference.org"] "Compose :: Conference (NY, NY, USA)")

hackathons :: Html ()
hackathons =
  do li_ (a_ [href_ "http://bayhac.org/"] "BayHac (Bay Area, USA)")
     li_ (a_ [href_ "https://wiki.haskell.org/Hac_Phi"] "Hac Phi (Philadelphia, PA, USA)")
     li_ (a_ [href_ "https://wiki.haskell.org/ZuriHac"] "ZuriHac (Zurich, CH)")

sigs :: Html ()
sigs =
  do li_ (a_ [href_ "http://industry.haskell.org/"] "Industrial Haskell Group")
     li_ (a_ [href_ "http://commercialhaskell.com/"] "Commercial Haskell Group")
     li_ (a_ [href_ "http://lurk.org/groups/haskell-art/"] "Haskell Art")
