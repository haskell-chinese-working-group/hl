{-# LANGUAGE OverloadedStrings #-}

-- | Features list.

module HL.View.Home.Features where

import HL.View
import HL.View.Code

import Data.Monoid

-- | Features section explains what's notable about Haskell as a
-- language.
features :: Html ()
features =
  div_ [class_ "features"]
       (container_
          (do h2_ "Features"
              row_ (do span6_ [class_ "col-md-6"] statically
                       span6_ [class_ "col-md-6"] purefunc)
              row_ (do span6_ [class_ "col-md-6"] inference
                       span6_ [class_ "col-md-6"] concurrent)
              row_ (do span6_ [class_ "col-md-6"] lazy
                       span6_ [class_ "col-md-6"] packages)))

purefunc :: Html ()
purefunc =
  do h3_ "纯函数式"
     p_ "Haskell中的函数也是数学意义上的函数，都具有一些类似的性质(例如\"pure\")， \
        \甚至是描述程序的功能的带有副作用的IO操作也都由纯的代码组成。Haskell \
        \程序中没有语句后者指令，只有表达式，这些表达式既不能修改局部变量和 \
        \全局变量，也不能获取例如时间或者随机数这样的带有状态的值。"
     p_ [class_ "text-center"] (a_ [data_ "toggle" "collapse", href_ "#collapse-functional", class_ "btn btn-xs btn-primary"] "点击展开")
     div_ [class_ "collapse", id_ "collapse-functional"] $ do
       p_ (do "下面这个函数接受一个整数作为参数，并返回一个整数。"
              "通过它的类型可以看出它不能产生任何副作用，不能修改它的参数。")
       haskellPre "square :: Int -> Int\n\
                  \square x = x * x"
       p_ (do "下面这个字符串拼接操作是可以成功运行的：")
       haskellPre "\"Hello: \" ++ \"World!\" "
       p_ (do "下面这个字符串拼接操作中包含着一个类型错误：")
       rejectedHaskellPre "Type error" "\"Name: \" ++ getLine"
       p_ (do "因为 "
              code_ "getLine"
              " 的类型是 "
              code_ "IO String"
              " ，而不是例如 "
              code_ "\"Name: \""
              " 这样的 "
              code_ "String"
              " 因此在类型系统的约束下，你不能将代码中纯的部分和不纯的（有副作用的）部分混在一起。")

statically :: Html ()
statically =
  do h3_ "静态类型"
     p_ "Haskell中每一个表达式都有自己的类型，类型在编译期确定。 \
       \类型通过函数应用组合在一起，在函数应用时类型必须相匹配， \
       \类型不能正确匹配的程序不能通过编译。Haskell中，类型不仅 \
       \仅是一种保证，更是一门表达程序构造的语言。"
     p_ [class_ "text-center"] (a_ [data_ "toggle" "collapse", href_ "#collapse-statically-typed", class_ "btn btn-xs btn-primary"] "点击展开")
     div_ [class_ "collapse", id_ "collapse-statically-typed"] $ do
       p_ "Haskell中每一个值都有对应的类型："
       haskellPre "char = 'a'    :: Char\n\
                  \int = 123     :: Int\n\
                  \fun = isDigit :: Char -> Bool\n"
       p_ "你必须将具有正确的类型的值作为参数传给函数，否则程序不能通过编译："
       rejectedHaskellPre "Type error" "isDigit 1"
       p_ "你可以将字节序列解码为Text："
       haskellPre "bytes = Crypto.Hash.SHA1.hash \"hello\" :: ByteString\n\
                  \text = decodeUtf8 bytes               :: Text\n"
       p_ "但是，你不能解码Text, Text已经是一个Unicode序列了："
       rejectedHaskellPre "Type error" "doubleDecode = decodeUtf8 (decodeUtf8 bytes)"

concurrent :: Html ()
concurrent =
  do h3_ "并发"
     p_ "Haskell显式处理副作用的特性使得Haskell非常适合并发编程。 \
       \Haskell的主流编译器GHC具有高性能的垃圾回收器和轻量级并发库， \
       \其中包含很多有用的并发编程需要的原语和抽象。"
     p_ [class_ "text-center"] (a_ [data_ "toggle" "collapse", href_ "#collapse-concurrent", class_ "btn btn-xs btn-primary"] "点击展开")
     div_ [class_ "collapse", id_ "collapse-concurrent"] $ do
       p_ "Haskell中，启动线程和线程间的通信都很容易通过标准库实现："
       haskellPre "main = do\n\
                  \  done <- newEmptyMVar\n\
                  \  forkIO (do putStrLn \"I'm one thread!\"\n\
                  \             putMVar done \"Done!\")\n\
                  \  second <- forkIO (do threadDelay 100000\n\
                  \                       putStrLn \"I'm another thread!\")\n\
                  \  killThread second\n\
                  \  msg <- takeMVar done\n\
                  \  putStrLn msg"
       p_ "通过异步API使用线程："
       haskellPre "do a1 <- async (getURL url1)\n\
                   \  a2 <- async (getURL url2)\n\
                   \  page1 <- wait a1\n\
                   \  page2 <- wait a2\n\
                   \  ..."
       p_ "使用软件事务内存（software transactional memory）的原子线程："
       haskellPre "transfer :: Account -> Account -> Int -> IO ()\n\
                   \transfer from to amount =\n\
                   \  atomically (do deposit to amount\n\
                   \                 withdraw from amount)"
       p_ "原子性事务必须是可以重复进行的，类型系统保证了在STM中不能使用任何IO操作："
       rejectedHaskellPre "Type error" "main = atomically (putStrLn \"Hello!\")"

inference :: Html ()
inference =
  do h3_ "类型推断"
     p_ "你不需要给每个Haskell程序都显式地写出其类型，程序的类型 \
       \通过对双向地合一所有类型进行推断。你可以让编译器替你推断 \
       \出函数的类型，但你自己也可以选择写出函数的类型，以便生成 \
       \更好的文档。"
     p_ [class_ "text-center"] (a_ [data_ "toggle" "collapse", href_ "#collapse-type-inference", class_ "btn btn-xs btn-primary"] "点击展开")
     div_ [class_ "collapse", id_ "collapse-type-inference"] $ do
       p_ "这个例子中，每一个值得类型签名都显式地给出："
       haskellPre "main :: IO ()\n\
                  \main = do line :: String <- getLine\n\
                  \          print (parseDigit line)\n\
                  \  where parseDigit :: String -> Maybe Int\n\
                  \        parseDigit ((c :: Char) : _) =\n\
                  \          if isDigit c\n\
                  \             then Just (ord c - ord '0')\n\
                  \             else Nothing"
       p_ "你也可以采用这样的写法："
       haskellPre "main = do line <- getLine\n\
                  \          print (parseDigit line)\n\
                  \  where parseDigit (c : _) =\n\
                  \          if isDigit c\n\
                  \             then Just (ord c - ord '0')\n\
                  \             else Nothing"
       p_ "你可以通过类型推断来避免为了说明你程序中的每一个值 \
         \花费太多的时间："
       haskellPre "do ss <- decode \"[\\\"Hello!\\\",\\\"World!\\\"]\"\n\
                  \   is <- decode \"[1,2,3]\"\n\
                  \   return (zipWith (\\s i -> s ++ \" \" ++ show (i + 5)) ss is)\n\
                  \ => Just [\"Hello! 6\",\"World! 7\"]"
       p_ "类型系统可以给出一个语法分析器的规范而不会带来任何 \
         \额外的负担，下面这样的输入不能并语法解析程序接受："
       haskellPre "do ss <- decode \"[1,2,3]\"\n\
                  \   is <- decode \"[null,null,null]\"\n\
                  \   return (zipWith (\\s i -> s ++ \" \" ++ show (i + 5)) ss is)\n\
                  \ => Nothing"

lazy :: Html ()
lazy =
  do h3_ "惰性求值"
     p_ "函数不会以及对它的参数求值，这意味着程序能够很好地组合在一起， \
       \以及能够仅仅通过普通的函数来实现诸如if/else这样的控制结构。 \
       \Haskell代码的\"纯\"的性质使将函数调用链融合在一起变得更加容易， \
       \这有助于提升程序的性能。"
     p_ [class_ "text-center"] (a_ [data_ "toggle" "collapse", href_ "#collapse-lazy", class_ "btn btn-xs btn-primary"] "点击展开")
     div_ [class_ "collapse", id_ "collapse-lazy"] $ do
       p_ "Haskell中定义程序控制结构是很容易的："
       haskellPre "when p m = if p then m else return ()\n\
                  \main = do args <- getArgs\n\
                  \          when (null args)\n\
                  \               (putStrLn \"No args specified!\") "
       p_ "如果你留意到程序中包含例如这样的："
       haskellPre "if c then t else False"
       p_ "重复出现的表达式模式，你可以给它一个名字，比如这样："
       haskellPre "and c t = if c then t else False"
       p_ "之后你就可以直接使用这个名字并获得与原来的表达式一样的效果。"
       p_ (do "通过组合惰性函数可以实现对代码的复用。很自然地就能想到，可以复用"
              code_ "map"
              " 和 "
              code_ "or"
              " 来表达 "
              code_ "any"
              " 函数：")
       haskellPre "any :: (a -> Bool) -> [a] -> Bool\n\
                  \any p = or . map p"
       p_ (do "通过使用例如"
              code_ "map"; ", "; code_ "filter"; ", "; code_ "foldr"
              "这样的函数可以复用其中包含的递归的程序模式。")

packages :: Html ()
packages =
  do h3_ "包"
     p_ "Haskell社区的开源贡献非常活跃，在Hackage上，可以找到大量开源的Haskell包。"
     p_ [class_ "text-center"] (a_ [data_ "toggle" "collapse", href_ "#collapse-packages", class_ "btn btn-xs btn-primary"] "点击展开")
     div_ [class_ "collapse", id_ "collapse-packages"] $ do
       p_ "目前已经有6,954个Haskell包可以自由获取，其中相当常用的包括："
       table_ [class_ "packages"] $
         forM_ (alternating pkgs)
               (\((name1,desc1),(name,desc)) ->
                  tr_ (do td_ (a_ [href_ ("https://hackage.haskell.org/package/" <> name)] $ toHtml name)
                          td_ (toHtml desc)
                          td_ [class_ "rhs"] $ a_ [href_ ("https://hackage.haskell.org/package/" <> name1)] $ toHtml name1
                          td_ [class_ "rhs"] $ toHtml desc1))
  where pkgs :: [(Text,Text)]
        pkgs =
          [("base"             , "Prelude, IO, threads")
          ,("bytestring"       , "Binary data")
          ,("text"             , "Unicode text")
          ,("network"          , "Networking")
          ,("directory"        , "File/directory")
          ,("parsec"           , "Parser library")
          ,("attoparsec"       , "Fast parser")
          ,("hspec"            , "RSpec-like tests")
          ,("persistent"       , "Database ORM")
          ,("monad-logger"     , "Logging")
          ,("tar"              , "Tar archives")
          ,("template-haskell" , "Meta-programming")
          ,("time"             , "Date, time, etc.")
          ,("snap"             , "Web framework")
          ,("yesod"            , "Web framework")
          ,("happstack"        , "Web framework")
          ,("fsnotify"         , "Watch filesystem")
          ,("containers"       , "Maps, graphs, sets")
          ,("unix"             , "UNIX bindings")
          ,("hint"             , "Interpret Haskell")
          ,("OpenGL"           , "OpenGL graphics system")
          ,("SDL"              , "SDL binding")
          ,("pango"            , "Text rendering")
          ,("criterion"        , "Benchmarking")
          ,("statistics"       , "Statistical analysis")
          ,("cairo"            , "Cairo graphics")
          ,("glib"             , "GLib library")
          ,("gtk"              , "Gtk+ library")
          ,("resource-pool"    , "Resource pooling")
          ,("test-framework"   , "Testing framework")
          ,("mwc-random"       , "High-quality randoms")
          ,("conduit"          , "Streaming I/O")
          ,("stm"              , "Atomic threading")
          ,("QuickCheck"       , "Property testing")
          ,("cereal"           , "Binary parsing/printing")
          ,("blaze-html"       , "Markup generation")
          ,("http-client"      , "HTTP client engine")
          ,("xml"              , "XML parser/printer")
          ,("yaml"             , "YAML parser/printer")
          ,("zlib"             , "zlib/gzip/raw")
          ,("binary"           , "Serialization")
          ,("pandoc"           , "Markup conversion")
          ,("zip-archive"      , "Zip compression")
          ,("tls"              , "TLS/SSL")
          ,("text-icu"         , "Text encodings")
          ,("warp"             , "Web server")
          ,("async"            , "Asyn concurrency")
          ,("vector"           , "Vectors")
          ,("scientific"       , "Arbitrary-prec. nums")
          ,("pipes"            , "Streaming IO")
          ,("aeson"            , "JSON parser/printer")
          ,("process"          , "Launch processes")
          ,("syb"              , "Generic prog.")
          ,("dlist"            , "Difflists")]

alternating :: [t] -> [(t, t)]
alternating (x:y:xs) = (x,y) : alternating xs
alternating _ = []
