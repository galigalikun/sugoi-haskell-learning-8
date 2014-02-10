# すごいHaskell楽しく学ぼう！輪読会

http://sugoihaskell.github.io/

2014/2/10 by gfx

## 第8章 入出力


## 8.1 不順なものと純粋なものを分離する

* 人間がコンピュータに与えるのはあるものが何であるかの定義である
  * 一連の実行ステップではない
* 関数が **副作用** を持つことは許されない
* 関数が同じ引数で2回呼び出されたら、 **必ず** 同じ結果を返す

> プログラミングにおける副作用（ふくさよう）とは、ある機能がコンピュータの(論理的な)状態を変化させ、それ以降で得られる結果に影響を与えることをいう。代表的な例は変数への値の代入である。(from Wikipedia)

* 前章では二分探索木をつくったが、insertで木を変更して要素を挿入するのではなく、要素を挿入した新しい木を返していた
* しかし副作用がなければ、計算結果をどこか（たとえばコンソール）に伝えることが出来ない
* そこで、Haskellは副作用を扱うためのシステムをもっている

## 8.2 Hello, World!

* ようやく本物のHaskellプログラム！！

```haskell
main = putStrLn "hello, world"
```

```shell
$ ghc --make helloworld
$ ./helloworld
```

* putStrLnの型

```ghci
ghci> :t putStrLn
putStrLn :: String -> IO ()
ghci> :t putStrLn "hello, world"
putStrLn "hello, world" :: IO ()
```

* putStrLnの型 :: Stringを引数に取り、unitを結果とする **I/Oアクション** を返す
* I/Oアクションとは、実行されると副作用を含む動作をして結果を返すような何か
* unit (空のタプル) は何もないことを示す値として使われる
  * unitは値も型も `()` で表現する
* I/Oアクションはmainという名前をつけると実行される
  * または、ghci上で評価すると実行される

## 8.3 IOアクションどうしをまとめる

## doブロック

複数のIOアクションを一つにまとめるための構文

```haskell
-- do-actions.hs
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hey " ++ name ++ ", you rock!"
```

※ 本文にはmainの型は明示しないとあるが、訳注によると明示するのが一般的らしい

一行一行バラして型を補った版

```haskell
-- do-actions2.hs
makeQuestion :: IO ()
makeQuestion = putStrLn "Hello, what's your name?"

getAnswer :: IO String
getAnswer = getLine

printName :: String -> IO ()
printName name = putStrLn $ "Hey " ++ name ++ ", you rock!"

main :: IO ()
main = do
    makeQuestion
    name <- getAnswer
    printName name

```

* doと書いてから、一行一行実行ステップ（IOアクション）を書き並べる
* doはブロックの最後の値（IOアクション）を返す。この例だと `IO ()`

## IOアクションの結果の束縛 `name <- getLine`

* 「IOアクションgetLineを実行して、それからその結果の値をnameに束縛せよ」という意味
  * nameの型はStringになる

## I/O アクション

* I/Oアクションは小さな足がついた箱
* 実世界に出て行って、そこで何かを行い、なにか値を持って帰ってくる
* 箱の中身のデータを手に入れる唯一の方法が `<-`
* getLineは不純なので２回実行すると同じ結果を返す保証はない
* nameはただのStringで純粋


不純なコードと純粋なコードを分けて書いたプログラム:

```haskell
-- fortune.hs
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Zis is your future: " ++ tellFortune name

-- tellFortuneは純粋なコード
tellFortune :: String -> String
tellFortune "Goro" = "good"
tellFortune name = "so so"
```

* では、以下のコードは有効か？

```haskell
nameTag = "Hello, my name is " ++ getLine
```

* No! String と IO String の連結はできない
* 不純なコードは純粋なコードに混ぜ合わせることはできない

* IO () もバインドすることができる: `foo <- putStrLn "..."`
  * このとき foo は `()` になる

以下のコードはなにが起きるか？

```haskell
myLine = getLine
```

* これは、`getLine` に別名をつけるだけ
  * `myLine :: IO String`
* IOアクションが **実行** されるのは、mainという名前をあたえられたときと他のdoブロックの中のみ（+ghci)

### IOアクションのなかでletを使う

* 純粋な値を名前に束縛するにはletを使う

```
-- let.hs
import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName  = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

```

### 逆順に表示する

* IOアクションに慣れるために、単語を逆順にして表示するプログラムを書いてみよう

```haskell
-- reverse.hs
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
-- reverseWords s = unwords  (map reverse  (words s))
```

* `getLine` は改行を除いた値を返す
* `null []` が `True` なので `null ""` も `True`
* `else` は IOアクションただひとつを受け取るので、doブロックをつかって2つのIOアクションを1つにまとめている

else文は以下でもいい

```haskell
else (do
	putStrLn $ reverseWords line
	main)
```

* `return ()` は他の言語のreturn文ではない
* `return ()` は純粋な値をIOアクションでくるむ
  * 実際にはMonadでくるむのでIOアクション以外でもつかえるっぽい
  * e.g. `let a = return "foo" :: IO String` とすると `a :: IO String` が得られる
* 上記reverseプログラムで `then return ()` とすしているのは、mainがIOアクションを要求する (`main :: IO ()`) から

## 8.4 いくつかの便利なIO関数

（ここでは以下の `Monad a` を `IO a` と読み替えてよい）

* `putStr :: String -> IO ()`
* `putChar :: Char -> IO ()`
* `print :: Show a => a -> IO ()`
  * Rubyの `p()` みたいなもの
* `when :: Monad m => Bool -> m () -> m ()` defined in Control.Monad
  * boolとIOアクションをうけとり、boolがTrueの時に渡されたIOアクションを返す
  * if-then-elseのelseが不要なときに使う
* `sequence :: Monad m => [m a] -> m [a]`
  * `[IO a]` を `IO [a]` に変換する
  * 使い方みるのがはやい

```haskell
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a, b, c]

main' = do
    s <- sequence [getLine, getLine, getLine]
    print s
```

* `mmpM :: Monad m => (a -> m b) -> [a] -> m [b]`
  * リストにIOアクションを適用してひとつのIOアクションにまとめる
  * `mapM  print [1, 2, 3] :: IO[()]`
  * `mapM_ print [1, 2, 3] :: IO ()`
* `forever :: Monad m => m a -> m b` defined in Control.Monad
* `forM :: Monad m => [a] -> (a -> m b) -> m [b]` defined in Control.Monad
  * mapMとほぼ同じだが、引数の順番が異なる
    * mapMとは、どの引数が長くなるかによって使い分ける
  * `forM  [1, 2, 3] print :: IO[()]`
  * `forM_ [1, 2, 3] print :: IO ()`

```haskell
-- forM.hs
import Control.Monad

main = do
    colors <- forM [1, 2, 3, 4] $ \a -> do
        putStrLn $ "Which color do you associate with the number "
            ++ show a ++ "?"
        color <- getLine
        return color
    putStrLn "The color that you associate with 1, 2, 3, and 4 are: "
    mapM_ putStrLn colors
```

## 8.5 IOアクションおさらい

* IOアクション「実世界とやりとりする何か」をくるんだ値
  * 関数の引数として渡したり、戻り値として返したりできる
* main関数の中に入っていると実行される
* 実世界との情報のやりとりを行う

## NOTES

doに IO() を書くと queue につまれ、どこかの時点でそのqueueが順番に消化されるイメージ。`jQuery.Deferred` などもモナドといわれるが、これは非同期処理をqueueに突っ込んで直列に実行するためのもの。

cf. 

```js
var deferred = $.Deferred();

deferred
  .then(function() {
     console.log('0');
  })
  .then(function() {
    console.log('1');
  })
  .then(function() {
    console.log('2');
  });

// ここではまだ何も処理されない
deferred.resolve(); // ここではじめてqueue入れられた関数が実行される
```

I/Oアクションの順番をならびかえる

```
main :: IO ()

main = do
    putStrLn "A"
    let b = putStrLn "B"
    let c = putStrLn "C"
    c
    b
```

