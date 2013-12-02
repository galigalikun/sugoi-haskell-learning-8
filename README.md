## 第8章 入出力

## 8.1 不順なものと純粋なものを分離する

* 人間がコンピュータに与えるのはあるものが何であるかの定義である
  * 一連の実行ステップではない
* 関数が **副作用** を持つことは許されない
* 関数が同じ引数で2回呼び出されたら、必ず同じ結果を返す

> プログラミングにおける副作用（ふくさよう）とは、ある機能がコンピュータの(論理的な)状態を変化させ、それ以降で得られる結果に影響を与えることをいう。代表的な例は変数への値の代入である。(from Wikipedia)

* 前章では二分探索木をつくたが、insertで木を変更して要素を挿入するのでｈなく、要素を挿入した新しい木を返していた
* しかし副作用がなければ、計算結果をどこかに伝えることが出来ない
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
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hey " ++ name ++ ", you rock!"
```

※ 本文にはmainの型は明示しないとあるが、訳注には明示するのが一般的らしい

一行一行バラして型を補った版

```haskell
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

## 束縛文 `name <- getLine`

## 8.4 いくつかの便利なIO関数

## 8.5 IOアクションおさらい

* IOアクションは値
  * 関数の引数として渡したり、戻り値として返したりできる
* main関数の中に入っていると実行される
* 実世界とのインタラクションを行う


