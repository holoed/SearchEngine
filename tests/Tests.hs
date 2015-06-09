module Main where

import Data.Map      (toList)
import InvertedIndex
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "numLines tests" $ do

    it "should return first line with index" $
      numLines "Hello World" `shouldBe` [("Hello World", 0)]

    it "should return two lines with index" $
      numLines "Hello\nWorld" `shouldBe` [("Hello", 0), ("World", 1)]

    it "should return n lines with index" $
      numLines "Welcome to\nReal World\nof Haskell"
       `shouldBe` [("Welcome to", 0), ("Real World", 1), ("of Haskell", 2)]

  describe "cleanWords tests" $ do

    it "should return words" $
      cleanWords ["Hello", "World"] `shouldBe` ["hello", "world"]

    it "should remove numbers" $
      cleanWords ["Welcome6"] `shouldBe` ["welcome"]

    it "should remove non ascii chars" $
      cleanWords ["This,", "is", "an", "experiment'", "of", "great", "%"]
        `shouldBe` ["this", "is", "an", "experiment", "of", "great"]

  describe "allNumWords tests" $ do

    it "should convert lines in to words keeping line numbers" $
      allNumWords [("Welcome to", 0), ("the real", 1)]
        `shouldBe` [("welcome", 0, 0), ("to", 1, 0), ("the", 0, 1), ("real", 1, 1)]

    it "should clean while splitting into words" $
      allNumWords [("Lo ha detto papa'", 0), ("che va' bene, capito", 1)]
        `shouldBe` [("lo", 0, 0), ("ha", 1, 0), ("detto", 2, 0), ("papa", 3, 0),
                    ("che", 0, 1), ("va", 1, 1), ("bene", 2, 1), ("capito", 3, 1)]

  describe "createIndex tests" $ do

    it "should create an empty index for an empty document" $
      (toList . createIndex) ""
        `shouldBe` []

    it "should create an index of one element for a single word doc" $
      (toList . createIndex) "Hello"
        `shouldBe` [("hello", [(0, 0)])]

    it "should create an index" $ do
      (toList . createIndex) "Hello World"
        `shouldBe` [("hello", [(0, 0)]), ("world", [(1, 0)])]

      (toList . createIndex) "Back to the Future\nA few good man"
        `shouldBe`[("a",[(0, 1)]),("back",[(0,0)]),("few",[(1,1)]),("future",[(3,0)]),
                   ("good",[(2, 1)]),("man",[(3, 1)]),("the",[(2, 0)]),("to",[(1, 0)])]

      (toList . createIndex) "A few good men\nAll the President's men"
        `shouldBe`[("a",[(0, 0)]),("all",[(0, 1)]),("few",[(1, 0)]),
                   ("good",[(2, 0)]),("men",[(3, 1), (3, 0)]),("presidents",[(2, 1)]),("the",[(1, 1)])]

      (toList . createIndex) "Back to the future\nIn to the future\nBack in time"
        `shouldBe`[("back",[(0,2),(0,0)]),("future",[(3,1),(3,0)]),("in",[(1,2), (0,1)]),
                   ("the",[(2,1),(2,0)]),("time",[(2, 2)]),("to",[(1,1),(1,0)])]

  describe "search tests" $ do

     it "should find single word document" $ do
       let index = createIndex "Hello"
       search index "Hello" `shouldBe` [("hello", 0,0)]

     it "should find word in two words document" $ do
       let index = createIndex "Hello World"
       search index "World" `shouldBe` [("world", 1,0)]

     it "should find word in two lines doc" $ do
       let index = createIndex "Hello\nWorld"
       search index "World" `shouldBe` [("world", 0, 1)]

     it "should find most likely line" $ do
       let index = createIndex (
              "Back to the future\n" ++
              "Behind enemy lines\n" ++
              "Behind the planet of the apes\n" ++
              "Planet of the apes")
       search index "Behind enemy" `shouldBe` [("behind",0, 1), ("enemy", 1, 1)]

  describe "Partial term search" $ do

    it "should find item with partial term" $ do
      let index = createIndex("Back to the future\n" ++
                              "Behind emeny lines\n")
      search index "fut" `shouldBe` [("fut", 3, 0)]
      search index "back to the fut" `shouldBe` [("back",0,0),("to",1,0),("the",2,0),("fut",3,0)]

  describe "Integration tests" $ do

    it "should find movies" $ do
      movies <- readFile "./tests/SampleData.txt"
      let index = createIndex movies
      search index "Cruise Hackman" `shouldBe` [("cruise",3,2),("hackman",5,2)]
      search index "Cruise Nicholson" `shouldBe` []
      search index "Robert De Niro" `shouldBe`  [("robert",5,7),("de",6,7),("niro",7,7),("robert",2,8),("de",3,8),("niro",4,8)]
      search index "Washington Ethan" `shouldBe` [("washington",3,5),("ethan",4,5)]
      search index "Hack" `shouldBe` [("hack",5,2),("hack",3,4),("hack",4,6)]
