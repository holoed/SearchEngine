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
      cleanWords ["Hello", "World"] `shouldBe` ["Hello", "World"]

    it "should remove numbers" $
      cleanWords ["Welcome6"] `shouldBe` ["Welcome"]

    it "should remove non ascii chars" $
      cleanWords ["This,", "is", "an", "experiment'", "of", "great", "%"]
        `shouldBe` ["This", "is", "an", "experiment", "of", "great"]

  describe "allNumWords tests" $ do

    it "should convert lines in to words keeping line numbers" $
      allNumWords [("Welcome to", 1), ("the real", 2)]
        `shouldBe` [("Welcome", 1), ("to", 1), ("the", 2), ("real", 2)]

    it "should clean while splitting into words" $
      allNumWords [("Lo ha detto papa'", 1), ("che va' bene, capito", 2)]
        `shouldBe` [("Lo", 1), ("ha", 1), ("detto", 1), ("papa", 1),
                    ("che", 2), ("va", 2), ("bene", 2), ("capito", 2)]

  describe "createIndex tests" $ do

    it "should create an empty index for an empty document" $
      (toList . createIndex) ""
        `shouldBe` []

    it "should create an index of one element for a single word doc" $
      (toList . createIndex) "Hello"
        `shouldBe` [("Hello", [0])]

    it "should create an index" $ do
      (toList . createIndex) "Hello World"
        `shouldBe` [("Hello", [0]), ("World", [0])]

      (toList . createIndex) "Back to the Future\nA few good man"
        `shouldBe`[("A",[1]),("Back",[0]),("Future",[0]),("few",[1]),
                   ("good",[1]),("man",[1]),("the",[0]),("to",[0])]

      (toList . createIndex) "A few good men\nAll the President's men"
        `shouldBe`[("A",[0]),("All",[1]),("Presidents",[1]),("few",[0]),
                   ("good",[0]),("men",[1,0]),("the",[1])]

      (toList . createIndex) "Back to the future\nIn to the future\nBack in time"
        `shouldBe`[("Back",[2,0]),("In",[1]),("future",[1,0]),
                   ("in",[2]),("the",[1,0]),("time",[2]),("to",[1,0])]

  describe "frequencies tests" $ do

    it "should return frequency of one for one item" $
      frequencies "x" `shouldBe` [('x', 1)]

    it "should return frequencies of more item" $ do
      frequencies "xyz" `shouldBe` [('z', 1), ('y', 1), ('x', 1)]
      frequencies "xyx" `shouldBe` [('x', 2), ('y', 1)]
      frequencies "xyyxy" `shouldBe` [('y', 3), ('x', 2)]

  describe "search tests" $ do

     it "should find single word document" $ do
       let index = createIndex "Hello"
       search index "Hello" `shouldBe` [(0,1)]

     it "should find word in two words document" $ do
       let index = createIndex "Hello World"
       search index "World" `shouldBe` [(0,1)]

     it "should find word in two lines doc" $ do
       let index = createIndex "Hello\nWorld"
       search index "World" `shouldBe` [(1, 1)]

     it "should find most likely line" $ do
       let index = createIndex (
              "Back to the future\n" ++
              "Behind enemy lines\n" ++
              "Behind the planet of the apes\n" ++
              "Planet of the apes")
       search index "Behind apes" `shouldBe` [(2,2),(3,1),(1,1)]

  describe "Integration tests" $ do

    it "should work" $ do
      movies <- readFile "./tests/SampleData.txt"
      let index = createIndex movies
      head (search index "Cruise Hackman") `shouldBe` (2, 2)
      head (search index "Cruise Nicholson") `shouldBe` (0, 2)
      take 3 (search index "Robert De Niro") `shouldBe` [(8, 3), (7, 3)]
      head (search index "Washington Ethan") `shouldBe` (5, 2)
