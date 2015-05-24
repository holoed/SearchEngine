module Main where

import           Data.Map      (toList)
import           InvertedIndex
import           Test.Hspec

main :: IO ()
main = hspec $ do

  describe "numLines tests" $ do

    it "should return first line with index" $
      numLines "Hello World" `shouldBe` [("Hello World", 1)]

    it "should return two lines with index" $
      numLines "Hello\nWorld" `shouldBe` [("Hello", 1), ("World", 2)]

    it "should return n lines with index" $
      numLines "Welcome to\nReal World\nof Haskell"
       `shouldBe` [("Welcome to", 1), ("Real World", 2), ("of Haskell", 3)]

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
        `shouldBe` [("Hello", [1])]

    it "should create an index" $ do
      (toList . createIndex) "Hello World"
        `shouldBe` [("Hello", [1]), ("World", [1])]

      (toList . createIndex) "Back to the Future\nA few good man"
        `shouldBe`[("A",[2]),("Back",[1]),("Future",[1]),("few",[2]),
                   ("good",[2]),("man",[2]),("the",[1]),("to",[1])]

      (toList . createIndex) "A few good men\nAll the President's men"
        `shouldBe`[("A",[1]),("All",[2]),("Presidents",[2]),("few",[1]),
                   ("good",[1]),("men",[2,1]),("the",[2])]

      (toList . createIndex) "Back to the future\nIn to the future\nBack in time"
        `shouldBe`[("Back",[3,1]),("In",[2]),("future",[2,1]),
                   ("in",[3]),("the",[2,1]),("time",[3]),("to",[2,1])]

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
       search index "Hello" `shouldBe` [(1,1)]

     it "should find word in two words document" $ do
       let index = createIndex "Hello World"
       search index "World" `shouldBe` [(1,1)]

     it "should find word in two lines doc" $ do
       let index = createIndex "Hello\nWorld"
       search index "World" `shouldBe` [(2, 1)]

     it "should find most likely line" $ do
       let index = createIndex (
              "Back to the future\n" ++
              "Behind enemy lines\n" ++
              "Behind the planet of the apes\n" ++
              "Planet of the apes")
       search index "Behind apes" `shouldBe` [(3,2),(4,1),(2,1)]
