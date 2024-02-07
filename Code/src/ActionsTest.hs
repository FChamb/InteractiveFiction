import Actions
import World
import Test.QuickCheck

main :: IO ()
main = do quickCheck move
          quickCheck objectHere
          quickCheck removeObject
          quickCheck addObject
          quickCheck findObj
          quickCheck objectData
          quickCheck updateRoom
          quickCheck addInv
          quickCheck removeInv
          quickCheck carrying

instance Arbitrary Direction where
    arbitrary = elements [North, East, West, South, Out, In]

instance Arbitrary Room where
  arbitrary = elements [bedroom, bathroom, kitchen, hall, street]

instance Arbitrary Object where
    arbitrary = elements [mug, fullmug, milkyCoffeeMug, emptyMilk, coffeepot, torch, emptyTorch, batteries, toothbrush, usedToothbrush, shower, lightswitch, milk, eggs, bread]

--instance Arbitrary GameData where
    --arbitrary = elements [GameData "bedroom" gameworld [] False False False False False False False False False False]

instance Testable Room where
    --testable = elements [bedroom, bathroom, kitchen, hall, street]

instance Testable Object where
    --testable = elements [mug, fullmug, milkyCoffeeMug, emptyMilk, coffeepot, torch, emptyTorch, batteries, toothbrush, usedToothbrush, shower, lightswitch, milk, eggs, bread]

instance Testable GameData where
    --testable = elements [GameData "bedroom" gameworld [] False False False False False False False False False False]
