-- Main.hs

import Data.Time

-- Product definition
data Product = Product
  { productId :: Int
  , productName :: String
  , productPrice :: Integer  -- In ADA
  } deriving (Show)

-- Customer definition
data Customer = Customer
  { customerName :: String
  , balance :: Integer       -- In ADA
  , location :: String       -- Delivery location
  } deriving (Show)

-- Check if today is within the discount window
isWithinDiscountPeriod :: Day -> Bool
isWithinDiscountPeriod currentDay =
  let discountEnd = fromGregorian 2025 7 3
  in currentDay <= discountEnd

-- Apply 50% discount if eligible
applyDiscount :: Integer -> Bool -> Integer
applyDiscount price eligible =
  if eligible
    then price `div` 2  -- 50% off
    else price

-- Simulate a purchase
purchaseProduct :: Product -> Customer -> Integer -> String
purchaseProduct product customer finalPrice
  | balance customer < finalPrice =
      "❌ " ++ customerName customer ++ " - Insufficient balance (" ++ show (balance customer) ++ " ADA)."
  | otherwise =
      "✅ " ++ customerName customer ++
      " - Paid " ++ show finalPrice ++ " ADA. " ++
      productName product ++ " will be delivered to " ++ location customer ++ "."

-- Main program
main :: IO ()
main = do
  currentDay <- utctDay <$> getCurrentTime
  let discountDeadline = fromGregorian 2025 7 3
  let discountActive = isWithinDiscountPeriod currentDay

  let baseProduct = Product 1 "Cardano Mug" 1500  -- 1500 ADA
  let finalPrice = applyDiscount (productPrice baseProduct) discountActive

  let customers =
        [ Customer "Eze"    300  "Abia State, Nigeria"
        , Customer "Victor" 5000  "Enugu State, Nigeria"
        , Customer "Macy"   1000 "Cross Rivers State, Nigeria"
        ]

  putStrLn "🛍️  Cardano Flash Discount Simulation\n"
  putStrLn $ "🗓️  Current Date: " ++ show currentDay
  putStrLn $ "🎯 Discount Deadline: July 3, 2025"
  putStrLn $ "💰 Original Price: " ++ show (productPrice baseProduct) ++ " ADA"
  if discountActive
    then putStrLn $ "🔥 50% OFF ACTIVE! Discounted Price: " ++ show finalPrice ++ " ADA\n"
    else putStrLn "❌ Discount expired. Full price applies.\n"

  putStrLn "🧾 Purchase Results:\n"
  mapM_ (\c -> putStrLn (purchaseProduct baseProduct c finalPrice)) customers
