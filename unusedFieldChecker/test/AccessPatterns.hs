{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module AccessPatterns where

import UnusedFieldChecker.FieldChecker (FieldChecker(..))
import GHC.Generics (Generic)

-- Test type with all fields used via different access patterns
data Product = Product
    { productName :: String
    , productPrice :: Double
    , productStock :: Int
    , productCategory :: String
    , productDescription :: String
    , productOptional :: Maybe String
    } deriving (Show, Generic)

instance FieldChecker Product where
    excludedFields _ = ["productDescription", "productCategory", "productStock", "productPrice"]

-- 1. Accessor function pattern
getProductInfo :: Product -> String
getProductInfo p =
    productName p ++ " costs $" ++ show (productPrice p)

-- 2. Pattern matching (explicit field binding)
describeProduct :: Product -> String
describeProduct Product { productName = name, productCategory = cat } =
    name ++ " in category: " ++ cat

-- 3. Named field puns (same name for binding)
stockLevel :: Product -> String
stockLevel Product { productStock } =
    "Stock: " ++ show productStock

-- 4. RecordWildCards pattern
fullDescription :: Product -> String
fullDescription Product {..} =
    unwords [productName, show productPrice, productDescription]

-- 5. Record construction
createProduct :: String -> Double -> Int -> String -> String -> Product
createProduct name price stock cat desc =
    Product
        { productName = name
        , productPrice = price
        , productStock = stock
        , productCategory = cat
        , productDescription = desc
        , productOptional = Nothing
        }

-- 6. Record update syntax
updatePrice :: Product -> Double -> Product
updatePrice product newPrice =
    product { productPrice = newPrice }

-- More complex patterns
data Customer = Customer
    { customerId :: Int
    , customerName :: String
    , customerEmail :: String
    , customerPhone :: Maybe String
    } deriving (Show, Generic)

instance FieldChecker Customer where
    excludedFields _ = ["customerName", "customerId"]

-- 7. Multiple pattern matches
customerContact :: Customer -> String
customerContact Customer { customerEmail = email, customerPhone = Just phone } =
    "Email: " ++ email ++ ", Phone: " ++ phone
customerContact Customer { customerEmail = email } =
    "Email: " ++ email

-- 8. Nested record access
data Order = Order
    { orderId :: Int
    , orderCustomer :: Customer
    , orderProduct :: Product
    , orderQuantity :: Int
    , orderNotes :: Maybe String
    } deriving (Show, Generic)

instance FieldChecker Order where
    excludedFields _ = ["orderId", "orderCustomer", "orderProduct", "orderQuantity"]

orderSummary :: Order -> String
orderSummary order =
    let customer = orderCustomer order
        product = orderProduct order
    in customerName customer ++ " ordered " ++
       show (orderQuantity order) ++ "x " ++
       productName product

-- 9. Case expression pattern matching
processOrder :: Order -> String
processOrder ord = case ord of
    Order { orderQuantity = qty, orderId = oid } ->
        "Order #" ++ show oid ++ " for " ++ show qty ++ " items"

-- 10. Let binding with pattern matching
calculateTotal :: Order -> Double
calculateTotal order =
    let Order { orderQuantity, orderProduct } = order
        Product { productPrice } = orderProduct
    in fromIntegral orderQuantity * productPrice

-- 11. Where clause with pattern matching
formatOrder :: Order -> String
formatOrder order = summary
  where
    Order { orderId, orderCustomer } = order
    Customer { customerName } = orderCustomer
    summary = "Order " ++ show orderId ++ " by " ++ customerName

-- 12. Guards with pattern matching
checkStock :: Order -> String
checkStock order@Order { orderQuantity }
    | orderQuantity > productStock (orderProduct order) = "Out of stock"
    | orderQuantity > 0 = "In stock"
    | otherwise = "Invalid quantity"

-- 13. Function composition with accessor
getCustomerEmails :: [Order] -> [String]
getCustomerEmails = map (customerEmail . orderCustomer)

-- 14. Record construction in do block
exampleOrder :: IO Order
exampleOrder = do
    let cust = Customer
            { customerId = 1
            , customerName = "Alice"
            , customerEmail = "alice@example.com"
            , customerPhone = Just "555-1234"
            }
    let prod = createProduct "Widget" 9.99 100 "Tools" "A useful widget"
    return Order
        { orderId = 101
        , orderCustomer = cust
        , orderProduct = prod
        , orderQuantity = 2
        , orderNotes = Nothing
        }

-- 15. Multiple field updates
updateOrder :: Order -> Int -> String -> Order
updateOrder order newQty newEmail =
    order
        { orderQuantity = newQty
        , orderCustomer = (orderCustomer order) { customerEmail = newEmail }
        }

-- 16. Lambda with pattern matching
filterLargeOrders :: [Order] -> [Order]
filterLargeOrders = filter (\Order { orderQuantity } -> orderQuantity > 10)

-- 17. Partial pattern in lambda
getOrderIds :: [Order] -> [Int]
getOrderIds = map (\Order { orderId } -> orderId)

-- Test with all access patterns
testAllPatterns :: IO ()
testAllPatterns = do
    order <- exampleOrder
    putStrLn $ getProductInfo (orderProduct order)
    putStrLn $ describeProduct (orderProduct order)
    putStrLn $ stockLevel (orderProduct order)
    putStrLn $ fullDescription (orderProduct order)
    putStrLn $ customerContact (orderCustomer order)
    putStrLn $ orderSummary order
    putStrLn $ processOrder order
    putStrLn $ show $ calculateTotal order
    putStrLn $ formatOrder order
    putStrLn $ checkStock order
