--Samuli Rahkonen

data JobTitle = Kesähessu | Johtaja | Myyjä | Orja deriving (Show, Eq, Ord)

data Performance = Sandwiches Int
                 | IceCreams Int deriving (Show, Eq)

data Location = Street String | Coordinates Double Double deriving (Show, Eq)

data PhoneNumber = PhoneNumber String deriving (Eq, Show)

data Name = Name String String deriving (Eq, Show)

data PO = PO Int deriving (Show, Eq)

data Address = Address Name Location PO deriving (Show, Eq)

data Contact = Contact PhoneNumber Address deriving (Show, Eq)

data PayAccount = PayAccount String deriving (Eq, Show)

data Salary = Salary Performance deriving (Show, Eq)

data Employee = Employee Name JobTitle Contact PayAccount Salary Performance deriving (Eq, Show)
-- Example: Employee (Name "Matti" "Meikalainen") Kesähessu (Contact (PhoneNumber "04012345") (Address (Name "Matti" "Meikalainen") (Street "Turhakatu 1") (PO 4040))) (PayAccount "1001010-1233") (Salary (Sandwiches 13))


data Employees = Employees [Employee] deriving (Show)


data Booth = Booth Employee Location deriving (Show, Eq)

data ProtocolName = HTTP | FTP deriving (Show, Eq)
