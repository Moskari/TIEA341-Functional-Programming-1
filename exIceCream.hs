--Samuli Rahkonen

--1.

type JobTitle = String
--"Kesähessu"

type PayAccount = Maybe String
--Just "12345"

type Salary = Maybe Double
--Just 3000

type Sandwiches = Int
--10

type Performance = (Sandwiches)
--10

type PhoneNumber = String

type Contact = (PhoneNumber, Address)
--("040231231")

-- This is further expanded later below with an example
--type Employee = (JobTitle, Contact, PayAccount, Salary, Performance) 

type Employees = [Employee]

--2.



type Street = String
--"Turhakatu 1"

type Coordinates = (Double, Double) -- Longitude, Latitude
--(23.760954, 61.497752)

type Location = Either (Maybe Street) (Maybe Coordinates)
--Left Just "Turhakatu 2"

type Booth = (Location, Employee)


--3.

type Name = String
--"Samuli Rahkonen"

type PO = Int
--12342

-- type Street = String
--"Turhakatu 1"

type Address = (Name, Street, PO)
--("Samuli Rahkonen", "Turhakatu 1", 2213)

--type Employee = (Name, Address)
type Employee = (Name,JobTitle, Contact, PayAccount, Salary, Performance)
--("Matti Meikalainen", "Kesähessu", ("040321321", ("Matti Meikalainen", "Turhakatu 1", 1234)), Just "12345", Just 3000, 10)


--4.

type ProtocolName = String
--"http"

--5.

type Title = Maybe String
--Just "MSc"
          