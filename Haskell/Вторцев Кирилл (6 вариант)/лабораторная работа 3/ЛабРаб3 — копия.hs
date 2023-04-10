{-задание 6
¬ некотором €зыке программировани€ существуют следующие типы данных:
- ѕростые типы: целые, вещественные и строки
- —ложные типы: структуры. —труктура имеет название и состоит из нескольких 
полей каждое из которых в свою очередь имеет название и простой тип.
Ѕаза данных идентификаторов программы представл€ет собой список пар, 
состо€щих из имени идентификатора и его типа. –азработайте тип данных, 
представл€ющий описанную информацию. ќпределите след. ф-ции:

1) isStructured, провер€юща€, что еЄ аргумент €вл€етс€ сложным типом.

2) getType, по заданному имени и списку идентификаторов (базе данных)
 возвращающа€ тип идентификатора с указанным именем (помните о том, 
что такого идентификатора в базе может и не оказатьс€).

3) getFields, по заданному имени возвращающа€ список полей идентификатора,
 если он имеет тип структуры.

4) getByType, возвращающа€ список идентификаторов указанного типа 
из базы данных.

5) getByTypes, аналогична€ предыдущей, но принимающей вместо одного 
типа список типов (с помощью этой ф-ции можно получить, например, 
список всех идентификаторов с числовым типом).-}

data MyType = Null |
           MyInt |
              MyFloat |
              MyString |
              MyStruct [(String,MyType)] deriving (Show,Eq)

db = [("x",MyInt),
      ("y",MyFloat),
      ("z",MyString),
      ("u",MyInt),
      ("s",MyStruct [("s1",MyInt),("s2",MyFloat)])]

isStructured :: MyType -> Bool
isStructured (MyStruct b) = True
isStructured (_) = False

getType :: String -> [(String,MyType)] -> MyType
getType _ [] = Null
getType s (h:t) = if fst(h) == s then snd(h) else getType s t

getFieldsFromStruct :: MyType -> [(String,MyType)]
getFieldsFromStruct (MyStruct a) = a
getFieldsFromStruct _ = []

getFields :: String -> [(String,MyType)] -> [(String,MyType)]
getFields _ [] = []
getFields s (h:t) = if fst(h) == s && isStructured (snd h) then
               getFieldsFromStruct (snd h)
                    else
                        getFields s t

getByType :: MyType -> [(String,MyType)] -> [String]
getByType _ [] = []
getByType a (h:t) = if a == (snd h) then
                        (fst h) : getByType a t
                    else
                        getByType a t

getByTypes :: [MyType] -> [(String,MyType)] -> [String]
getByTypes [] _ = []
getByTypes _ [] = []
getByTypes (h:t) db = (getByType h db) ++ getByTypes t db





