module ListyInstances where

import Data.Monoid
import Listy

-- ถ้า copy instances เหล่านี้ไปใสในไฟล์ Listy
-- แล้วลอง compile, instance มันจะชนกัน
instance Semigroup (Listy a) where
  (<>) (Listy l) (Listy l') = Listy $ (<>) l l'

-- หรือถ้าเราไม่ได้ import Listy แต่ behaviour
-- ของ instance ก็ predict ไม่ได้อยู่ดี(เพราะขึ้นอยู่กับ module ที่ import)
instance Monoid (Listy a) where
  mempty  = Listy []
