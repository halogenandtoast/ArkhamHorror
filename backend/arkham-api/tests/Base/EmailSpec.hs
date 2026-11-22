module Base.EmailSpec (spec) where

import Base.Email (normalizeEmail)
import Test.Hspec

spec :: Spec
spec = describe "normalizeEmail" do
  it "trims whitespace and ignores letter case" do
    normalizeEmail "  JuAnGHiRi@Hotmail.COM  " `shouldBe` "juanghiri@hotmail.com"

  it "is idempotent" do
    normalizeEmail (normalizeEmail "JuAn@example.com") `shouldBe` normalizeEmail "JuAn@example.com"
