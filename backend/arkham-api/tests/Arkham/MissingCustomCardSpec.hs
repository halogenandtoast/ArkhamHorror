module Arkham.MissingCustomCardSpec (spec) where

import Arkham.Card (cbCardBuilder)
import Arkham.Card.CardType (CardType (InvestigatorType))
import Arkham.Card.CustomCard (isMissingCustomCard, missingCustomCardDef)
import Arkham.Card.Id (nullCardId)
import Arkham.ClassSymbol (ClassSymbol (Neutral))
import Arkham.Classes.Entity (toAttrs)
import Arkham.Custom.Investigator (customInvestigator)
import Arkham.Id (PlayerId (..))
import Arkham.Investigator.Types (InvestigatorAttrs (investigatorClass))
import Control.Exception (evaluate)
import Data.UUID (nil)
import Test.Hspec
import Prelude

-- The stand-in for a custom card whose def is gone exists so a missing def costs
-- that one card rather than the game. An investigator is the one entity built
-- from a field the stand-in has no obvious value for -- its class -- and for a
-- long time it had none, so building one threw and took the request with it
-- (#5891).
spec :: Spec
spec = describe "the missing custom card stand-in" do
  let def = missingCustomCardDef InvestigatorType "*00000000000000000000000000000000"

  it "says it is a stand-in" do
    isMissingCustomCard def `shouldBe` True

  it "builds as an investigator" do
    let i = cbCardBuilder (customInvestigator def) nullCardId (PlayerId nil)
    cls <- evaluate (investigatorClass (toAttrs i))
    cls `shouldBe` Neutral
