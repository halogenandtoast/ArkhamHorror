module Arkham.Treachery.Cards.LegInjurySpec (spec) where

import Arkham.Matcher qualified as Matcher
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Leg Injury" do
  it "does not count as damage for InvestigatorWithDamage thresholds" . gameTest $ \self -> do
    withProp @"damage" 4 self
    _ <- self `putTreacheryIntoPlay` Treacheries.legInjury

    select (Matcher.InvestigatorWithDamage $ Matcher.atLeast 5) `shouldReturn` []

  it "still makes the investigator damage-healable" . gameTest $ \self -> do
    _ <- self `putTreacheryIntoPlay` Treacheries.legInjury

    select (Matcher.HealableInvestigator GameSource #damage Matcher.Anyone) `shouldReturn` [toId self]
