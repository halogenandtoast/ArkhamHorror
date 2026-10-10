module Arkham.Homebrew.TheMasqueOfTheRedDeath.Locations.VioletChamber (violetChamber) where

import Arkham.Card (cardMatch, card_)
import Arkham.ChaosToken (pattern NegativeModifier)
import Arkham.ChaosToken.Types (ChaosTokenValue (ChaosTokenValue))
import Arkham.Helpers.History (HistoryField (HistoryPlayedCards), getHistoryField, playedCard)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMaybe)
import Arkham.Helpers.SkillTest (getSkillTestSource, withSkillTest)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (
  chamberToll,
  describedSkullEffect,
  tollDoomMayAdvanceAgenda,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype VioletChamber = VioletChamber LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

violetChamber :: LocationCard VioletChamber
violetChamber =
  locationWith VioletChamber Cards.violetChamber 2 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ chamberToll

instance HasModifiersFor VioletChamber where
  -- "Each non-weakness event in the hand of an investigator at Violet Chamber gets
  -- +X cost, where X is the number of events that investigator has played this round."
  getModifiersFor (VioletChamber a) = whenRevealed a do
    modifySelectMaybe a (investigatorAt a) \iid -> do
      played <- getHistoryField #round iid HistoryPlayedCards
      let n = count ((`cardMatch` card_ #event) . playedCard) played
      guard (n > 0)
      pure [IncreaseCostOf (inHandOf ForPlay iid <> basic (#event <> NonWeakness)) n]

instance HasAbilities VioletChamber where
  -- "[skull]: -2. -0 instead if this test is printed on an event."
  getAbilities (VioletChamber a) =
    extendRevealed1 a $ describedSkullEffect 0 "-2. -0 instead if this test is printed on an event." a 1

instance RunMessage VioletChamber where
  runMessage msg l@(VioletChamber attrs) = runQueueT do
    tollDoomMayAdvanceAgenda attrs msg
    case msg of
      UseThisAbility _ (isSource attrs -> True) 1 -> do
        onEvent <-
          maybe (pure False) (`sourceMatches` SourceIsEvent AnyEvent) =<< getSkillTestSource
        unless onEvent do
          withSkillTest \sid ->
            skillTestModifier sid (attrs.ability 1) sid
              $ AddChaosTokenValue (ChaosTokenValue #skull (NegativeModifier 2))
        pure l
      _ -> VioletChamber <$> liftRunMessage msg attrs
