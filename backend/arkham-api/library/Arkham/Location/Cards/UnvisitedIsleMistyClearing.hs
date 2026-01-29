module Arkham.Location.Cards.UnvisitedIsleMistyClearing (unvisitedIsleMistyClearing) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleMistyClearing = UnvisitedIsleMistyClearing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleMistyClearing :: LocationCard UnvisitedIsleMistyClearing
unvisitedIsleMistyClearing = location UnvisitedIsleMistyClearing Cards.unvisitedIsleMistyClearing 1 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleMistyClearing where
  getModifiersFor (UnvisitedIsleMistyClearing a) = whenUnrevealed a $ maybeModifySelf a do
    sidedWithLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
    isLit <- selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
    guard $ if sidedWithLodge then not isLit else isLit
    pure [Blocked]

instance HasAbilities UnvisitedIsleMistyClearing where
  getAbilities (UnvisitedIsleMistyClearing attrs) =
    extendRevealed
      attrs
      [ skillTestAbility $ restricted attrs 1 Here $ ActionAbility [Action.Circle] Nothing $ ActionCost 1
      , scenarioI18n $ hauntedI "unvisitedIsleMistyClearing.haunted1" attrs 2
      ]

instance RunMessage UnvisitedIsleMistyClearing where
  runMessage msg l@(UnvisitedIsleMistyClearing attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      circleTest sid iid (attrs.ability 1) attrs [#willpower, #agility] (Fixed 11)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseOneM iid do
        scenarioI18n $ labeled' "unvisitedIsleMistyClearing.doom" $ placeDoom attrs attrs 1
        withI18n
          $ numberVar "damage" 1
          $ numberVar "horror" 1
          $ labeled' "takeDamageAndHorror"
          $ assignDamageAndHorror iid (attrs.ability 2) 1 1
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleMistyClearing <$> liftRunMessage msg attrs
