module Arkham.Homebrew.AgainstTheWendigo.Locations.FortMcDonald (fortMcDonald) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (civilizedResign, scenarioI18n)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype FortMcDonald = FortMcDonald LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortMcDonald :: LocationCard FortMcDonald
fortMcDonald = location FortMcDonald Cards.fortMcDonald 3 (PerPlayer 1)

instance HasAbilities FortMcDonald where
  getAbilities (FortMcDonald a) =
    extendRevealed
      a
      [ civilizedResign a
      , skillTestAbility $ restricted a 1 Here parleyAction_
      , playerLimit PerGame
          $ restricted a 2 (Here <> youExist (can.heal.any (a.ability 2)))
          $ actionAbilityWithCost (ResourceCost 2)
      ]

instance RunMessage FortMcDonald where
  runMessage msg l@(FortMcDonald attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let awarded = toResultDefault (0 :: Int) attrs.meta
      cap <- getPlayerCount
      if awarded < cap
        then do
          gainClues iid (attrs.ability 1) 1
          pure $ FortMcDonald $ setMeta (awarded + 1) attrs
        else pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      canDamage <- canHaveDamageHealed (attrs.ability 2) iid
      canHorror <- canHaveHorrorHealed (attrs.ability 2) iid
      chooseOrRunOneM iid $ scenarioI18n $ scope "fortMcDonald" do
        when canDamage $ labeled "healDamage" $ healDamage iid (attrs.ability 2) 2
        when canHorror $ labeled "healHorror" $ healHorror iid (attrs.ability 2) 2
      pure l
    _ -> FortMcDonald <$> liftRunMessage msg attrs
