module Arkham.Homebrew.AgesUnwound.Locations.Kitchen (kitchen) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Kitchen = Kitchen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kitchen :: LocationCard Kitchen
kitchen = symbolLabel $ location Kitchen Cards.kitchen 2 (PerPlayer 2)

instance HasAbilities Kitchen where
  getAbilities (Kitchen a) =
    extendRevealed
      a
      [ -- "Forced - After you reveal Kitchen: Spawn 2 copies of The Myriad
        -- Gentleman at Kitchen."
        mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , {- "[action] Test [willpower] (3). For each point you succeed by, defeat a
        copy of The Myriad Gentleman at Kitchen. This action does not provoke
        attacks of opportunity." -}
        noAOO $ restricted a 2 Here actionAbility
      ]

instance RunMessage Kitchen where
  runMessage msg l@(Kitchen attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      spawnMyriadCopiesAt lead 2 attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) attrs #willpower (Fixed 3)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 2 -> True) n | n > 0 -> do
      copies <- select $ enemyIs Enemies.theMyriadGentleman_042 <> EnemyAt (be attrs)
      unless (null copies) do
        chooseNM iid (min n (length copies)) $ targets copies \eid -> defeatEnemy eid iid (attrs.ability 2)
      pure l
    _ -> Kitchen <$> liftRunMessage msg attrs
