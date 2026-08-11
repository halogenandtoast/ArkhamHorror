module Arkham.Homebrew.DarkMatter.Enemies.MiGoScientist (miGoScientist) where

import Arkham.Ability
import Arkham.Ability.Types qualified as AT
import Arkham.Enemy.Import.Lifted
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MiGoScientist = MiGoScientist EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoScientist :: EnemyCard MiGoScientist
miGoScientist = enemy MiGoScientist Cards.miGoScientist

{- | "Aloof. Patrol (any connecting location)." — both from the card definition. /
"{fast} Exhaust Mi-Go Scientist: Resolve an [action] ability on Mi-Go Scientist's
location, ignoring its '[action]' cost. Investigators at any location may trigger
this ability."
-}
instance HasAbilities MiGoScientist where
  getAbilities (MiGoScientist a) =
    extend1 a $ restricted a 1 NoRestriction $ FastAbility (exhaust a)

-- | Strip the 'ActionCost' from an ability so it can be resolved for free.
setAbilityFree :: Ability -> Ability
setAbilityFree ab = ab {AT.abilityType = go ab.abilityType}
 where
  go = \case
    ActionAbility as ms _ -> ActionAbility as ms Free
    other -> other

instance RunMessage MiGoScientist where
  runMessage msg e@(MiGoScientist attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      abilities <-
        select $ AbilityIsActionAbility <> AbilityOnLocation (locationWithEnemy attrs.id)
      -- "ignoring its '[action]' cost"
      {- "ignoring its '[action]' cost": the action cost is stripped from the
      copy offered here. -}
      chooseOneM iid $ for_ abilities \ab -> abilityLabeled_ iid (setAbilityFree ab)
      pure e
    _ -> MiGoScientist <$> liftRunMessage msg attrs
