module Arkham.Location.Cards.TheBlobThatAteEverythingELSE.Abbatoir (abbatoir) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (ScenarioModifierValue))
import Arkham.Location.CardDefs.TheBlobThatAteEverythingELSE qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Ooze))

newtype Abbatoir = Abbatoir LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abbatoir :: LocationCard Abbatoir
abbatoir = locationWith Abbatoir Cards.abbatoir 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities Abbatoir where
  getAbilities (Abbatoir a) =
    let succeeded = toResultDefault False a.meta
     in extendRevealed a
          $ [ restricted a 1 (Here <> thisExists a LocationWithoutClues) $ actionAbilityWithCost Free
            | not succeeded
            ]

instance RunMessage Abbatoir where
  runMessage msg l@(Abbatoir attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      enemies <- select $ EnemyWithTrait Ooze
      for_ enemies \enemy -> do
        roundModifier (attrs.ability 1) enemy (ScenarioModifierValue "Blob" (toJSON (2 :: Int)))
        moveToward enemy (LocationWithId attrs.id)
        moveToward enemy (LocationWithId attrs.id)
      pure $ Abbatoir $ setMeta True attrs
    _ -> Abbatoir <$> liftRunMessage msg attrs
