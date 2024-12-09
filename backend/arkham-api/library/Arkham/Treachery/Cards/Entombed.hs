module Arkham.Treachery.Cards.Entombed (entombed, Entombed (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Metadata = Metadata {difficultyReduction :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Entombed = Entombed (TreacheryAttrs `With` Metadata)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entombed :: TreacheryCard Entombed
entombed = treachery (Entombed . (`With` Metadata 0)) Cards.entombed

instance HasModifiersFor Entombed where
  getModifiersFor (Entombed (attrs `With` _)) =
    inThreatAreaGets attrs [CannotMove, CannotDisengageEnemies]

instance HasAbilities Entombed where
  getAbilities (Entombed (a `With` _)) = [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage Entombed where
  runMessage msg t@(Entombed (attrs `With` metadata)) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let
        difficulty = max 0 (4 - difficultyReduction metadata)
        testChoice sType =
          SkillLabel
            sType
            [Msg.beginSkillTest sid iid (attrs.ability 1) attrs sType (Fixed difficulty)]
      chooseOne iid [testChoice #agility, testChoice #combat]
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      pure $ Entombed $ attrs `With` Metadata (difficultyReduction metadata + 1)
    EndRound -> pure $ Entombed $ attrs `With` Metadata 0
    _ -> Entombed . (`with` metadata) <$> liftRunMessage msg attrs
