module Arkham.Treachery.Cards.Despoiled (despoiled) where

import Arkham.Ability
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Modifier
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Despoiled = Despoiled TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

despoiled :: TreacheryCard Despoiled
despoiled = treachery Despoiled Cards.despoiled

instance HasAbilities Despoiled where
  getAbilities (Despoiled a) =
    [ restricted a 1 (InYourThreatArea <> DuringTurn You)
        $ forced
        $ SkillTestResult #after You AnySkillTest #success
    , skillTestAbility $ restricted a 2 OnSameLocation actionAbility
    ]

instance RunMessage Despoiled where
  runMessage msg t@(Despoiled attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      turnModifier iid attrs iid (Difficulty 1)
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      whenAny (at_ (locationWithInvestigator iid) <> ExhaustedEnemy <> EnemyWithTrait Witch) do
        skillTestAutomaticallySucceeds (attrs.ability 2) sid
      beginSkillTest sid iid attrs iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Despoiled <$> liftRunMessage msg attrs
