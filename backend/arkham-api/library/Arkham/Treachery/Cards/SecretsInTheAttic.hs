module Arkham.Treachery.Cards.SecretsInTheAttic (secretsInTheAttic) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SecretsInTheAttic = SecretsInTheAttic TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsInTheAttic :: TreacheryCard SecretsInTheAttic
secretsInTheAttic = treachery SecretsInTheAttic Cards.secretsInTheAttic

instance HasModifiersFor SecretsInTheAttic where
  getModifiersFor (SecretsInTheAttic a) =
    modifySelect
      a
      Anyone
      [CannotTriggerAbilityMatching $ AbilityIsFastAbility <> AbilityOnLocation Anywhere]

instance HasAbilities SecretsInTheAttic where
  getAbilities (SecretsInTheAttic a) =
    [limited (MaxPer Cards.secretsInTheAttic PerRound 1) $ mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage SecretsInTheAttic where
  runMessage msg t@(SecretsInTheAttic attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 1
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> SecretsInTheAttic <$> liftRunMessage msg attrs
