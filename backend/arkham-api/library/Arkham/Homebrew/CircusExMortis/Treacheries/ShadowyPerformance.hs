module Arkham.Homebrew.CircusExMortis.Treacheries.ShadowyPerformance (shadowyPerformance) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMapM)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokens)
import Arkham.Matcher
import Arkham.SkillType (allSkills)
import Arkham.Treachery.Import.Lifted

newtype ShadowyPerformance = ShadowyPerformance TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowyPerformance :: TreacheryCard ShadowyPerformance
shadowyPerformance = treachery ShadowyPerformance Cards.shadowyPerformance

instance HasModifiersFor ShadowyPerformance where
  getModifiersFor (ShadowyPerformance a) = for_ a.attached.location \lid ->
    modifySelectMapM a (investigatorAt lid) \iid -> do
      n <- length <$> getSealedMoonTokens iid
      pure [SkillModifier sk (-n) | n > 0, sk <- allSkills]

instance HasAbilities ShadowyPerformance where
  getAbilities (ShadowyPerformance a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage ShadowyPerformance where
  runMessage msg t@(ShadowyPerformance attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> ShadowyPerformance <$> liftRunMessage msg attrs
