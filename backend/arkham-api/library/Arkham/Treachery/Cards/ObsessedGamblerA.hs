module Arkham.Treachery.Cards.ObsessedGamblerA (obsessedGamblerA) where

import Arkham.Ability
import Arkham.Helpers.History
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ObsessedGamblerA = ObsessedGamblerA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsessedGamblerA :: TreacheryCard ObsessedGamblerA
obsessedGamblerA = treachery ObsessedGamblerA Cards.obsessedGamblerA

instance HasAbilities ObsessedGamblerA where
  getAbilities (ObsessedGamblerA a) = [restricted a 1 InYourThreatArea $ forced $ TurnEnds #when You]

instance RunMessage ObsessedGamblerA where
  runMessage msg t@(ObsessedGamblerA attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      whenNone
        ( treacheryInThreatAreaOf iid
            <> mapOneOf treacheryIs [Cards.obsessedGamblerA, Cards.obsessedGamblerB, Cards.obsessedGamblerC]
        )
        do
          placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- getHistoryField TurnHistory iid HistoryResourcesGained
      when (n == 0) $ assignHorror iid (attrs.ability 1) 1
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> ObsessedGamblerA <$> liftRunMessage msg attrs
