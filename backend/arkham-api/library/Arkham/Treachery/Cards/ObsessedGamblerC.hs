module Arkham.Treachery.Cards.ObsessedGamblerC (obsessedGamblerC) where

import Arkham.Ability
import Arkham.Helpers.History
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ObsessedGamblerC = ObsessedGamblerC TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsessedGamblerC :: TreacheryCard ObsessedGamblerC
obsessedGamblerC = treachery ObsessedGamblerC Cards.obsessedGamblerC

instance HasAbilities ObsessedGamblerC where
  getAbilities (ObsessedGamblerc a) = [restricted a 1 InYourThreatArea $ forced $ TurnEnds #when You]

instance RunMessage ObsessedGamblerC where
  runMessage msg t@(ObsessedGamblerC attrs) = runQueueT $ case msg of
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
    _ -> ObsessedGamblerC <$> liftRunMessage msg attrs
