module Arkham.Treachery.Cards.ChildrenOfBlood.SanguineSecrets.MorbidRituals (morbidRituals) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.CardDefs.ChildrenOfBlood.SanguineSecrets qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MorbidRituals = MorbidRituals TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

morbidRituals :: TreacheryCard MorbidRituals
morbidRituals = treachery MorbidRituals Cards.morbidRituals

instance RunMessage MorbidRituals where
  runMessage msg t@(MorbidRituals attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- selectCount $ SealedOnInvestigator (InvestigatorWithId iid) #blood
      cultists <-
        if n == 0
          then pure []
          else select $ NearestEnemyToFallback iid $ EnemyWithTrait Cultist <> CanPlaceDoomOnEnemy
      if null cultists
        then selectOne (OnlyInBag #blood) >>= traverse_ (sealChaosToken iid iid)
        else chooseTargetM iid cultists $ placeDoomOn attrs n
      pure t
    _ -> MorbidRituals <$> liftRunMessage msg attrs
