module Arkham.Treachery.Cards.DeepOneAssault (deepOneAssault, DeepOneAssault (..)) where

import Arkham.Card
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (DeepOne))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeepOneAssault = DeepOneAssault TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneAssault :: TreacheryCard DeepOneAssault
deepOneAssault = treachery DeepOneAssault Cards.deepOneAssault

instance RunMessage DeepOneAssault where
  runMessage msg t@(DeepOneAssault attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      engaged <- select $ enemyEngagedWith iid <> withTrait DeepOne <> at_ (locationWithInvestigator iid)
      for_ engaged $ \enemyId -> disengageEnemy iid enemyId
      doStep 1 msg
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      enemiesToEngage <-
        select
          $ enemy_
          $ oneOf
            [ at_ (locationWithInvestigator iid)
            , at_ (ConnectedFrom $ locationWithInvestigator iid)
                <> EnemyCanEnter (locationWithInvestigator iid)
            ]
          <> withTrait DeepOne
      chooseOneAtATimeM iid do
        for_ enemiesToEngage $ \enemy ->
          targeting enemy $ enemyEngageInvestigator enemy iid
      when (null enemiesToEngage) $ findEncounterCard iid attrs $ card_ $ #enemy <> withTrait DeepOne
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      createEnemy_ card iid
      pure t
    _ -> DeepOneAssault <$> liftRunMessage msg attrs
