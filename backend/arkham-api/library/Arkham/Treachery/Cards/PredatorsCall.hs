module Arkham.Treachery.Cards.PredatorsCall (predatorsCall) where

import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Trait (Trait (Mutated))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PredatorsCall = PredatorsCall TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

predatorsCall :: TreacheryCard PredatorsCall
predatorsCall = treachery PredatorsCall Cards.predatorsCall

instance HasModifiersFor PredatorsCall where
  getModifiersFor (PredatorsCall a) =
    for_ a.attached.enemy \enemy ->
      modified_ a enemy [EnemyFight 1, EnemyEvade 1, DamageDealt 1, HorrorDealt 1]

instance RunMessage PredatorsCall where
  runMessage msg t@(PredatorsCall attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      greens <- select $ NearestEnemyTo iid $ factionEnemy GreenFaction
      if null greens
        then gainSurge attrs
        else chooseOrRunOneM iid $ targets greens \enemy -> do
          attachTreachery attrs enemy
          whenM (enemy <=~> EnemyWithTrait Mutated) $ placeMutations attrs enemy 1
      pure t
    _ -> PredatorsCall <$> liftRunMessage msg attrs
