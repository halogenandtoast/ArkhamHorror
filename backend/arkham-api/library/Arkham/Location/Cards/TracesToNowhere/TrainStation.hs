module Arkham.Location.Cards.TracesToNowhere.TrainStation (trainStation) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card.CardType
import Arkham.Helpers.Modifiers (ModifierType (AlternateSuccessfullInvestigation))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Trait (Trait (Syndicate))

newtype TrainStation = TrainStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trainStation :: LocationCard TrainStation
trainStation = location TrainStation Cards.trainStation 2 (PerPlayer 2)

instance HasAbilities TrainStation where
  getAbilities (TrainStation a) =
    extendRevealed
      a
      [ restricted a 1 Here $ freeReaction (SuccessfulInvestigation #when You (be a))
      , hireACab a 2
      ]

instance RunMessage TrainStation where
  runMessage msg l@(TrainStation attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifier
          sid
          (attrs.ability 1)
          (toTarget attrs)
          (AlternateSuccessfullInvestigation $ ProxyTarget (toTarget attrs) (toTarget attrs))
      pure l
    Successful (Action.Investigate, _) iid _ (ProxyTarget (isTarget attrs -> True) _) _ -> do
      findEncounterCard iid attrs (CardWithType EnemyType <> CardWithTrait Syndicate)
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) card -> do
      createEnemyAt_ card attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hireACabEffect iid (attrs.ability 2)
      pure l
    _ -> TrainStation <$> liftRunMessage msg attrs
