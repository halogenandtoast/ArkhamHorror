module Arkham.Act.Cards.ChildrenOfBlood.RiverOfBlood.CornerTheSuspect (cornerTheSuspect) where

import Arkham.Ability
import Arkham.Act.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ChildrenOfBlood.RiverOfBlood.Helpers

newtype CornerTheSuspect = CornerTheSuspect ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cornerTheSuspect :: ActCard CornerTheSuspect
cornerTheSuspect = act (2, A) CornerTheSuspect Cards.cornerTheSuspect Nothing

instance HasAbilities CornerTheSuspect where
  getAbilities (CornerTheSuspect a) =
    [ mkAbility a 1
        $ actionAbilityWithCost
        $ GroupClueCost (PerPlayer 1) (LocationWithEnemy "Julia Stern")
    , mkAbility a 2 $ Objective $ forced $ EnemyLeavesPlay #after "Julia Stern"
    ]

instance RunMessage CornerTheSuspect where
  runMessage msg a@(CornerTheSuspect attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> scenarioI18n do
      inVictory <- selectAny $ VictoryDisplayCardMatch $ basic "Julia Stern"
      if inVictory
        then leadChooseOneM do
          labeled' "r1" $ push R1
          labeled' "r2" $ push R2
        else push $ ResetActDeckToStage 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      scenarioSpecific_ "placeSnare"
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- if the agenda is mid-advance, it must finish resolving before we advance
      let advance = AdvanceAct attrs.id (toSource attrs) #other
      inserted <- insertAfterMatchingMaybe [advance] \case
        AdvanceAgendaDeck {} -> True
        _ -> False
      unless inserted $ push advance
      pure a
    _ -> CornerTheSuspect <$> liftRunMessage msg attrs
