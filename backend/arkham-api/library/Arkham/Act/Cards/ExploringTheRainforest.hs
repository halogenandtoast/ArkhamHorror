module Arkham.Act.Cards.ExploringTheRainforest (exploringTheRainforest) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Trait

newtype ExploringTheRainforest = ExploringTheRainforest ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exploringTheRainforest :: ActCard ExploringTheRainforest
exploringTheRainforest =
  act (1, A) ExploringTheRainforest Cards.exploringTheRainforest Nothing

instance HasAbilities ExploringTheRainforest where
  getAbilities (ExploringTheRainforest x) =
    [ mkAbility x 1
        $ Objective
        $ triggered (RoundEnds #when)
        $ GroupClueCost (PerPlayer 3) (not_ $ LocationWithTrait Campsite)
    ]

instance RunMessage ExploringTheRainforest where
  runMessage msg a@(ExploringTheRainforest attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      withLocationOf lead (createEnemyAt_ (SetAsideCardMatch $ CardWithTitle "Ichtaca"))
      shuffleEncounterDiscardBackIn
      advanceActDeck attrs
      pure a
    _ -> ExploringTheRainforest <$> liftRunMessage msg attrs
