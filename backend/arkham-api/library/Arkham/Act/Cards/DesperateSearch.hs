module Arkham.Act.Cards.DesperateSearch (desperateSearch) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Constants
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Placement
import Arkham.Spawn
import Arkham.Trait (Trait (Lair))

newtype DesperateSearch = DesperateSearch ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desperateSearch :: ActCard DesperateSearch
desperateSearch = act (1, A) DesperateSearch Cards.desperateSearch Nothing

instance HasAbilities DesperateSearch where
  getAbilities = actAbilities1 \a ->
    restricted
      a
      ActAdvancement
      (DuringTurn Anyone <> EachUndefeatedInvestigator (at_ $ LocationWithTrait Lair))
      $ Objective (FastAbility $ GroupClueCost (PerPlayer 3) $ LocationWithTrait Lair)

instance HasModifiersFor DesperateSearch where
  getModifiersFor (DesperateSearch a) =
    modifySelect
      a
      (EnemyWithoutSpawn <> EnemyDrawnFrom Deck.EncounterDeck)
      [ForceSpawn (SpawnPlaced $ OutOfPlay PursuitZone)]

instance RunMessage DesperateSearch where
  runMessage msg a@(DesperateSearch attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> DesperateSearch <$> liftRunMessage msg attrs
