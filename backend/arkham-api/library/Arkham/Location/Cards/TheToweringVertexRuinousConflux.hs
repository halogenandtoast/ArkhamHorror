module Arkham.Location.Cards.TheToweringVertexRuinousConflux (theToweringVertexRuinousConflux) where

import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers, modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement

newtype TheToweringVertexRuinousConflux = TheToweringVertexRuinousConflux LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theToweringVertexRuinousConflux :: LocationCard TheToweringVertexRuinousConflux
theToweringVertexRuinousConflux =
  location TheToweringVertexRuinousConflux Cards.theToweringVertexRuinousConflux 6 (Static 0)

concealedPositions :: [Pos]
concealedPositions = [Pos (-1) 2, Pos 0 3, Pos 1 2]

instance HasModifiersFor TheToweringVertexRuinousConflux where
  getModifiersFor (TheToweringVertexRuinousConflux a) = do
    when a.unrevealed do
      mods <- getModifiers a
      unless (ScenarioModifier "canEnter" `elem` mods) do
        modifySelf a [Blocked]
    when a.revealed do
      modifySelect
        a
        (ConcealedCardOneOf $ map (ConcealedCardWithPlacement . InPosition) concealedPositions)
        [ScenarioModifier "doNotRemove"]

instance HasAbilities TheToweringVertexRuinousConflux where
  getAbilities (TheToweringVertexRuinousConflux a) =
    extendRevealed a []

instance RunMessage TheToweringVertexRuinousConflux where
  runMessage msg l@(TheToweringVertexRuinousConflux attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[decoy]" v -> do
      let (iid, c) :: (InvestigatorId, ConcealedCard) = toResult v
      when (maybe False (`elem` concealedPositions) c.position) do
        do_ $ PlaceConcealedCard iid c.id c.placement
        doStep1 0 $ flipOver iid c
      pure l
    ScenarioSpecific "exposed[MimeticNemesis]" v -> do
      let (iid, c) :: (InvestigatorId, ConcealedCard) = toResult v
      mimeticNemesis <- selectJust $ enemyIs Enemies.mimeticNemesisInfiltratorOfRealities
      nonAttackEnemyDamage (Just iid) attrs 3 mimeticNemesis
      do_ $ PlaceConcealedCard iid c.id c.placement
      scenarioSpecific_ "shuffleAllConcealed"
      pure l
    _ -> TheToweringVertexRuinousConflux <$> liftRunMessage msg attrs
