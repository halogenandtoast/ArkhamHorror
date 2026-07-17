module Arkham.Act.Cards.ACircleUnbroken (aCircleUnbroken) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype ACircleUnbroken = ACircleUnbroken ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor ACircleUnbroken where
  getModifiersFor (ACircleUnbroken a) = do
    modifySelectMaybe a (LocationIsInFrontOf Anyone) \lid -> do
      iid <- MaybeT $ field LocationInFrontOf lid
      pure
        [ ConnectedToWhen (LocationWithId lid)
            $ not_ (LocationWithId lid)
            <> LocationIsInFrontOf (InvestigatorWithId iid)
        ]

aCircleUnbroken :: ActCard ACircleUnbroken
aCircleUnbroken = act (4, A) ACircleUnbroken Cards.aCircleUnbroken Nothing

instance HasAbilities ACircleUnbroken where
  getAbilities (ACircleUnbroken x) =
    [ mkAbility x 1 $ Objective $ forced $ ifEnemyDefeated Enemies.anetteMason
    , onlyOnce
        $ restricted x 2 (exists $ locationIs Locations.witchesCircle <> LocationWithoutClues)
        $ Objective (forced AnyWindow)
    ]

instance RunMessage ACircleUnbroken where
  runMessage msg a@(ACircleUnbroken attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      defeatedAnette <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.anetteMason
      push $ if defeatedAnette then R1 else R2
      pure a
    _ -> ACircleUnbroken <$> liftRunMessage msg attrs
