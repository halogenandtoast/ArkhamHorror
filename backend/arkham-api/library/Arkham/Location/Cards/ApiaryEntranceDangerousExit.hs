module Arkham.Location.Cards.ApiaryEntranceDangerousExit (apiaryEntranceDangerousExit) where

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheApiary.Helpers
import Arkham.Spawn
import Arkham.Trait (Trait (Cultist, Stowaway))

newtype ApiaryEntranceDangerousExit = ApiaryEntranceDangerousExit LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apiaryEntranceDangerousExit :: LocationCard ApiaryEntranceDangerousExit
apiaryEntranceDangerousExit = location ApiaryEntranceDangerousExit Cards.apiaryEntranceDangerousExit 4 (Static 0)

instance HasModifiersFor ApiaryEntranceDangerousExit where
  getModifiersFor (ApiaryEntranceDangerousExit attrs) =
    -- "When a Stowaway enemy is drawn, spawn it at Apiary Entrance instead of its
    -- normal spawn location." OverwrittenSpawn replaces the printed spawn only, so
    -- a drawing effect that forces a spawn elsewhere still takes precedence.
    modifySelect attrs (EnemyWithTrait Stowaway) [OverwrittenSpawn (SpawnAt (be attrs))]

instance HasAbilities ApiaryEntranceDangerousExit where
  getAbilities (ApiaryEntranceDangerousExit a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> oneOf [exists (cultistAssetAt a), exists (cultistEnemyAt a)])
          $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
      , scenarioI18n $ withI18nTooltip "apiaryEntranceDangerousExit.resign" $ locationResignAction a
      ]

{- | The pilgrims can be led out either as the Cultist enemies they spawned as, or
as Maria Rivera, the Cultist story asset an investigator takes in the interlude.
-}
cultistAssetAt :: LocationAttrs -> AssetMatcher
cultistAssetAt a = AssetWithTrait Cultist <> AssetAt (be a)

cultistEnemyAt :: LocationAttrs -> EnemyMatcher
cultistEnemyAt a = EnemyWithTrait Cultist <> EnemyAt (be a)

instance RunMessage ApiaryEntranceDangerousExit where
  runMessage msg l@(ApiaryEntranceDangerousExit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Place a Cultist asset or enemy at this location underneath the act."
      -- Putting the card under the act is what takes the entity out of play.
      act <- selectJust AnyAct
      assets <- selectWithField AssetCard $ cultistAssetAt attrs
      enemies <- selectWithField EnemyCard $ cultistEnemyAt attrs
      chooseOneM iid do
        for_ assets \(asset, card) -> targeting asset $ placeUnderneath act (only card)
        for_ enemies \(enemy, card) -> targeting enemy $ placeUnderneath act (only card)
      pure l
    _ -> ApiaryEntranceDangerousExit <$> liftRunMessage msg attrs
