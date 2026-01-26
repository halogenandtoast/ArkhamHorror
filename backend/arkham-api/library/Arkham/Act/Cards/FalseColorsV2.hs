module Arkham.Act.Cards.FalseColorsV2 (falseColorsV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Field
import Arkham.Helpers.Location (placementLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenarios.DancingMad.Helpers
import Arkham.Trait (Trait (Coterie))

newtype FalseColorsV2 = FalseColorsV2 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseColorsV2 :: ActCard FalseColorsV2
falseColorsV2 = act (2, A) FalseColorsV2 Cards.falseColorsV2 Nothing

isADesiderio :: EnemyMatcher
isADesiderio = mapOneOf enemyIs [Enemies.desiderioDelgadoAlvarez106, Enemies.desiderioDelgadoAlvarez107]

instance HasModifiersFor FalseColorsV2 where
  getModifiersFor (FalseColorsV2 a) = do
    when (onSide B a) do
      modifySelect
        a
        isADesiderio
        [RemoveConcealed, RemoveKeyword Keyword.Hunter, AddKeyword Keyword.Aloof]

instance HasAbilities FalseColorsV2 where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ parleyAction
          ( ChooseEnemyCostAndMaybeGroupFieldClueCost
              YourLocation
              (EnemyAt YourLocation <> EnemyWithTrait Coterie)
              EnemyRemainingHealth
          )
    , scenarioI18n
        $ withI18nTooltip "falseColorsV2.resign"
        $ mkAbility a 2
        $ ActionAbility [#resign] (ActionCost 1)
    , restricted
        a
        3
        (HasCalculation (SumEnemyFieldCalculation (InPlayEnemy AnyEnemy) EnemyClues) (AtLeast $ PerPlayer 4))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage FalseColorsV2 where
  runMessage msg a@(FalseColorsV2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ p -> do
      let (meid, n) = (chosenEnemyPayment &&& totalCluePayment) p
      for_ meid \eid -> do
        placeTokens (attrs.ability 1) eid #clue n
        removeAllDoom (attrs.ability 1) eid
        automaticallyEvadeEnemy iid eid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resign iid
      pure a
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      turnOverAllConcealed attrs
      do_ msg
      inPlayVersion <- selectJust $ enemyIs Enemies.desiderioDelgadoAlvarez106
      placement <- field EnemyPlacement inPlayVersion
      mlocation <- placementLocation placement
      badVersion <- fetchCard inPlayVersion
      outOfPlayCard <- fetchCard Assets.desiderioDelgadoAlvarez
      let goodVersion = lookupCard Enemies.desiderioDelgadoAlvarez107.cardCode outOfPlayCard.id
      replaceCard goodVersion.id goodVersion
      removeFromGame inPlayVersion

      shuffle [goodVersion, badVersion] >>= \case
        [x, y] -> do
          x1 <- createEnemy x Unplaced
          push $ UpdateEnemy x1 $ Update EnemyPlacement placement
          for_ mlocation $ createEnemy_ y . AtLocation
        _ -> error "Invalid needs both"

      selectWithField EnemyClues EnemyWithAnyClues >>= traverse_ \(enemy, clues) -> do
        removeAllClues attrs enemy
        healDamage enemy attrs clues

      advanceActDeck attrs
      pure a
    Do msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> do
      concealed <- selectMap (.id) (not_ IsDecoy)
      lead <- getLead
      leadChooseOneM $ targets concealed \card -> do
        moveFromShadows lead attrs card
        do_ msg'
      pure a
    _ -> FalseColorsV2 <$> liftRunMessage msg attrs
