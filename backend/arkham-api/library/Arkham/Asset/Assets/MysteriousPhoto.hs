module Arkham.Asset.Assets.MysteriousPhoto (mysteriousPhoto) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.RedTideRising.Helpers
import Arkham.Trait (Trait (Suspect))

newtype MysteriousPhoto = MysteriousPhoto AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousPhoto :: AssetCard MysteriousPhoto
mysteriousPhoto = asset MysteriousPhoto Cards.mysteriousPhoto

cluelessSuspectHere :: Criterion
cluelessSuspectHere =
  exists $ EnemyWithTrait Suspect <> EnemyWithClues (EqualTo (Static 0)) <> EnemyAt YourLocation

instance HasAbilities MysteriousPhoto where
  getAbilities (MysteriousPhoto a) =
    [ restricted a 1 ControlsThis $ FastAbility (exhaust a)
    , restricted a 2 (ControlsThis <> cluelessSuspectHere <> exists (AgendaWithStep 1))
        $ FastAbility (GroupClueCost (PerPlayer 1) YourLocation)
    , restricted a 3 (ControlsThis <> cluelessSuspectHere <> exists (AgendaWithStep 2))
        $ FastAbility (GroupClueCost (PerPlayer 2) YourLocation)
    , mkAbility a 4 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage MysteriousPhoto where
  runMessage msg a@(MysteriousPhoto attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      abilities <-
        map (`applyAbilityModifiers` [IgnoreActionCost])
          <$> select
            ( PerformableAbilityBy (InvestigatorWithId iid) [IgnoreActionCost]
                <> #parley
                <> AbilityOnEnemy (EnemyWithTrait Suspect)
            )
      chooseOneM iid $ scenarioI18n do
        labeled' "flipMysteriousPhoto" $ flipOverBy iid (attrs.ability 1) attrs
        for_ abilities \ab -> abilityLabeled iid ab nothing
      pure a
    UseThisAbility iid (isSource attrs -> True) n | n == 2 || n == 3 -> do
      suspects <-
        select $ EnemyWithTrait Suspect <> EnemyWithClues (EqualTo (Static 0)) <> enemyAtLocationWith iid
      chooseOrRunOneM iid $ targets suspects \suspect -> addToVictory iid suspect
      pure a
    UseCardAbility _ (isSource attrs -> True) 4 ws _ -> do
      whenM (selectAny wendyAdams) $ don'tRemove attrs ws
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.mysteriousPhotoBack
      pure a
    _ -> MysteriousPhoto <$> liftRunMessage msg attrs
