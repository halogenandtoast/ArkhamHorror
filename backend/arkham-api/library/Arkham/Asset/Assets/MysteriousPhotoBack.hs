module Arkham.Asset.Assets.MysteriousPhotoBack (mysteriousPhotoBack) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Scenarios.RedTideRising.Helpers
import Arkham.Trait (Trait (Hideout))

newtype MysteriousPhotoBack = MysteriousPhotoBack AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousPhotoBack :: AssetCard MysteriousPhotoBack
mysteriousPhotoBack = asset MysteriousPhotoBack Cards.mysteriousPhotoBack

atCluelessHideout :: [CardDef] -> Criterion
atCluelessHideout defs =
  exists $ YourLocation <> LocationWithTrait Hideout <> LocationWithoutClues <> mapOneOf locationIs defs

instance HasAbilities MysteriousPhotoBack where
  getAbilities (MysteriousPhotoBack a) =
    [ restricted a 1 ControlsThis $ FastAbility (exhaust a)
    , restricted a 2 (ControlsThis <> atCluelessHideout perPlayerOneHideouts <> exists (AgendaWithStep 1))
        $ FastAbility (GroupClueCost (PerPlayer 1) YourLocation)
    , restricted a 3 (ControlsThis <> atCluelessHideout perPlayerTwoHideouts <> exists (AgendaWithStep 1))
        $ FastAbility (GroupClueCost (PerPlayer 2) YourLocation)
    , restricted a 4 (ControlsThis <> atCluelessHideout perPlayerOneHideouts <> exists (AgendaWithStep 2))
        $ FastAbility (GroupClueCost (PerPlayer 2) YourLocation)
    , restricted a 5 (ControlsThis <> atCluelessHideout perPlayerTwoHideouts <> exists (AgendaWithStep 2))
        $ FastAbility (GroupClueCost (PerPlayer 3) YourLocation)
    , mkAbility a 6 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage MysteriousPhotoBack where
  runMessage msg a@(MysteriousPhotoBack attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      abilities <-
        map (`applyAbilityModifiers` [IgnoreActionCost])
          <$> select
            ( #investigate
                <> AbilityOnLocation (locationWithInvestigator iid <> LocationWithTrait Hideout)
                <> PerformableAbilityBy (InvestigatorWithId iid) [IgnoreActionCost]
            )
      chooseOneM iid $ scenarioI18n do
        labeled' "flipMysteriousPhoto" $ flipOverBy iid (attrs.ability 1) attrs
        for_ abilities \ab -> abilityLabeled iid ab nothing
      pure a
    UseThisAbility iid (isSource attrs -> True) n | n >= 2 && n <= 5 -> do
      withLocationOf iid \lid -> do
        investigators <- select $ investigatorAt lid
        enemies <- select $ enemyAt lid
        connecting <- select $ ConnectedFrom NotForMovement (LocationWithId lid)
        for_ investigators \investigator ->
          chooseOrRunOneM investigator $ targets connecting $ moveTo (attrs.ability n) investigator
        lead <- getLead
        for_ enemies \enemy ->
          chooseOrRunOneM lead $ targets connecting \dest -> push $ EnemyMove enemy dest
        addToVictory iid lid
        push $ RemoveLocation lid
      pure a
    UseCardAbility _ (isSource attrs -> True) 6 ws _ -> do
      whenM (selectAny wendyAdams) $ don'tRemove attrs ws
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.mysteriousPhoto
      pure a
    _ -> MysteriousPhotoBack <$> liftRunMessage msg attrs
