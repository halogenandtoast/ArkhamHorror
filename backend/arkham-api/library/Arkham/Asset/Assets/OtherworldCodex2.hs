module Arkham.Asset.Assets.OtherworldCodex2 (otherworldCodex2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Card
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Trait (Trait (Elite))

newtype OtherworldCodex2 = OtherworldCodex2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldCodex2 :: AssetCard OtherworldCodex2
otherworldCodex2 = asset OtherworldCodex2 Cards.otherworldCodex2

instance HasAbilities OtherworldCodex2 where
  getAbilities (OtherworldCodex2 attrs) =
    [ controlled attrs 1 (youExist can.target.encounterDeck)
        $ actionAbilityWithCost (exhaust attrs <> assetUseCost attrs Secret 1)
    ]

instance RunMessage OtherworldCodex2 where
  runMessage msg a@(OtherworldCodex2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search
        iid
        (attrs.ability 1)
        EncounterDeckTarget
        [fromTopOfDeck 9]
        (basic $ NotCard $ CardWithTrait Elite)
        (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      if null cards
        then withI18n $ prompt_ iid "noCardsFound"
        else do
          let defs = map toCardDef cards
          treacheries <- selectTargets $ mapOneOf treacheryIs defs
          enemies <- selectTargets $ mapOneOf enemyIs defs
          locations <- selectTargets $ mapOneOf locationIs defs
          assets <- selectTargets $ mapOneOf assetIs defs
          -- [ALERT] EncounterDeckTypes
          let allTargets = treacheries <> enemies <> locations <> assets
          if null allTargets
            then withI18n $ prompt_ iid "noCardsFound"
            else chooseTargetM iid allTargets $ toDiscardBy iid (attrs.ability 1)

      pure a
    _ -> OtherworldCodex2 <$> liftRunMessage msg attrs
