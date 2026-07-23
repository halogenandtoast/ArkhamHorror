module Arkham.Asset.Assets.LaudanumDesperateMeasures (laudanum) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Power, Terror))

newtype LaudanumDesperateMeasures = LaudanumDesperateMeasures AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laudanum :: AssetCard LaudanumDesperateMeasures
laudanum = asset LaudanumDesperateMeasures Cards.laudanum

instance HasAbilities LaudanumDesperateMeasures where
  getAbilities (LaudanumDesperateMeasures a) =
    [ storyControlled
        a
        1
        (exists $ HealableInvestigator (a.ability 1) #horror $ colocatedWithMatch You)
        (actionAbilityWithCost $ assetUseCost a Supply 1)
    , reactionAbility
        a
        2
        Free
        ( DrawCard
            #when
            You
            (CanCancelRevelationEffect You $ basic $ NonWeaknessTreachery <> hasAnyTrait [Power, Terror])
            AnyDeck
        )
        ControlsThis
    ]

instance RunMessage LaudanumDesperateMeasures where
  runMessage msg a@(LaudanumDesperateMeasures attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #horror $ colocatedWith iid
      chooseOrRunOneM iid $ targets investigators \target -> healHorror target (attrs.ability 1) 1
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 (cardDrawn -> card) _ -> do
      cancelRevelation (attrs.ability 2) card
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 2)
      removeFromGame attrs
      pure a
    _ -> LaudanumDesperateMeasures <$> liftRunMessage msg attrs
