module Arkham.Asset.Assets.Bandages (bandages) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Bandages = Bandages AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bandages :: AssetCard Bandages
bandages = assetWith Bandages Cards.bandages discardWhenNoUses

instance HasAbilities Bandages where
  getAbilities (Bandages a) =
    [ controlled_ a 1
        $ triggered
          ( oneOf
              [ AssetDealtDamage
                  #after
                  AnySource
                  (HealableAsset (toSource a) #damage $ AssetAt YourLocation <> #ally)
              , InvestigatorTakeDamage
                  #after
                  (HealableInvestigator (toSource a) #damage $ colocatedWithMatch You)
                  AnySource
              ]
          )
          (assetUseCost a Supply 1)
    ]

getHealTargets :: [Window] -> [Target]
getHealTargets = \case
  (windowType -> Window.DealtDamage _ _ target _) : rest -> target : getHealTargets rest
  (windowType -> Window.TakeDamage _ _ target _) : rest -> target : getHealTargets rest
  _ : rest -> getHealTargets rest
  [] -> []

instance RunMessage Bandages where
  runMessage msg a@(Bandages attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getHealTargets -> healTargets) _ -> do
      chooseOrRunOneM iid do
        targets healTargets \target -> healDamage target (attrs.ability 1) 1
      pure a
    _ -> Bandages <$> liftRunMessage msg attrs
