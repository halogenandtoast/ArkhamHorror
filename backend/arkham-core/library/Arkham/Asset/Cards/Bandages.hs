module Arkham.Asset.Cards.Bandages (bandages, Bandages (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Types (discardWhenNoUses)
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Bandages = Bandages AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bandages :: AssetCard Bandages
bandages = assetWith Bandages Cards.bandages discardWhenNoUses

instance HasAbilities Bandages where
  getAbilities (Bandages a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( oneOf
              [ AssetDealtDamage
                  #after
                  AnySource
                  (HealableAsset (toSource a) #damage $ AssetAt YourLocation <> #ally)
              , InvestigatorTakeDamage
                  #after
                  (HealableInvestigator (toSource a) #damage $ InvestigatorAt YourLocation)
                  AnySource
              ]
          )
          (assetUseCost a Supply 1)
    ]

getHealTarget :: [Window] -> Target
getHealTarget = \case
  (windowType -> Window.DealtDamage _ _ target _) : _ -> target
  _ : rest -> getHealTarget rest
  [] -> error "No target found"

instance RunMessage Bandages where
  runMessage msg a@(Bandages attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getHealTarget -> target) _ -> do
      push $ HealDamage target (attrs.ability 1) 1
      pure a
    _ -> Bandages <$> liftRunMessage msg attrs
