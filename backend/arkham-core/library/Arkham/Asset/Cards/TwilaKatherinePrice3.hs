module Arkham.Asset.Cards.TwilaKatherinePrice3 (
  twilaKatherinePrice3,
  TwilaKatherinePrice3 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Trait (Trait (Spell))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TwilaKatherinePrice3 = TwilaKatherinePrice3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

twilaKatherinePrice3 :: AssetCard TwilaKatherinePrice3
twilaKatherinePrice3 = ally TwilaKatherinePrice3 Cards.twilaKatherinePrice3 (1, 2)

instance HasAbilities TwilaKatherinePrice3 where
  getAbilities (TwilaKatherinePrice3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (SpentUses #after You Charge (AssetWithTrait Spell) (atLeast 1)) (exhaust a)
    ]

getSpellAsset :: [Window] -> AssetId
getSpellAsset [] = error "No spell asset found"
getSpellAsset ((windowType -> Window.SpentUses _ aid _ _) : _) = aid
getSpellAsset (_ : ws) = getSpellAsset ws

instance RunMessage TwilaKatherinePrice3 where
  runMessage msg a@(TwilaKatherinePrice3 attrs) = case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getSpellAsset -> aid) _ -> do
      push $ AddUses aid Charge 1
      pure a
    _ -> TwilaKatherinePrice3 <$> runMessage msg attrs
