module Arkham.Asset.Cards.SplitTheAngleIreOfTheVoid (
  splitTheAngleIreOfTheVoid,
  SplitTheAngleIreOfTheVoid (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability

newtype SplitTheAngleIreOfTheVoid = SplitTheAngleIreOfTheVoid AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

splitTheAngleIreOfTheVoid :: AssetCard SplitTheAngleIreOfTheVoid
splitTheAngleIreOfTheVoid = asset SplitTheAngleIreOfTheVoid Cards.splitTheAngleIreOfTheVoid

instance HasAbilities SplitTheAngleIreOfTheVoid where
  getAbilities (SplitTheAngleIreOfTheVoid x) =
    [ controlledAbility x 1 (youExist $ can.reveal.cards <> can.target.encounterDeck) actionAbility
    , controlledAbility x 2 (youExist $ can.manipulate.deck <> can.target.encounterDeck)
        $ FastAbility (exhaust x <> DiscardTopOfDeckCost 1)
    ]

instance RunMessage SplitTheAngleIreOfTheVoid where
  runMessage msg a@(SplitTheAngleIreOfTheVoid attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      revealing iid (attrs.ability 1) EncounterDeckTarget (FromTopOfDeck 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ DiscardTopOfEncounterDeck iid 1 (attrs.ability 2) Nothing
      pure a
    _ -> SplitTheAngleIreOfTheVoid <$> lift (runMessage msg attrs)
