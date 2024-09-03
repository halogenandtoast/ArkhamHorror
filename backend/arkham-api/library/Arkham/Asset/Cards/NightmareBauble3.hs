module Arkham.Asset.Cards.NightmareBauble3 (
  nightmareBauble3,
  NightmareBauble3 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Skill.Cards qualified as Skills
import Arkham.Window qualified as Window

newtype NightmareBauble3 = NightmareBauble3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightmareBauble3 :: AssetCard NightmareBauble3
nightmareBauble3 = asset NightmareBauble3 Cards.nightmareBauble3

instance HasAbilities NightmareBauble3 where
  getAbilities (NightmareBauble3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (RevealChaosToken #when You #autofail)
          (ShuffleAttachedCardIntoDeckCost (toTarget a) (cardIs Skills.dreamParasite))
    ]

instance RunMessage NightmareBauble3 where
  runMessage msg a@(NightmareBauble3 attrs) = case msg of
    InvestigatorPlayAsset iid aid | aid == assetId attrs -> do
      bonded <- take 3 <$> searchBonded iid Skills.dreamParasite
      push $ PlaceUnderneath (toTarget attrs) bonded
      NightmareBauble3 <$> runMessage msg attrs
    RemoveFromPlay (isSource attrs -> True) -> do
      for_ (assetCardsUnderneath attrs) \card -> do
        when (card `cardMatch` cardIs Skills.dreamParasite)
          $ for_ (toCardOwner card) \owner ->
            push $ PlaceInBonded owner card
      NightmareBauble3 <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 (Window.revealedChaosTokens -> [token]) _ -> do
      cancelChaosToken token
      pure a
    _ -> NightmareBauble3 <$> runMessage msg attrs
