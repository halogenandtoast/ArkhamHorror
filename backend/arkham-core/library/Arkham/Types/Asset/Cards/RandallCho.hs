module Arkham.Types.Asset.Cards.RandallCho
  ( randallCho
  , RandallCho(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes

newtype RandallCho = RandallCho AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randallCho :: AssetCard RandallCho
randallCho = ally RandallCho Cards.randallCho (1, 3)

instance HasAbilities RandallCho where
  getAbilities (RandallCho x) = [
    restrictedAbility x 1 OwnsThis $ FastAbility $ Free
  ]

instance AssetRunner env => RunMessage env RandallCho where
  runMessage msg (RandallCho attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ chooseOne
        iid
        [ Label
          "Heal 3 damage"
          [HealDamage iid 3]
        , Label
          "Search your deck and discard pile for a Weapon asset, play it (paying its cost), and shuffle your deck"
          -- [ SearchDeckAndDiscardForTraits iid source [Weapon]
          [ SearchDeckForTraits iid source [Weapon]
            $ ShuffleBackIn
            $ PlayFound iid
          ]
        ]
    _ -> RandallCho <$> runMessage msg attrs
